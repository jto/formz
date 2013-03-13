package form

import scalaz._
import Scalaz._

object Api {

  type Mapping[Err, From, To] = (From => ValidationNEL[Err, To])
  type Constraint[T] = Mapping[String, T, T]
  type VA[Key, To] = Validation[NonEmptyList[(Key, NonEmptyList[String])], To]

  object Constraints {

    import scala.util.matching.Regex

    def validateWith[From](msg: String)(pred: From => Boolean): Constraint[From] =
      v => validation(!pred(v) either nel(msg) or v)

    def noConstraint[From]: Constraint[From] = _.success
    def isInt = validateWith("validation.int"){(_: String).matches("-?[0-9]+")}
    def min(m: Int) = validateWith("validation.min"){(_: Int) > m}
    def max(m: Int) = validateWith("validation.max"){(_: Int) < m}
    def positive = validateWith("validation.positive"){(_: Int) >= 0}
    def notEmpty[A] = validateWith("validation.notempty"){!(_:Seq[A]).isEmpty}
    def notEmptyText = validateWith("validation.notemptytext"){!(_: String).isEmpty}
    // XXX: it's probably possible tu use Seq[Char] instead of String
    def minLength(l: Int) = validateWith("validation.minLength"){(_: String).size >= l}
    def maxLength(l: Int) = validateWith("validation.maxLength"){(_: String).size < l}
    def pattern(regex: Regex) = validateWith("validation.pattern"){regex.unapplySeq(_: String).isDefined}
    def email = pattern("""\b[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\b""".r)(_: String).fail.map(_ => nel("validation.email")).validation

    def same[Key, T:Equal](key: Key)(t: (T, T)): VA[Key, T] =
      validation(!(t._1 === t._2) either nel(key -> nel("validation.eq")) or t._1)

  }

  implicit def mappingSemigroup[Err, From, To]: Semigroup[Mapping[Err, From, To]] = semigroup { (m1, m2) =>
    (m1 <**> m2){ _ *> _ }
  }

  def validate[Key, From, To](f: Key => ValidationNEL[String, From])(m: Mapping[String, From, To])(n: Key, v: Constraint[To]): Validation[NonEmptyList[(Key, NonEmptyList[String])], To] = {
    import Validation.Monad._
    (f(n) >>= m >>= v).fail.map(err => nel(n -> err)).validation
  }

  object MapValidation {
    import Constraints._

    type M = Map[String, Seq[String]]

    def fromMap(data: M) = (name: String) =>
      data.get(name).map(_.head).toSuccess("validation.required").liftFailNel

    // TODO: The general concept of Path (like in JsPath) can be generalized to any tree-ish structure.
    // That includes Json, Files, nested Maps, etc.
    def withPrefix[To](prefix: String, v: M => Validation[NonEmptyList[(String, NonEmptyList[String])], To]) = { (data: M) =>
      val  p = s"""$prefix."""
      val sub = data.filterKeys(_.startsWith(p)).map { case (k, v) =>
        k.substring(p.size) -> v
      }
      v(sub)
    }

    def text(name: String, c: Constraint[String] = noConstraint) = (data: M) =>
      validate(fromMap(data))(_.success)(name, c)

    def int(name: String, c: Constraint[Int] = noConstraint) = (data: M) =>
      validate(fromMap(data)) { x: String => isInt(x).map(Integer.parseInt)}(name, c)
  }

  object JsonValidation {
    import Constraints._
    import play.api.libs.json._

    implicit def jspathEq = equalA[JsPath]
    implicit def jspathShow = showA[JsPath]

    def fromJson(data: JsValue) = (path: JsPath) =>
      path(data).headOption.toSuccess("validation.required").liftFailNel

    def withPrefix[To](prefix: JsPath, v: JsValue => Validation[NonEmptyList[(JsPath, NonEmptyList[String])], To]) =
      (data: JsValue) => v(prefix(data).head)

    def text(path: JsPath, c: Constraint[String] = noConstraint) = (data: JsValue) =>
      validate(fromJson(data)){
        case JsString(s) => s.successNel
        case j => "validation.string".failNel
      }(path, c)

    def int(path: JsPath, c: Constraint[Int] = noConstraint) = (data: JsValue) =>
      validate(fromJson(data)){
        case JsNumber(n) if (n.scale <= 0) => n.intValue.successNel
        case j => "validation.int".failNel
      }(path, c)
  }

}


object Examples {

  import Api._
  import Constraints._

  val age = min(18) |+| max(120) |+| positive
  val name = notEmptyText |+| minLength(3)

  def testConstraints = {
    age(27) assert_=== 27.success
    age(17) assert_=== "validation.min".failNel[Int]
    age(160) assert_=== "validation.max".failNel[Int]
    age(-1) assert_=== nel("validation.min", "validation.positive").fail[Int]

    name("toto") assert_=== "toto".success
    name("") assert_=== nel("validation.notemptytext", "validation.minLength").fail[String]
    name("ss") assert_=== "validation.minLength".failNel[String]

    "Success!"
  }

  def withState = {
    import MapValidation._

    type V[To] = ValidationNEL[String, To]

    // has to be implemented for each source
    def find(name: String) = (data: M) => fromMap(data)(name)
    def int = isInt(_: String).map(Integer.parseInt)

    val mock = Map(
      "firstname" -> Seq("Julien"),
      "lastname" -> Seq("Tournay"),
      "age" -> Seq("27"))

    import Validation.Monad._

    def path(p: String) = kleisli[V, M, String](find(p))

    val v = state{ key: String => (key, path(key) >=> int >=> age) }
    val validated = v("age").map(_(mock))

    //val userValidation = for {
    //  fn <- path("firstname") >=> name;
    //  ln <- path("lastname") >=> name;
    //  a  <- path("age") >=> int >=> age
    //} yield (fn |@| ln |@| a)
    //
    //validate[JsValue](
    //  path("firstname") is name,
    //  path("lastname") is name,
    //  path("age") is int is age)(json)

    println(validated)
    "Success!"
  }

  def validateMap = {
    import MapValidation._

    val mock = Map(
      "firstname" -> Seq("Julien"),
      "lastname" -> Seq("Tournay"),
      "age" -> Seq("27"))

    val userValidation = for {
      fn <-  text("firstname", name);
      ln <-  text("lastname", name);
      a  <-  int("age", age)
    } yield (fn |@| ln |@| a)

    val user = userValidation(mock).tupled

    user assert_=== ("Julien", "Tournay", 27).success[NonEmptyList[(String, NonEmptyList[String])]]

    "Success!"
  }

  def validateJson = {
    import play.api.libs.json._
    import JsonValidation._

    val mock = Json.obj(
      "firstname" -> "Julien",
      "lastname" -> "Tournay",
      "age" -> 27)

    val userValidation = for {
      fn <-  text(__ \ "firstname", name);
      ln <-  text(__ \ "lastname", name);
      a  <-  int(__ \ "age", age)
    } yield (fn |@| ln |@| a)

    val user = userValidation(mock).tupled

    user assert_=== ("Julien", "Tournay", 27).success[NonEmptyList[(JsPath, NonEmptyList[String])]]

    "Success!"
  }


  def validateComplexMap = {

    import MapValidation._

    val mock = Map(
      "firstname" -> Seq("Julien"),
      "lastname" -> Seq("Tournay"),
      "age" -> Seq("27"),
      "informations.label" -> Seq("work"),
      "informations.email" -> Seq("jto@zenexity.com"),
      "informations.phones" -> Seq("1234567890", "0987654321"))

    val infoValidation = for {
      l <-  text("label");
      e <-  text("email", email);
      p  <- text("phones", pattern("""[0-9.+]+""".r))
    } yield (l |@| e |@| p).tupled

    val userValidation = for {
      f <- text("firstname", name);
      l <- text("lastname", name);
      a <- int("age", age);
      i <- withPrefix("informations", infoValidation)
    } yield (f |@| l |@| a |@| i)

    val passwordValidation = for {
      p <- text("password");
      c <- text("confirm")
    } yield {
      import Validation.Monad._
      (p <|*|> c) >>= same("pass")
    }

    val user = userValidation(mock).tupled
    user assert_=== ("Julien", "Tournay", 27, ("work", "jto@zenexity.com", "1234567890")).success[NonEmptyList[(String, NonEmptyList[String])]]

    val pass = Map(
      "password" -> Seq("secret"),
      "confirm" -> Seq("secret"))

    val password = passwordValidation(pass)
    password assert_=== "secret".success[NonEmptyList[(String, NonEmptyList[String])]]

    "Success!"
  }

  def validateComplexJson = {

    import play.api.libs.json._
    import JsonValidation._

    val mock = Json.obj(
      "firstname" -> "Julien",
      "lastname" -> "Tournay",
      "age" -> 27,
      "informations" -> Json.obj(
        "label" -> "work",
        "email" -> "jto@zenexity.com",
        "phones" -> "1234567890"))

    // TODO: Something like this would be super nice. (no need to know the data source type anymore, exact same validation would work for Map, Json, anything)
    // path(__ \ "email") >=> text >=> email
    val infoValidation = for {
      l <-  text(__ \ "label");
      e <-  text(__ \ "email", email);
      p  <- text(__ \ "phones", pattern("""[0-9.+]+""".r))
    } yield (l |@| e |@| p).tupled

    val userValidation = for {
      f <- text(__ \ "firstname", name);
      l <- text(__ \ "lastname", name);
      a <- int(__ \ "age", age);
      i <- withPrefix(__ \ "informations", infoValidation)
    } yield (f |@| l |@| a |@| i)

    val passwordValidation = for {
      p <- text(__ \ "password");
      c <- text(__ \ "confirm")
    } yield {
      import Validation.Monad._
      (p <|*|> c) >>= same(__ \ "pass")
    }

    val user = userValidation(mock).tupled
    user assert_=== ("Julien", "Tournay", 27, ("work", "jto@zenexity.com", "1234567890")).success[NonEmptyList[(JsPath, NonEmptyList[String])]]

    val pass = Json.obj(
      "password" -> "secret",
      "confirm" -> "secret")

    val password = passwordValidation(pass)
    password assert_=== "secret".success[NonEmptyList[(JsPath, NonEmptyList[String])]]

    "Success!"
  }
}