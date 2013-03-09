package form

import scalaz._
import Scalaz._

object Api {

  type Mapping[Err, From, To] = (From => ValidationNEL[Err, To])
  type Constraint[T] = Mapping[String, T, T]

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
    // it's probably possible tu use Seq[Char] instead of String
    def minLength(l: Int) = validateWith("validation.minLength"){(_: String).size >= l}
    def maxLength(l: Int) = validateWith("validation.maxLength"){(_: String).size < l}
    def pattern(regex: Regex) = validateWith("validation.pattern"){regex.unapplySeq(_: String).isDefined}
    def email = pattern("""\b[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\b""".r)(_: String).fail.map(_ => nel("validation.email")).validation
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

    def withPrefix[Key, To](prefix: String, v: M => Validation[NonEmptyList[(Key, NonEmptyList[String])], To]) = { (data: M) =>
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
    import play.api.libs.json._

    implicit def jspathEq = equalA[JsPath]
    implicit def jspathShow = showA[JsPath]

    def fromJson(data: JsValue) = (path: JsPath) =>
      path(data).headOption.toSuccess("validation.required").liftFailNel

    def text(path: JsPath, c: Constraint[String]) = (data: JsValue) =>
      validate(fromJson(data)){
        case JsString(s) => s.successNel
        case j => "validation.string".failNel
      }(path, c)

    def int(path: JsPath, c: Constraint[Int]) = (data: JsValue) =>
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


  def validateComplex = {

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


    type VA[Key, To] = Validation[NonEmptyList[(Key, NonEmptyList[String])], To]

    def same[Key, T:Equal](key: Key)(t: (T, T)): VA[Key, T] =
      validation(!(t._1 === t._2) either nel(key -> nel("validation.eq")) or t._1)

    val passwordValidation = for {
      p <- text("password");
      c <- text("confirm")
    } yield {
      import Validation.Monad._
      (p <|*|> c) >>= same("pass and confirm")
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
}