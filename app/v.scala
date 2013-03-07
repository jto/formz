package form

import scalaz._
import Scalaz._

object Api {

  type Mapping[Err, From, To] = (From => ValidationNEL[Err, To])
  type Constraint[T] = Mapping[String, T, T]

  object Constraints {

    def validateWith[From](msg: String)(pred: From => Boolean): Constraint[From] =
      v => validation(!pred(v) either nel(msg) or v)

    def isInt = validateWith("validation.int"){(_: String).matches("-?[0-9]+")}
    def min(m: Int) = validateWith("validation.min"){(_: Int) > m}
    def max(m: Int) = validateWith("validation.max"){(_: Int) < m}
    def positive = validateWith("validation.positive"){(_: Int) >= 0}
    def notEmpty[A] = validateWith("validation.notempty"){!(_:Seq[A]).isEmpty}
    def notEmptyText = validateWith("validation.notemptytext"){!(_: String).isEmpty}
    // it's probably possible tu use Seq[Char] instead of String
    def minLength(l: Int) = validateWith("validation.minLength"){(_: String).size >= l}
    def maxLength(l: Int) = validateWith("validation.maxLength"){(_: String).size < l}
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

    def text(name: String, c: Constraint[String]) = (data: M) =>
      validate[String, String, String](fromMap(data))(_.success)(name, c)

    def int(name: String, c: Constraint[Int]) = (data: M) =>
      validate[String, String, Int](fromMap(data)) { x: String => isInt(x).map(Integer.parseInt)}(name, c)
  }

  object JsonValidation {
    import play.api.libs.json._

    implicit def jspathEq = equalA[JsPath]
    implicit def jspathShow = showA[JsPath]

    def fromJson(data: JsValue) = (path: JsPath) =>
      path(data).headOption.toSuccess("validation.required").liftFailNel

    def text(path: JsPath, c: Constraint[String]) = (data: JsValue) =>
      validate[JsPath, JsValue, String](fromJson(data)){
        case JsString(s) => s.successNel
        case j => "validation.string".failNel
      }(path, c)


    def int(path: JsPath, c: Constraint[Int]) = (data: JsValue) =>
      validate[JsPath, JsValue, Int](fromJson(data)){
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

    println("Success!")
  }

  def validateMap = {
    import MapValidation._

    val mock = Map(
      "firstname" -> Seq("Julien"),
      "lastname" -> Seq("Tournay"),
      "age" -> Seq("27"))

    val validateUser =
      text("firstname", name)(mock) |@|
      text("lastname", name)(mock) |@|
      int("age", age)(mock)

    // It could also be written like this (IMO better):
    //  val validateUser =
    //    (text("firstname") >>= name)(mock) |@|
    //    (text("lastname") >>=  name)(mock) |@|
    //    (int("age") >>= age)(mock)
    // But importing Validation.Monad._ causes a scope issue (the implicit the Applicative[Validation] is shadowed by Applicative[MA] which does not accumulate errors)

    val user = validateUser.tupled

    user assert_=== ("Julien", "Tournay", 27).success[NonEmptyList[(String, NonEmptyList[String])]]

    println("Success!")
  }

  def validateJson = {
    import play.api.libs.json._
    import JsonValidation._

    val mock = Json.obj(
      "firstname" -> "Julien",
      "lastname" -> "Tournay",
      "age" -> 27)

    val validateUser =
      text(__ \ "firstname", name)(mock) |@|
      text(__ \ "lastname", name)(mock) |@|
      int(__ \ "age", age)(mock)

    val user = validateUser.tupled

    user assert_=== ("Julien", "Tournay", 27).success[NonEmptyList[(JsPath, NonEmptyList[String])]]

    println("Success!")
  }
}