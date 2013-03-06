package form

import scalaz._
import Scalaz._

object Models {
   case class User(firstname: String, lastname: String, age: Int)
   implicit val userEq = equalA[User]
   implicit val userSh = showA[User]
}

object V {

  type Mapping[From, To] = (From => ValidationNEL[String, To])
  type FormMapping[To] = Mapping[String, To]
  type Constraint[T] = Mapping[T, T]

  object Constraints {

    def validateWith[From](msg: String)(pred: From => Boolean): Constraint[From] =
      v => validation(!pred(v) either nel(msg) or v)

    def isInt = validateWith[String]("validation.int"){_.matches("-?[0-9]+")}
    def min(m: Int) = validateWith[Int]("validation.min"){_ > m}
    def max(m: Int) = validateWith[Int]("validation.max"){_ < m}
    def positive = validateWith[Int]("validation.positive"){_ >= 0}
    def notEmpty[A] = validateWith[Seq[A]]("validation.notempty"){!_.isEmpty}
    def notEmptyText = validateWith[String]("validation.notemptytext"){!_.isEmpty}
    // it's probably possible tu use Seq[Char] instead of String
    def minLength(l: Int) = validateWith[String]("validation.minLength"){_.size >= l}
    def maxLength(l: Int) = validateWith[String]("validation.maxLength"){_.size < l}
  }

  implicit def mappingSemigroup[From, To]: Semigroup[Mapping[From, To]] = semigroup { (m1, m2) =>
    (m1 <**> m2){ _ *> _ }
  }

  // extractor, we could easily write something equivalent for Json by defining
  // `def path(json: JsObject): Mapping[JsPath, String] = path => ...`

  def validate[Key, From, To](f: Key => ValidationNEL[String, From])(m: Mapping[From, To])(n: Key, v: Constraint[To]): Validation[NonEmptyList[(Key, NonEmptyList[String])], To] = {
    import Validation.Monad._
    (f(n) >>= m >>= v).fail.map(err => nel(n -> err)).validation
  }


  object MapValidation {
    import Constraints._

    type M = Map[String, Seq[String]]

    def fromMap(data: M) = (name: String) =>
      data.get(name).map(_.head).toSuccess[String]("validation.required").liftFailNel

    def text(name: String, c: Constraint[String]) = (data: M) =>
      validate[String, String, String](fromMap(data))(_.success)(name, c)

    def int(name: String, c: Constraint[Int]) = (data: M) =>
      validate[String, String, Int](fromMap(data)) { x: String => isInt(x).map(Integer.parseInt)}(name, c)
  }

  /*
   def data(request: play.api.mvc.Request[_]) = {
    (request.body match {
      case body: play.api.mvc.AnyContent if body.asFormUrlEncoded.isDefined => body.asFormUrlEncoded.get
      case body: play.api.mvc.AnyContent if body.asMultipartFormData.isDefined => body.asMultipartFormData.get.asFormUrlEncoded
      // case body: play.api.mvc.AnyContent if body.asJson.isDefined => FormUtils.fromJson(js = body.asJson.get).mapValues(Seq(_))
      case body: Map[_, _] => body.asInstanceOf[Map[String, Seq[String]]]
      case body: play.api.mvc.MultipartFormData[_] => body.asFormUrlEncoded
      // case body: play.api.libs.json.JsValue => FormUtils.fromJson(js = body).mapValues(Seq(_))
      case _ => Map.empty[String, Seq[String]]
    }) ++ request.queryString
  }
  */

  def validateMap = {

    import Constraints._
    import Models._
    import MapValidation._

    val age = min(18) |+| max(120) |+| positive
    val name = notEmptyText |+| minLength(3)

    val mock = Map(
      "firstname" -> Seq("Julien"),
      "lastname" -> Seq("Tournay"),
      "age" -> Seq("27"))

    val validateUser = text("firstname", name)(mock) |@| text("lastname", name)(mock) |@| int("age", age)(mock)

    val user = validateUser(User.apply)

    user assert_=== User("Julien", "Tournay", 27).success[NonEmptyList[(String, NonEmptyList[String])]]

    age(27) assert_=== 27.success
    age(17) assert_=== "validation.min".failNel[Int]
    age(160) assert_=== "validation.max".failNel[Int]
    age(-1) assert_=== nel("validation.min", "validation.positive").fail[Int]

    name("toto") assert_=== "toto".success
    name("") assert_=== nel("validation.notemptytext", "validation.minLength").fail[String]
    name("ss") assert_=== "validation.minLength".failNel[String]

    println("YEAH!")
  }

}