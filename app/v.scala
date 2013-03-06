package form

import scalaz._
import scalaz.{ Scalaz => Z }
import Scalaz._

object V {

  type Mapping[From, To] = (From => ValidationNEL[String, To])
  type FormMapping[To] = Mapping[String, To]
  type Constraint[T] = Mapping[T, T]

  object Constraints {

    def validateWith[From](msg: String)(pred: From => Boolean): Constraint[From] =
      v => validation(!pred(v) either nel(msg) or v)

    def field(data: Map[String, Seq[String]]): Mapping[String, String] = name =>
      data.get(name).map(_.head).toSuccess[String]("validation.required").liftFailNel

    def isInt = validateWith[String]("validation.int"){_.matches("-?[0-9]+")}
    def min(m: Int) = validateWith[Int]("validation.min"){_ > m}
    def max(m: Int) = validateWith[Int]("validation.max"){_ < m}
    def positive = validateWith[Int]("validation.positive"){_ >= 0}
    def notEmpty[A] = validateWith[Seq[A]]("validation.notempty"){!_.isEmpty}
    def notEmptyText = validateWith[String]("validation.notemptytext"){!_.isEmpty}
    // Probably possible tu use Seq[Char] instead of String
    def minLength(l: Int) = validateWith[String]("validation.minLength"){_.size >= l}
    def maxLength(l: Int) = validateWith[String]("validation.maxLength"){_.size < l}

    def text: FormMapping[String] = _.success
    def int(x: String) = isInt(x).map(Integer.parseInt)
  }

  implicit def mappingSemigroup[From, To]: Semigroup[Mapping[From, To]] = semigroup { (m1, m2) =>
    (m1 <**> m2){_ *> _}
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

    case class User(firstname: String, lastname: String, age: Int)

    // Composition using kleisli arrows
    import Validation.Monad._
    type VA[A] = ValidationNEL[String, A]
    val agek = kleisli[VA, String, Int](int) >=> (min(18) |+| max(120) |+| positive)

    // Composition using Monads
    val age = int(_: String) >>= min(18) |+| max(120) |+| positive
    val name = text(_: String) >>= notEmptyText |+| minLength(3)

    val mock = Map(
      "firstname" -> Seq("Julien"),
      "lastname" -> Seq("Tournay"),
      "age" -> Seq("27"))

    val f = field(mock) // should be part of play, working an implicit Request[...]

    // extractor, we could easily write something equivalent for Json by defining
    // `def path(json: JsObject): Mapping[JsPath, String] = path => ...`

    val validateUser =
      (f("firstname") >>= name) |@|
      (f("lastname") >>= name) |@|
      (f("age") >>= age)

    val user = validateUser(User.apply)

    implicit val userEq = equalA[User]
    implicit val userSh = showA[User]

    user assert_=== User("Julien", "Tournay", 27).successNel[String]
    agek("27") assert_=== 27.success
    agek("17") assert_=== "validation.min".failNel[Int]
    agek("160") assert_=== "validation.max".failNel[Int]
    agek("-1") assert_=== nel("validation.min", "validation.positive").fail[Int]

    age("27") assert_=== 27.success
    age("17") assert_=== "validation.min".failNel[Int]
    age("160") assert_=== "validation.max".failNel[Int]
    age("-1") assert_=== nel("validation.min", "validation.positive").fail[Int]

    name("toto") assert_=== "toto".success
    name("") assert_=== nel("validation.notemptytext", "validation.minLength").fail[String]
    name("ss") assert_=== "validation.minLength".failNel[String]

    println("YEAH!")
  }


  /*
  def fromForm = {

    import play.api.data._
    import play.api.data.Forms._
    import play.api.data.validation._

    import controllers.Contacts._

    val form2KO = contactForm.bind(Map("firstname" -> "", "lastname" -> ""))
    val formOK  = contactForm.bind(Map("firstname" -> "julien", "lastname" -> "tournay"))
    val formKO  = contactForm.bind(Map("firstname" -> "julien", "lastname" -> ""))


    // Directly compose Constraint.
    // For now in Play, you can only compose Mapping.
    // A Mapping is Constraint* + key + format + submapping*
    // What you *really* want to compose is always Constraint so it does not make sense to have composition methods on Mapping
    implicit def constraintSemigroup[A]: Semigroup[Constraint[A]] = semigroup{ (c1, c2) =>
      Constraint[A]{ v: A =>
        (c1.apply(v), c2.apply(v)) match {
          case (Valid, Valid)             => Valid
          case (Invalid(e), Valid)        => Invalid(e)
          case (Valid, Invalid(e))        => Invalid(e)
          case (Invalid(e1), Invalid(e2)) => Invalid(e1 |+| e2)
        }
    }}

    val notEmpty = Constraint{ xs: String =>
      if(xs.size > 0) Valid
      else Invalid("should not be empty")
    }

    implicit def formToValidation[T](f: Form[T]): ValidationNEL[FormError, T] =
      Z.validation((f.hasErrors) either f.errors.toList.toNel.get or f.get)

    // Constraints composition
    val c = notEmpty |+| pattern("""[0-9.+]+""".r, error="A valid phone number is required")
    val constraint = c.apply("").toString

    //val userWithInformation = userMapping |+| informationsMapping
  }
  */
}