package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._

import views._

import models._

object Contacts extends Controller {

  /**
   * Contact Form definition.
   */
  val contactForm: Form[Contact] = Form(

    // Defines a mapping that will handle Contact values
    mapping(
      "firstname" -> nonEmptyText,
      "lastname" -> nonEmptyText,
      "company" -> optional(text),

      // Defines a repeated mapping
      "informations" -> seq(
        mapping(
          "label" -> nonEmptyText,
          "email" -> optional(email),
          "phones" -> list(
            text verifying pattern("""[0-9.+]+""".r, error="A valid phone number is required")
          )
        )(ContactInformation.apply)(ContactInformation.unapply)
      )

    )(Contact.apply)(Contact.unapply)
  )

  /**
   * Display an empty form.
   */
  def form = Action {
    Ok(html.contact.form(contactForm));
  }

  /**
   * Display a form pre-filled with an existing Contact.
   */
  def editForm = Action {
    val existingContact = Contact(
      "Fake", "Contact", Some("Fake company"), informations = List(
        ContactInformation(
          "Personal", Some("fakecontact@gmail.com"), List("01.23.45.67.89", "98.76.54.32.10")
        ),
        ContactInformation(
          "Professional", Some("fakecontact@company.com"), List("01.23.45.67.89")
        ),
        ContactInformation(
          "Previous", Some("fakecontact@oldcompany.com"), List()
        )
      )
    )
    Ok(html.contact.form(contactForm.fill(existingContact)))
  }

  /**
   * Handle form submission.
   */
  def submit = Action { implicit request =>
    contactForm.bindFromRequest.fold(
      errors => BadRequest(html.contact.form(errors)),
      contact => Ok(html.contact.summary(contact))
    )
  }

  def test = Action {
    import scalaz._
    import scalaz.{ Scalaz => Z }
    import Scalaz._

    import play.api.data._
    import play.api.data.Forms._
    import play.api.data.validation._

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

    Ok
  }


  def newApi = Action { implicit request =>

    import scalaz._
    import scalaz.{ Scalaz => Z }
    import Scalaz._

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

    type Mapping[From, To] = (From => ValidationNEL[String, To])
    type FormMapping[To] = Mapping[String, To]
    type Constraint[T] = Mapping[T, T]


    implicit def mappingSemigroup[From, To]: Semigroup[Mapping[From, To]] = semigroup { (m1, m2) =>
      (m1 <**> m2){_ *> _}
    }

    def field(name: String): Mapping[Map[String, Seq[String]], String] = data =>
      data.get(name).map(_.head).toSuccess[String]("required").liftFailNel

    def isInt: Constraint[String] = v =>
      validation(!v.matches("-?[0-9]+") either nel("not an Int") or v)
    def min(m: Int): Constraint[Int] = v =>
      validation(!(v > m) either nel("MIN") or v)
    def max(m: Int): Constraint[Int] = v =>
      validation(!(v < m) either nel("MAX") or v)
    def positive: Constraint[Int] = v =>
      validation(!(v >= 0) either nel("positive") or v)
    def notEmpty[A]: Constraint[Seq[A]] = vs =>
      validation(vs.isEmpty either nel("expected an non empty collection") or vs)
    def notEmptyText: Constraint[String] =
      notEmpty[Char](_).map(_.mkString)
    // Probably possible tu use Seq[Char] instead of String
    def minLength[A](l: Int): Constraint[String] = vs =>
      validation(!(vs.size >= l) either nel("min length") or vs)
    def maxLength[A](l: Int): Constraint[String] = vs =>
      validation(!(vs.size < l) either nel("max length") or vs)

    def text: FormMapping[String] = _.success
    def int: FormMapping[Int] = isInt(_).map(Integer.parseInt)



    case class User(firstname: String, lastname: String, age: Int)
    // Composition using kleisli arrows
    import Validation.Monad._
    type VA[A] = ValidationNEL[String, A]
    val agek = kleisli[VA, String, Int](int) >=> (min(18) |+| max(120) |+| positive)


    // Using Monads
    val age = int(_: String) >>= min(18) |+| max(120) |+| positive
    val name = text(_: String) >>= notEmptyText |+| minLength(3)

    val user = (name("julien") |@| name("tournay") |@| age("27")) { User.apply }

    val mock = Map(
      "firstname" -> Seq("Julien"),
      "lastname" -> Seq("Tournay"),
      "age" -> Seq("27"))

    println("== user ==")
    println(user)

    println("== v1 ==")
    println(agek("27"))
    println(agek("17"))
    println(agek("160"))
    println(agek("-1"))

    println("== v2 ==")
    println(age("27"))
    println(age("17"))
    println(age("160"))
    println(age("-1"))

    println("== name2 ==")
    println(name("toto"))
    println(name(""))
    println(name("ss"))

    Ok
  }

}