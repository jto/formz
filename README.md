# Form / Validation API re-design proposal

## Motivations

There's currently a lot of redundancy in Play concerning data validation. We have a validation API for Forms, a validation API for Json, and one more validation API in Anorm (as Anorm validates the resuls returned by the DB).

This is an attempt to solve this issue by providing an validation API working for different data types, and easy to extend.

## Concepts

The basics of it are pretty simple.

Validating data basically means you have to go through 3 steps:

- Extract "pieces" of data from a "blob" (a blob being an Http request, a scala Map, a jdbc ResultSet, a Json object etc.)
- validate formats and convert each "pieces" in Scala types ("17" => 17:Int, JsNumber(17) => 17 ...)
- Apply validation rules (business logic here), for example age must not be < 0.

At first, theses steps seems to be very different, they are in fact specializations of `Input => Either[Errors, Result]`.

Let's take the example of validating an Int contained in a request body

- Extraction is `Request => Either[Errors, String]`
- Format validation is `String => Either[Errors, Int]`
- "Business" validation is `Int => Either[Errors, Int]`

This POC uses `Validation` instead of `Either`, but the general idea is the same

``` scala
type Mapping[Err, From, To] = (From => ValidationNEL[Err, To]) // Steps 1 and 2
type Constraint[T] = Mapping[String, T, T]                     // Step 3
```

There's a strong separation between "Business" logic (`Constraint`) and Extraction / Format logic.
Thanks to that, every predefined `Constraint` can be used on _any_ data source.

Json validation and Map validation are almost identical and are using the same `Constraint`:

```scala
// Map Validation
val userValidation = for {
  fn <-  text("firstname", name);
  ln <-  text("lastname", name);
  a  <-  int("age", age)
} yield (fn |@| ln |@| a)

// Json validation
val userValidation = for {
  fn <-  text(__ \ "firstname", name);
  ln <-  text(__ \ "lastname", name);
  a  <-  int(__ \ "age", age)
} yield (fn |@| ln |@| a)
````

It's also fairly easy to add support for a new data source by writing a bunch of `Mapping`.

## Writing `Constraint`

Since constraints are just pure functions, it's really easy to write one from scratch (and even easier using a provided helper ).

```scala
def min(m: Int) = validateWith("validation.min"){(_: Int) > m}
```

It's also possible to write new `Constraint` by composition (since Constraints are Monoids).
Note that all all validation will be applied, and all errors kept.

```scala
 val age = min(1) |+| max(120)
```

I don't know if a final implementation should rely on scalaz. I think it can, but scalaz typeclasse and operators should be hidden to most users. It's certainly useful to have it internally, and probably to let users play with it if they want to by adding a special import like `import play.api.typeclasses._`

Feedbacks are welcome :)

jto.
