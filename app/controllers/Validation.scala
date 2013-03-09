package controllers

import play.api._
import play.api.mvc._

import form._

object Validation extends Controller {

  def map = Action {
    Ok(Examples.validateMap)
  }

  def json = Action {
    Ok(Examples.validateMap)
  }

  def complex = Action {
    Ok(Examples.validateComplex)
  }

}