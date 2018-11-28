package devnoname120

import play.api.libs.json.JsonConfiguration.Aux
import play.api.libs.json._

sealed trait BooleanExpression
case object True extends BooleanExpression
case object False extends BooleanExpression
case class Variable(symbol: String) extends BooleanExpression
case class Not(e: BooleanExpression) extends BooleanExpression
case class Or(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression
case class And(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression


/**
  * Play JSON Read/Write implicits
  */
trait BooleanExpressionJsonFormat {
  // Only keep the class name in the JSON type field
  implicit val cfg: Aux[Json.MacroOptions] = JsonConfiguration(
    typeNaming = JsonNaming { fullName =>
      Unit
      fullName.split('.').last
    }
  )

  implicit val trueFormat: OFormat[True.type] = Json.format[True.type]
  implicit val falseFormat: OFormat[False.type] = Json.format[False.type]
  implicit val variableFormat: OFormat[Variable] = Json.format[Variable]
  implicit lazy val notFormat: OFormat[Not] = Json.format[Not]
  implicit lazy val orFormat: OFormat[Or] = Json.format[Or]
  implicit lazy val andFormat: OFormat[And] = Json.format[And]
  implicit val booleanExpressionFormat: OFormat[BooleanExpression] = Json.format[BooleanExpression]
}

/**
  * Convert a BooleanExpression from/to JSON
  */
object BooleanExpression extends BooleanExpressionJsonFormat {
  /**
    * Parse a [[BooleanExpression]] from a JSON string
    *
    * @param booleanExpression A JSON string containing a boolean expression
    * @return a [[BooleanExpression]] parsed from the string
    */
  def fromJSON(booleanExpression: String) : BooleanExpression = {
    Json.parse(booleanExpression).as[BooleanExpression]
  }

  /**
    * Convert a [[BooleanExpression]] to a JSON string
    *
    * @param booleanExpression A boolean expression
    * @return the string representation
    */
  def toJSON(booleanExpression: BooleanExpression) : String = {
    Json.toJson(booleanExpression).toString
  }

  /**
    * Convert a [[BooleanExpression]] into a human-readable boolean algebra expression
    * @param top Is a top expression
    * @return
    */
  def toMathString(booleanExpression: BooleanExpression, top: Boolean = true): String = {
    booleanExpression match {
      case True => "true"
      case False => "false"
      case Variable(symbol) => symbol
      case Not(e) => "¬" + BooleanExpression.toMathString(e, top = false)
      case Or(e1, e2) =>
        val expr = BooleanExpression.toMathString(e1,top = false) + "∨" + BooleanExpression.toMathString(e2, top = false)
        if (!top)
          s"($expr)"
        else
          expr
      case And(e1, e2) =>
        val expr = BooleanExpression.toMathString(e1, top = false) + "∧" + BooleanExpression.toMathString(e2, top = false)
        if (!top)
          s"($expr)"
        else
          expr
    }
  }
}

