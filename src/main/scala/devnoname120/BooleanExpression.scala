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
}

