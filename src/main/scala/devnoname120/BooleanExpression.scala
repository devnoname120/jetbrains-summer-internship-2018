package devnoname120

import play.api.libs.json._

sealed trait BooleanExpression
case object True extends BooleanExpression
case object False extends BooleanExpression
case class Variable(symbol: String) extends BooleanExpression
case class Not(e: BooleanExpression) extends BooleanExpression
case class Or(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression
case class And(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression

trait BooleanExpressionJsonFormat {
  // Only keep the class name in the JSON type field
  implicit val cfg = JsonConfiguration(
    typeNaming = JsonNaming { fullName =>
      Unit
      fullName.split('.').last
    }
  )

  implicit val trueFormat = Json.format[True.type]
  implicit val falseFormat = Json.format[False.type]
  implicit val variableFormat = Json.format[Variable]
  implicit lazy val notFormat = Json.format[Not]
  implicit lazy val orFormat = Json.format[Or]
  implicit lazy val andFormat = Json.format[And]
  implicit val booleanExpresssionFormat: OFormat[BooleanExpression] = Json.format[BooleanExpression]
}

object BooleanExpression extends BooleanExpressionJsonFormat {
  def fromJSON(booleanExpression: String) : BooleanExpression = {
    Json.parse(booleanExpression).as[BooleanExpression]
  }
  def toJSON(booleanExpression: BooleanExpression) : String = {
    Json.toJson(booleanExpression).toString
  }
}

