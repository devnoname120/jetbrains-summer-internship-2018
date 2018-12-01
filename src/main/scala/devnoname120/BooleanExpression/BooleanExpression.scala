package devnoname120.BooleanExpression

import play.api.libs.json.JsonConfiguration.Aux
import play.api.libs.json.Reads._
import play.api.libs.json._

sealed trait BooleanExpression
case object True extends BooleanExpression
case object False extends BooleanExpression
case class Variable(symbol: String) extends BooleanExpression {
  require(symbol.nonEmpty)
}
case class Not(e: BooleanExpression) extends BooleanExpression
case class Or(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression
case class And(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression


package object Extensions {
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
    implicit val variableWrites: Writes[Variable] = Json.writes[Variable]
    implicit val variableReads: Reads[Variable] = (JsPath \ "symbol").read[String](minLength[String](1)).map(Variable)
    implicit lazy val notFormat: OFormat[Not] = Json.format[Not]
    implicit lazy val orFormat: OFormat[Or] = Json.format[Or]
    implicit lazy val andFormat: OFormat[And] = Json.format[And]
    implicit val booleanExpressionFormat: OFormat[BooleanExpression] = Json.format[BooleanExpression]
  }

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
  }

  /**
    * Extra methods for a BooleanExpression
    */
  implicit class BooleanExpressionWithExtensions(expr: BooleanExpression) extends BooleanExpressionJsonFormat {
    /**
      * Convert a expr to a JSON string
      *
      * @return the string representation
      */
    def toJSON: String = {
      Json.toJson(expr).toString
    }

    /**
      * Convert a expr to a JSON string readable by a human
      *
      * @return the string representation
      */
    def toPrettyJSON: String = {
      Json.prettyPrint(Json.toJson(expr))
    }

    /**
      * Convert expr into a human-readable boolean algebra expression
      *
      * @param top Is a top expression
      * @return
      */
    def toMathString(top: Boolean = true): String = {
      def concatExpr(e1: BooleanExpression, e2: BooleanExpression, sym: String): String = {
        val left = e1.toMathString(top = false)
        val right = e2.toMathString(top = false)
        val expr = s"$left $sym $right"
        if (!top)
          s"($expr)"
        else
          expr
      }

      expr match {
        case True => "true"
        case False => "false"
        case Variable(symbol) => symbol.replace(' ', '_')
        case Not(e) => "¬" + e.toMathString(top = false)
        case Or(e1, e2) =>
          concatExpr(e1, e2, "∨")
        case And(e1, e2) =>
          concatExpr(e1, e2, "∧")
      }
    }

    /**
      * Convert [[expr]] to [[https://en.wikipedia.org/wiki/Conjunctive_normal_form Conjunctive Normal Form]]
      */
    def toCNF: BooleanExpression = {
      ConjunctiveNormalFormTransformer.transform(expr)
    }

    /**
      * Convert [[expr]] to [[https://en.wikipedia.org/wiki/Disjunctive_normal_form Disjunctive Normal Form]]
      */
    def toDNF: BooleanExpression = {
      DisjunctiveNormalFormTransformer.transform(expr)
    }
  }
}



