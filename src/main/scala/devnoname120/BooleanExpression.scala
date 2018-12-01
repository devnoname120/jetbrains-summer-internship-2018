package devnoname120

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
    * A set of methods allowing to convert a boolean expression to a Normal Form
    */
  sealed abstract class NormalFormTransformer {
    /**
      * Transform a [[BooleanExpression]] to a Normal Form
      * @param expr A boolean expression
      * @return A boolean expression in normal form
      */
    def transform(expr: BooleanExpression): BooleanExpression = {
      expr match {
        case True | False | Variable(_) => expr
        case And(e1, e2) => transform(And(e1, e2))
        case Or(e1, e2) => transform(Or(e1, e2))

        case Not(e) =>
          e match {
            case False | True | Variable(_) => expr
            // Double negation
            case Not(e1) => transform(e1)
            // de Morgan's laws
            case And(e1, e2) => transform(Or(Not(e1), Not(e2)))
            case Or(e1, e2) => transform(And(Not(e1), Not(e2)))
          }
      }
    }

    /**
      * Special case: Transform a [[And]] to a Normal Form
      * @param and An [[And]] expression
      * @return A boolean expression in normal form
      */
    def transform(and: And): BooleanExpression
    /**
      * Special case: Transform a [[Or]] to a Normal Form
      * @param or An [[And]] expression
      * @return A boolean expression in normal form
      */
    def transform(or: Or): BooleanExpression

    def cartesianProduct(e1: Seq[BooleanExpression], e2: Seq[BooleanExpression])
    : Seq[(BooleanExpression, BooleanExpression)] = {
      for { x <- e1; y <- e2 } yield (x, y)
    }
  }

  case object ConjunctiveNormalFormTransformer extends NormalFormTransformer {
    /**
      * Flatten a nested And()
      *
      * @param and A nested And()
      * @return A sequence of unwrapped And() clauses
      * @example andFlatten(And(And(True, True), False)) == Seq(True, True, False)
      * @example andFlatten(And(Or(True, True), False)) == Seq(Or(True, True), False)
      */
    def andFlatten[T <: BooleanExpression](and: T): Seq[BooleanExpression] = {
      def flattenRec(expr: BooleanExpression): Seq[BooleanExpression] = expr match {
        case And(e1, e2) => List(e1, e2).flatMap(flattenRec)
        case _ => List(expr)
      }

      flattenRec(and)
    }

    override def transform(and: And): BooleanExpression = And(transform(and.e1), transform(and.e2))

    override def transform(or: Or): BooleanExpression = {
      def flatten(b: BooleanExpression): Seq[BooleanExpression] = b match {
        case a: And => andFlatten(a)
        case b: BooleanExpression => Seq(b)
      }

      val f1 = flatten(transform(or.e1))
      val f2 = flatten(transform(or.e2))

      val orList: Seq[Or] = cartesianProduct(f1, f2).map(Or.tupled)
      val andExpr: BooleanExpression = orList.reduceRight(And)

      andExpr
    }
  }
  case object DisjunctiveNormalFormTransformer extends NormalFormTransformer {
    /**
      * Flatten a nested Or()
      *
      * @param or A nested Or()
      * @return A sequence of unwrapped Or() clauses
      * @example andFlatten(And(And(True, True), False)) == Seq(True, True, False)
      * @example andFlatten(And(Or(True, True), False)) == Seq(Or(True, True), False)
      */
    def orFlatten(or: Or): Seq[BooleanExpression] = {
      def flattenRec(expr: BooleanExpression): Seq[BooleanExpression] = expr match {
        case Or(e1, e2) => List(e1, e2).flatMap(flattenRec)
        case _ => List(expr)
      }

      flattenRec(or)
    }

    override def transform(and: And): BooleanExpression = {
      def flatten(b: BooleanExpression): Seq[BooleanExpression] = b match {
        case a: Or => orFlatten(a)
        case b: BooleanExpression => Seq(b)
      }

      val f1 = flatten(transform(and.e1))
      val f2 = flatten(transform(and.e2))

      val andList: Seq[And] = cartesianProduct(f1, f2).map(And.tupled)
      val orExpr: BooleanExpression = andList.reduceRight(Or)

      orExpr
    }

    override def transform(or: Or): BooleanExpression = Or(transform(or.e1), transform(or.e2))
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



