package devnoname120.BooleanExpression

import Extensions._

object Main extends App {
  val complexExpression = And(
    Or(Variable("X1"), Variable("Y1")),
    And(
      Or(Variable("X2"), Variable("Y2")),
      And(
        Or(Variable("X3"), Variable("Y3")),
        Or(Variable("X4"), Variable("Y4"))
      )
    )
  )

  println(s"Complex expression as math string: ${complexExpression.toMathString()}")
  println()

  println(s"Serialized to JSON: ${complexExpression.toPrettyJSON}")
  println()

  println(s"Converted to DNF form: ${complexExpression.toDNF.toMathString()}")
  println(s"==> Exponential explosion of the formula (worst case)")
  println()

  println(s"Converted to CNF form: ${complexExpression.toCNF.toMathString()}")
  println(s"==> Nothing changed, it was already a CNF")
  println()

  println("Deserialize BooleanExpression from JSON...")
  val expr = BooleanExpression.fromJSON("""{"_type":"And","e1":{"_type":"Or","e1":{"symbol":"X1","_type":"Variable"},"e2":{"symbol":"Y1","_type":"Variable"}},"e2":{"_type":"And","e1":{"_type":"Or","e1":{"symbol":"X2","_type":"Variable"},"e2":{"symbol":"Y2","_type":"Variable"}},"e2":{"_type":"And","e1":{"_type":"Or","e1":{"symbol":"X3","_type":"Variable"},"e2":{"symbol":"Y3","_type":"Variable"}},"e2":{"_type":"Or","e1":{"symbol":"X4","_type":"Variable"},"e2":{"symbol":"Y4","_type":"Variable"}}}}}""")
  println(s"The expression is the same as the previous expression: ${expr == complexExpression}")

}
