package devnoname120

import devnoname120.Extensions._
import org.scalatest._
import play.api.libs.json.{JsResultException, Json}

class BooleanExpressionSpec extends FlatSpec with Matchers {

  "BooleanExpression" should "be converted to a math string correctly" in {
    True.toMathString() shouldEqual "true"
    False.toMathString() shouldEqual "false"
    Not(True).toMathString() shouldEqual "¬true"
    And(True, False).toMathString() shouldEqual "true ∧ false"
    Or(True, False).toMathString() shouldEqual "true ∨ false"
    Or(And(True, True), False).toMathString() shouldEqual "(true ∧ true) ∨ false"
    Variable("long variable").toMathString() shouldEqual "long_variable"
  }


  "True" should "be parsed correctly from JSON" in {
    val expr = BooleanExpression.fromJSON("""{"_type": "True"}""")
    expr shouldEqual True
  }
  it should "be converted correctly to JSON" in {
    Json.parse(True.toJSON) shouldEqual Json.obj("_type" -> "True")
  }

  "False" should "be parsed correctly from JSON" in {
    val expr = BooleanExpression.fromJSON("""{"_type": "False"}""")
    expr shouldEqual False
  }
  it should "be converted correctly to JSON" in {
    Json.parse(False.toJSON) shouldEqual Json.obj("_type" -> "False")
  }

  "Variable" should "not allow to have an empty symbol" in {
    a [IllegalArgumentException] should be thrownBy {
      Variable("")
    }
  }
  it should "be parsed correctly from JSON" in {
    val expr = BooleanExpression.fromJSON("""{"_type": "Variable", "symbol": "foobar"}""")
    expr shouldEqual Variable("foobar")
  }
  it should "throw JsResultException if symbol is missing when parsing from JSON" in {
    the[JsResultException] thrownBy {
      BooleanExpression.fromJSON("""{"_type": "Variable"}""")
    } should have message "JsResultException(errors:List((/symbol,List(JsonValidationError(List(error.path.missing),WrappedArray())))))"
  }
  it should "throw JsResultException if symbol is empty when parsing from JSON" in {
    the[JsResultException] thrownBy {
      BooleanExpression.fromJSON("""{"_type": "Variable", "symbol": ""}""")
    } should have message "JsResultException(errors:List((/symbol,List(JsonValidationError(List(error.minLength),WrappedArray(1))))))"
  }
  it should "throw JsResultException if symbol is not a string when parsing from JSON" in {
    the[JsResultException] thrownBy {
      BooleanExpression.fromJSON("""{"_type": "Variable", "symbol": true}""")
    } should have message "JsResultException(errors:List((/symbol,List(JsonValidationError(List(error.expected.jsstring),WrappedArray())))))"
  }
  it should "be converted correctly to JSON" in {
    Json.parse(Variable("foobar").toJSON) shouldEqual Json.obj("_type" -> "Variable", "symbol" -> "foobar")
  }

  "Not" should "be parsed correctly from JSON" in {
    val expr = BooleanExpression.fromJSON("""{"_type": "Not", "e": {"_type": "True"}}""")
    expr shouldEqual Not(True)
  }
  it should "use last instance of redefined parameter when parsing from JSON" in {
    val expr = BooleanExpression.fromJSON("""{"_type": "Not", "e": {"_type": "False"}, "e": {"_type": "True"}}""")
    expr shouldEqual Not(True)
    val expr2 = BooleanExpression.fromJSON("""{"_type": "Not", "e": {"_type": "True"}, "e": {"_type": "False"}}""")
    expr2 shouldEqual Not(False)
  }
  it should "throw JsResultException if expression is missing from JSON" in {
    the[JsResultException] thrownBy {
      BooleanExpression.fromJSON("""{"_type": "Not"}""")
    } should have message "JsResultException(errors:List((/e,List(JsonValidationError(List(error.path.missing),WrappedArray())))))"
  }
  it should "be converted correctly to JSON" in {
    Json.parse(Not(True).toJSON) shouldEqual Json.obj("_type" -> "Not", "e" -> Json.parse(True.toJSON))
  }

  "Or" should "be parsed correctly from JSON" in {
    val expr1 = BooleanExpression.fromJSON("""{"_type": "Or", "e1": {"_type": "True"}, "e2": {"_type": "False"}}""")
    expr1 shouldEqual Or(True, False)
    val expr2 = BooleanExpression.fromJSON("""{"_type": "Or", "e2": {"_type": "False"}, "e1": {"_type": "True"}}""")
    expr2 shouldEqual Or(True, False)
  }
  it should "throw JsResultException if first expression is missing from JSON" in {
    the[JsResultException] thrownBy {
      BooleanExpression.fromJSON("""{"_type": "Or", "e2": {"_type": "False"}}""")
    } should have message "JsResultException(errors:List((/e1,List(JsonValidationError(List(error.path.missing),WrappedArray())))))"
  }
  it should "throw JsResultException if second expression is missing from JSON" in {
    the[JsResultException] thrownBy {
      BooleanExpression.fromJSON("""{"_type": "Or", "e1": {"_type": "True"}}""")
    } should have message "JsResultException(errors:List((/e2,List(JsonValidationError(List(error.path.missing),WrappedArray())))))"
  }
  it should "be converted correctly to JSON" in {
    Json.parse(Or(True, False).toJSON) shouldEqual Json.obj(
      "_type" -> "Or",
      "e1" -> Json.parse(True.toJSON),
      "e2" -> Json.parse(False.toJSON)
    )
  }

  "And" should "be parsed correctly from JSON" in {
    val expr = BooleanExpression.fromJSON("""{"_type": "And", "e1": {"_type": "True"}, "e2": {"_type": "False"}}""")
    expr shouldEqual And(True, False)
  }
  it should "be converted correctly to JSON" in {
    Json.parse(Or(False, True).toJSON) shouldEqual Json.obj(
      "_type" -> "Or",
      "e1" -> Json.parse(False.toJSON),
      "e2" -> Json.parse(True.toJSON)
    )
  }

  "Unknown operator JSON" should "throw JsResultException" in {
    the[JsResultException] thrownBy {
      val expr = BooleanExpression.fromJSON("""{"_type": "Xor", "e1": {"_type": "True"}, "e1": {"_type": "False"}}""")
    } should have message "JsResultException(errors:List((,List(JsonValidationError(List(error.invalid),WrappedArray())))))"
  }

  "Complex nested JSON" should "be parsed correctly" in {
    val complex =
      """{
        |  "_type": "Or",
        |  "e1": {
        |    "_type": "False"
        |  },
        |  "e2": {
        |    "_type": "And",
        |    "e1": {
        |      "symbol": "foobar",
        |      "_type": "Variable"
        |    },
        |    "e2": {
        |      "e": {
        |        "_type": "True"
        |      },
        |      "_type": "Not"
        |    }
        |  }
        |}""".stripMargin
    val expr = BooleanExpression.fromJSON(complex)
    expr shouldEqual Or(False, And(Variable("foobar"), Not(True)))
  }

  "toCNF" should "be invariant on CNF forms" in {
    // See https://en.wikipedia.org/wiki/Conjunctive_normal_form#Examples_and_non-examples
    val A = Variable("A")
    A.toCNF shouldEqual A

    val AorB = Or(A, Variable("B"))
    AorB.toCNF shouldEqual AorB

    val expr1 = And(AorB, Variable("C"))
    expr1.toCNF shouldEqual expr1

    val expr2 = And(expr1, Or(Not(Variable("D")), Or(Variable("E"), Variable("F"))))
    expr2.toCNF shouldEqual expr2
  }

  it should "convert correctly to CNF" in {
    val A = Variable("A")
    val B = Variable("B")
    val C = Variable("C")

    val expr = Not(Or(B, C))
    val exprCNF = And(Not(B), Not(C))
    expr.toCNF shouldEqual exprCNF

    val expr2 = Or(And(A, B), C)
    val exprCNF2 = And(Or(A,C), Or(B, C))
    expr2.toCNF shouldEqual exprCNF2

    val expr3 = Or(C, And(A, B))
    val exprCNF3 = And(Or(C, A), Or(C, B))
    expr3.toCNF shouldEqual exprCNF3

    val notNotTrue = Not(Not(True))
    notNotTrue.toCNF shouldEqual True

    val notNotFalse = Not(Not(False))
    notNotFalse.toCNF shouldEqual False
  }

  "toDNF" should "be invariant on DNF forms" in {
    // See https://en.wikipedia.org/wiki/Disjunctive_normal_form#Definition
    val A = Variable("A")
    val B = Variable("B")
    val C = Variable("C")
    val D = Variable("D")
    val E = Variable("E")
    val F = Variable("F")

    A.toDNF shouldEqual A

    val AandB = And(A, Variable("B"))
    AandB.toDNF shouldEqual AandB

    val expr1 = Or(AandB, Variable("C"))
    expr1.toDNF shouldEqual expr1

    val expr2 = Or(And(A, And(Not(B), Not(C))), And(Not(D), And(E, F)))
    expr2.toDNF shouldEqual expr2
  }

  it should "convert correctly to DNF" in {
    val A = Variable("A")
    val B = Variable("B")
    val C = Variable("C")
    val D = Variable("D")

    val expr = Not(Or(A, B))
    val exprDNF = And(Not(A), Not(B))
    expr.toDNF shouldEqual exprDNF

    val expr2 = Or(A, And(B, Or(C, D)))
    val exprDNF2 = Or(A, Or(And(B, C), And(B, D)))
    expr2.toDNF shouldEqual exprDNF2
  }
}
