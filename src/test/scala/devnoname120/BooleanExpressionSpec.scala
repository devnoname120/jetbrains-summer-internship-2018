package devnoname120

import org.scalatest._
import play.api.libs.json.JsResultException
import Extensions._

class BooleanExpressionSpec extends FlatSpec with Matchers {
  "Variable" should "not allow to have an empty symbol" in {
    a [IllegalArgumentException] should be thrownBy {
      Variable("")
    }
  }

  "BooleanExpression" should "be converted to a math string correctly" in {
    True.toMathString() shouldEqual "true"
    False.toMathString() shouldEqual "false"
    Not(True).toMathString() shouldEqual "¬true"
    And(True, False).toMathString() shouldEqual "true ∧ false"
    Or(True, False).toMathString() shouldEqual "true ∨ false"
    Or(And(True, True), False).toMathString() shouldEqual "(true ∧ true) ∨ false"
    Variable("long variable").toMathString() shouldEqual "long_variable"
  }

  "True JSON" should "be parsed correctly" in {
    val expr = BooleanExpression.fromJSON("""{"_type": "True"}""")
    expr shouldEqual True
  }
  "False JSON" should "be parsed correctly" in {
    val expr = BooleanExpression.fromJSON("""{"_type": "False"}""")
    expr shouldEqual False
  }

  "Variable JSON" should "be parsed correctly" in {
    val expr = BooleanExpression.fromJSON("""{"_type": "Variable", "symbol": "foobar"}""")
    expr shouldEqual Variable("foobar")
  }
  it should "throw JsResultException if symbol is missing" in {
    the[JsResultException] thrownBy {
      BooleanExpression.fromJSON("""{"_type": "Variable"}""")
    } should have message "JsResultException(errors:List((/symbol,List(JsonValidationError(List(error.path.missing),WrappedArray())))))"
  }
  it should "throw JsResultException if symbol is empty" in {
    the[JsResultException] thrownBy {
      BooleanExpression.fromJSON("""{"_type": "Variable", "symbol": ""}""")
    } should have message "JsResultException(errors:List((/symbol,List(JsonValidationError(List(error.minLength),WrappedArray(1))))))"
  }
  it should "throw JsResultException if symbol is not a string" in {
    the[JsResultException] thrownBy {
      BooleanExpression.fromJSON("""{"_type": "Variable", "symbol": true}""")
    } should have message "JsResultException(errors:List((/symbol,List(JsonValidationError(List(error.expected.jsstring),WrappedArray())))))"
  }

  "Not JSON" should "be parsed correctly" in {
    val expr = BooleanExpression.fromJSON("""{"_type": "Not", "e": {"_type": "True"}}""")
    expr shouldEqual Not(True)
  }
  it should "use last instance of redefined parameter" in {
    val expr = BooleanExpression.fromJSON("""{"_type": "Not", "e": {"_type": "False"}, "e": {"_type": "True"}}""")
    expr shouldEqual Not(True)
    val expr2 = BooleanExpression.fromJSON("""{"_type": "Not", "e": {"_type": "True"}, "e": {"_type": "False"}}""")
    expr2 shouldEqual Not(False)
  }
  it should "throw JsResultException if expression is missing" in {
    the[JsResultException] thrownBy {
      BooleanExpression.fromJSON("""{"_type": "Not"}""")
    } should have message "JsResultException(errors:List((/e,List(JsonValidationError(List(error.path.missing),WrappedArray())))))"
  }

  "Or JSON" should "be parsed correctly" in {
    val expr1 = BooleanExpression.fromJSON("""{"_type": "Or", "e1": {"_type": "True"}, "e2": {"_type": "False"}}""")
    expr1 shouldEqual Or(True, False)
    val expr2 = BooleanExpression.fromJSON("""{"_type": "Or", "e2": {"_type": "False"}, "e1": {"_type": "True"}}""")
    expr2 shouldEqual Or(True, False)
  }
  it should "throw JsResultException if first expression is missing" in {
    the[JsResultException] thrownBy {
      BooleanExpression.fromJSON("""{"_type": "Or", "e2": {"_type": "False"}}""")
    } should have message "JsResultException(errors:List((/e1,List(JsonValidationError(List(error.path.missing),WrappedArray())))))"
  }
  it should "throw JsResultException if second expression is missing" in {
    the[JsResultException] thrownBy {
      BooleanExpression.fromJSON("""{"_type": "Or", "e1": {"_type": "True"}}""")
    } should have message "JsResultException(errors:List((/e2,List(JsonValidationError(List(error.path.missing),WrappedArray())))))"
  }

  "And JSON" should "be parsed correctly" in {
    val expr = BooleanExpression.fromJSON("""{"_type": "And", "e1": {"_type": "True"}, "e2": {"_type": "False"}}""")
    expr shouldEqual And(True, False)
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
}
