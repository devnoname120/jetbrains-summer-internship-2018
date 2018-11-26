package devnoname120

import org.scalatest._

class BooleanExpressionSpec extends FlatSpec with Matchers {
  "True JSON" should "be parsed correctly" in {
    val expr = BooleanExpression.fromJSON("""{"True":{}}""")
    expr shouldEqual True
  }
  "False JSON" should "be parsed correctly" in {
    val expr = BooleanExpression.fromJSON("""{"False":{}}""")
    expr shouldEqual False
  }
  "Variable JSON" should "be parsed correctly" in {
    val expr = BooleanExpression.fromJSON("""{"Variable":{"symbol": "foobar"}}""")
    expr shouldEqual Variable("foobar")
  }
  it should "throw IllegalArgumentException if symbol is missing" in {
    a [IllegalArgumentException] should be thrownBy {
      BooleanExpression.fromJSON("""{"Variable":{}}""")
    }
  }
  it should "throw IllegalArgumentException if symbol is not a string" in {
    a [IllegalArgumentException] should be thrownBy {
      BooleanExpression.fromJSON("""{"Variable": true}""")
    }
  }
  "Not JSON" should "be parsed correctly" in {
    val expr = BooleanExpression.fromJSON("""{"Not":{"e": {"True": {}}}}""")
    expr shouldEqual Not(True)
  }
  it should "throw IllegalArgumentException if expression is missing" in {
    a [IllegalArgumentException] should be thrownBy {
      BooleanExpression.fromJSON("""{"Not": {}}""")
    }
  }
  "Or JSON" should "be parsed correctly" in {
    val expr1 = BooleanExpression.fromJSON("""{"Or":{"e1": {"True": {}},"e2": {"False": {}}}}""")
    expr1 shouldEqual Or(True,False)
    val expr2 = BooleanExpression.fromJSON("""{"Or":{"e2": {"False": {}},"e1": {"True": {}}}}""")
    expr2 shouldEqual Or(True,False)
  }
  it should "throw IllegalArgumentException if first expression is missing" in {
    a [IllegalArgumentException] should be thrownBy {
      BooleanExpression.fromJSON("""{"Or": {"e2": {"True": {}}}}""")
    }
  }
  it should "throw IllegalArgumentException if second expression is missing" in {
    a [IllegalArgumentException] should be thrownBy {
      BooleanExpression.fromJSON("""{"Or": {"e1": {"True": {}}}}""")
    }
  }
  "And JSON" should "be parsed correctly" in {
    val expr = BooleanExpression.fromJSON("""{"And":{"e1": {"True": {}},"e2": {"False": {}}}}""")
    expr shouldEqual And(True,False)
  }
  it should "throw IllegalArgumentException if first expression is missing" in {
    a [IllegalArgumentException] should be thrownBy {
      BooleanExpression.fromJSON("""{"And": {"e2": {"True": {}}}}""")
    }
  }
  it should "throw IllegalArgumentException if second expression is missing" in {
    a [IllegalArgumentException] should be thrownBy {
      BooleanExpression.fromJSON("""{"And": {"e1": {"True": {}}}}""")
    }
  }
  "Complex nested JSON" should "be parsed correctly" in {
    val complex = """{
                    |  "Or" : {
                    |    "e1" : {
                    |      "False" : {
                    |
                    |      }
                    |    },
                    |    "e2" : {
                    |      "And" : {
                    |        "e1" : {
                    |          "Variable" : {
                    |            "symbol" : "foobar"
                    |          }
                    |        },
                    |        "e2" : {
                    |          "Not" : {
                    |            "e" : {
                    |              "True" : {
                    |
                    |              }
                    |            }
                    |          }
                    |        }
                    |      }
                    |    }
                    |  }
                    |}""".stripMargin
    val expr = BooleanExpression.fromJSON(complex)
    expr shouldEqual Or(False, And(Variable("foobar"), Not(True)))
  }
  "Redundant entries in JSON" should "be parsed taking only the first one according to alphabetical order" in {
    val expr = BooleanExpression.fromJSON("""{"True":{}, "False": {}}""")
    expr shouldEqual False
    val expr2 = BooleanExpression.fromJSON("""{"Or":{"e1": {"True": {}}, "e2": {"True": {}}}, "False": {}}""")
    expr2 shouldEqual False
    val expr3 = BooleanExpression.fromJSON("""{"And":{"e1": {"True": {}}, "e2": {"True": {}}}, "False": {}}""")
    expr3 shouldEqual And(True, True)
  }
}
