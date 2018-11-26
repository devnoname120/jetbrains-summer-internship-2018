package devnoname120

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

sealed trait BooleanExpression
case object True extends BooleanExpression
case object False extends BooleanExpression
case class Variable(symbol: String) extends BooleanExpression
case class Not(e: BooleanExpression) extends BooleanExpression
case class Or(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression
case class And(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression


object BooleanExpression {
  private def parseDecodeError(err: DecodingFailure, cursor: HCursor) : String = {
    val path: String = CursorOp.opsToPath(err.history)
    val down = cursor.downField(path)

    if (down.succeeded) {
      s"Could not decode [$path.${down.focus.get}] as type [${err.message}]."
    } else {
      s"The field [$path] is incorrect."
    }
  }

  def fromJSON(booleanExpression: String) : BooleanExpression = {
    parse(booleanExpression) match {
      case Left(err: ParsingFailure) => throw err
      case Right(json: Json) =>
        val cursor = json.hcursor
        json.as[BooleanExpression] match {
          case Right(expr) => expr
          case Left(err: DecodingFailure) => throw new IllegalArgumentException(parseDecodeError(err, cursor))
        }
    }
      }
  def toJSON(booleanExpression: BooleanExpression) : String = {
    booleanExpression.asJson.noSpaces
  }
}

