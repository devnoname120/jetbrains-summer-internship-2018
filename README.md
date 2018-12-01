# Test assignment [![](https://travis-ci.org/devnoname120/jetbrains-summer-internship-2018.svg?branch=master)](https://travis-ci.org/devnoname120/jetbrains-summer-internship-2018)

## Warmup

If we suppose that `f(x) = f(x-1)+f(x-1)` then it follows that `f(x) = 2*f(x-1)`. By induction, `f(x) = 2^x`.
The following Scala code computes this function:

```scala
def f(x: Int) : Int = {
  scala.math.pow(2, x).toInt
}
```

1. My solution has a time and space complexity of `O(1)` (number of operations bounded by a constant) assuming that the exponentiation function of Scala is `O(1)`.
2. I don't think that I could have improved it more. The only way I can improve it is by providing a more efficient exponentiation implementation than Scala's.

**Note:** At first I thought that there might have been a mistake, and what was meant was `f(x) = f(x-1)+f(x-2)`. But then, both `f(0)` and `f(1)` should have been provided, which is not the case. I assumed that it was unlikely that it was forgotten as well.

## JSON serialization

### Functionality

For this assignment, I've built code that has the following functionality:
- Convert a `BooleanExpression` to JSON.
- Convert a JSON to a `BooleanExpression`.
- Display a `BooleanExpression` using math symbols.
- Convert a `BooleanExpression` to CNF / DNF.

Basic behavior can be observed by opening or running [Main.scala](https://github.com/devnoname120/jetbrains-summer-internship-2018/tree/master/src/main/scala/devnoname120/BooleanExpression/Main.scala).

### Testing

I've also made sure that the code was properly tested using ScalaTest. The tests are located in [BooleanExpressionSpec.scala](https://github.com/devnoname120/jetbrains-summer-internship-2018/tree/master/src/test/scala/devnoname120/BooleanExpression/BooleanExpressionSpec.scala).

This repository also has continuous integration. I have chosen [Travis-CI](https://travis-ci.org/devnoname120/jetbrains-summer-internship-2018) for this.

### Known limitations:

- A `Variable` cannot have an empty symbol (design choice).
- The `toMathString` method adds unnecessary parentheses, but the formula is still correct. This could be fixed by unrolling the nested `And`'s and `Or`'s.
- `Json.fromJSON()` can throw Scala Play JSON exceptions if the JSON is invalid, if some fields are missing or if they are of the wrong type. See [BooleanExpressionSpec.scala](https://github.com/devnoname120/jetbrains-summer-internship-2018/tree/master/src/test/scala/devnoname120/BooleanExpression/BooleanExpressionSpec.scala) for examples of expected exceptions.
- Parsing is made using (non-tail)recursive functions, if a JSON is very deep, it can cause stack overflow.