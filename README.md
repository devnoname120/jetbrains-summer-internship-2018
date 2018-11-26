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

TODO