package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /** ****************
    * * TWEET LENGTH **
    * *****************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  /** ****************
    * * CALCULATOR  **
    * *****************/

  trait testSet1 {
    val map1 = Map(
      "a" -> Signal(Literal(1.0).asInstanceOf[Expr]),
      "b" -> Signal(Literal(2.0).asInstanceOf[Expr]),
      "c" -> Signal(Literal(3.0).asInstanceOf[Expr]),
      "d" -> Signal(Literal(4.0).asInstanceOf[Expr]),
      "e" -> Signal(Ref("b").asInstanceOf[Expr]),
      "f" -> Signal(Plus(Literal(1.0).asInstanceOf[Expr], Ref("a").asInstanceOf[Expr]).asInstanceOf[Expr]),
      "g" -> Signal(Minus(Literal(1.0).asInstanceOf[Expr], Ref("a").asInstanceOf[Expr]).asInstanceOf[Expr]),
      "h" -> Signal(Times(Ref("f").asInstanceOf[Expr], Ref("g").asInstanceOf[Expr]).asInstanceOf[Expr])
    )
  }

  test("eval literals and references") {
    new testSet1 {
      assert(Calculator.eval(Literal(3.0), Map()) == 3.0)
      assert(Calculator.eval(Ref("a"), map1) == 1.0)
    }
  }

  test("eval literal arithmetic") {
    new testSet1 {
      assert(Calculator.eval(Plus(Literal(1.0).asInstanceOf[Expr], Literal(1.0).asInstanceOf[Expr]).asInstanceOf[Expr], Map()) == 2.0)
      assert(Calculator.eval(Minus(Literal(2.0).asInstanceOf[Expr], Literal(1.0).asInstanceOf[Expr]).asInstanceOf[Expr], Map()) == 1.0)
      assert(Calculator.eval(Times(Literal(2.0).asInstanceOf[Expr], Literal(2.0).asInstanceOf[Expr]).asInstanceOf[Expr], Map()) == 4.0)
      assert(Calculator.eval(Divide(Literal(9.0).asInstanceOf[Expr], Literal(3.0).asInstanceOf[Expr]).asInstanceOf[Expr], Map()) == 3.0)
    }
  }

  test("eval reference and literal arithmetic") {
    new testSet1 {
      assert(Calculator.eval(Plus(Literal(1.0).asInstanceOf[Expr], Ref("a").asInstanceOf[Expr]).asInstanceOf[Expr], map1) == 2.0)
      assert(Calculator.eval(Minus(Literal(2.0).asInstanceOf[Expr], Ref("a").asInstanceOf[Expr]).asInstanceOf[Expr], map1) == 1.0)
      assert(Calculator.eval(Times(Literal(2.0).asInstanceOf[Expr], Ref("b").asInstanceOf[Expr]).asInstanceOf[Expr], map1) == 4.0)
      assert(Calculator.eval(Divide(Literal(9.0).asInstanceOf[Expr], Ref("c").asInstanceOf[Expr]).asInstanceOf[Expr], map1) == 3.0)
    }
  }

  test("eval complex arithmetic") {
    new testSet1 {
      assert(Calculator.eval(Ref("f").asInstanceOf[Expr], map1) == 2.0)
      assert(Calculator.eval(Ref("h").asInstanceOf[Expr], map1) == 0.0)
    }
  }

  test("computeValues complex arithmetic") {
    new testSet1 {
      val results = Calculator.computeValues(map1)
      assert(results.apply("f")() == 2.0)
      assert(results.apply("h")() == 0.0)
    }
  }
}
