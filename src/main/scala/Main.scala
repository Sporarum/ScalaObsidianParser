import fastparse.{*, given}
import fastparse.NoWhitespace.noWhitespaceImplicit
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

def parseA[$: P] = P("a")

@main def hello(): Unit =
  testParse("a", parseA, ())


  /*
  testParse("b", parseA, ())
  val f @ Parsed.Failure(label, index, extra) = parse("b", parseA(using _)): @unchecked
  assert(label == "")
  assert(index == 0)
  assert(f.msg == """Position 1:1, found "b"""")
  */

  println("No errors!")


def testParser[T](parser: P[?] ?=> P[T])(testCases: (String, T)*) =
  testCases.foreach{ case (s, expected) =>
    testParse(s, parser, expected)
  }

def testParse[T](s: String, parser: P[?] ?=> P[T], expected: T) =
  parse[T](s, parser(using _)) match
    case Success(got, index) =>
      assert(got == expected)
    case f@Failure(label, index, extra) =>
      assert(false, f.msg)