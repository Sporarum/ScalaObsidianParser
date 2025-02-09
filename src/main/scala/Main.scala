import fastparse.{*, given}
import fastparse.NoWhitespace.noWhitespaceImplicit
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

def simpleTag[$: P] = P( CharsWhileIn("a-zA-Z0-9_\\-", min = 1) )

def tag[$: P]: P[Seq[String]] = P( "#" ~ simpleTag.!.rep(min = 1, sep = "/") )

def parseA[$: P] = P("a")

@main def hello(): Unit =
  testParse("a", parseA, ())

  testParser(tag)(
    ("#test", Seq("test")),
    ("#test/moreSpecific", Seq("test", "moreSpecific")),
    ("#a-b_cZZZ", Seq("a-b_cZZZ")),
    ("#FolderNote", Seq("FolderNote"))
  )


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