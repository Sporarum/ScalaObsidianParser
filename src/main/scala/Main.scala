import fastparse.{*, given}
import fastparse.NoWhitespace.noWhitespaceImplicit
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

def noLineBreaks[$: P] = P( CharsWhile(c => c != '\n', min = 1) )

def lineWhitespace[$: P] = P( CharsWhileIn(" \t", min = 0) )

def simpleTag[$: P] = P( CharsWhileIn("a-zA-Z0-9_\\-", min = 1) )

def tag[$: P]: P[Seq[String]] = P( "#" ~ simpleTag.!.rep(min = 1, sep = "/") )

def noteName[$: P] = P( CharsWhileIn("a-zA-Z0-9_\\- ", min = 1) )

def sectionName[$: P] = P( CharsWhileIn("a-zA-Z0-9_\\- ", min = 1) )

def wikilink[$: P]: P[(Option[String], Option[String])] = P("[[" ~ noteName.!.? ~ ("#" ~ sectionName.!).? ~ "]]")

object Frontmatter:
  def quotedLine[$: P]: P[String] = P( "\"" ~ CharsWhile(c => c != '\n' && c != '"', min = 0).! ~ "\"") // TODO: Add escapes

  def yamlKey[$: P]: P[String] = P( CharsWhileIn("a-zA-Z\\-", min = 1).! )

  def yamlValue[$: P]: P[String | Seq[String]] = P( yamlScalar | yamlSequence )

  def yamlScalar[$: P]: P[String] = P( " " ~ (quotedLine | noLineBreaks.?.!))

  def yamlSequenceLine[$: P]: P[String] = P( lineWhitespace ~ "-" ~ yamlScalar )
  def yamlSequence[$: P]: P[Seq[String]] = P( "\n" ~ yamlSequenceLine.rep(min = 1, sep = "\n") )

  def simplifiedYaml[$: P]: P[Map[String, String | Seq[String]]] = P( (yamlKey ~ ":" ~ yamlValue).rep(min = 0, sep = "\n").map(Map(_*)) )

  def parser[$: P] = P("---\n" ~ simplifiedYaml ~ "\n---")


def parseA[$: P] = P("a")

@main def hello(): Unit =
  testParse("a", parseA, ())

  testParser(tag)(
    ("#test", Seq("test")),
    ("#test/moreSpecific", Seq("test", "moreSpecific")),
    ("#a-b_cZZZ", Seq("a-b_cZZZ")),
    ("#FolderNote", Seq("FolderNote"))
  )

  testParser(wikilink)(
    ("[[]]", (None, None)), // TODO: dubious
    ("[[Master]]", (Some("Master"), None)),
    ("[[Branches#openrazer]]", (Some("Branches"), Some("openrazer"))),
  )

  testParser(Frontmatter.parser)(
    ("""
    |---
    |tech-stack:
    |  - "[[HTML]]"
    |  - CSS
    |  - "[[Typescript]]"
    |  - "[[React]]"
    |---
    |""".stripMargin.tail.init, Map(
      "tech-stack" -> Seq(
        "[[HTML]]",
        "CSS",
        "[[Typescript]]",
        "[[React]]",
      )
    )),
    ("""
    |---
    |summary: "A test"
    |tech-stack:
    |  - "[[HTML]]"
    |  - "[[CSS]]"
    |  - "[[Typescript]]"
    |  - "[[React]]"
    |---
    |""".stripMargin.tail.init, Map(
      "summary" -> "A test",
      "tech-stack" -> Seq(
        "[[HTML]]",
        "[[CSS]]",
        "[[Typescript]]",
        "[[React]]",
      )
    )),
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