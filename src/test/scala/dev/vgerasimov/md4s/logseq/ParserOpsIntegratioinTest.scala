package dev.vgerasimov.md4s.logseq

import models.*
import ops.{ *, given }

class ParserOpsIntegratioinTest extends munit.ScalaCheckSuite {

  import dev.vgerasimov.md4s.logseq.Parser.*
  import dev.vgerasimov.slowparse.{ P, POut }

  lazy val ctx = Context.default()
  lazy val parser = new Parser(ctx)

  test(
    "find block by property node: flat list of headed sections, each with id property node, should be found"
  ) {
    val toParse = """- # heading 1
  id:: 10
- # heading 2
  id:: 20
- # heading 3
  id:: 30"""
    parser.document(toParse) match
      case POut.Failure(message, _) => fail(s"$toParse not parsed: $message")
      case POut.Success(document, _, _, _) =>
        val actual = document.findBlockByPropertyNode {
          case PropertyDrawer.Node(
                PropertyDrawer.Node.Key("id", _),
                PropertyDrawer.Node.Value(elements),
                _
              ) if elements.exists {
                case Text("20") => true
                case _          => false
              } =>
            true
          case _ => false
        }
        actual match {
          case None => fail("Block not found")
          case Some(
                HeadedSection(
                  Heading(
                    Heading.Level(1, _),
                    List(Text("heading 2")),
                    _,
                    _,
                    Some(
                      PropertyDrawer(
                        List(
                          PropertyDrawer.Node(
                            PropertyDrawer.Node.Key("id", _),
                            PropertyDrawer.Node.Value(List(Text("20"))),
                            _
                          )
                        )
                      )
                    ),
                  ),
                  Nil,
                  _
                )
              ) =>
            ()
          case Some(x) => fail(s"Unexpected block: $x")
        }

  }
}
