package dev.vgerasimov.md4s.logseq

import dev.vgerasimov.slowparse.{ P, POut }

import models.*
import ops.{ *, given }
import parser.*
import dev.vgerasimov.md4s.logseq.models.MarkdownList.Unordered

class LogseqMarkdownParserTest extends munit.ScalaCheckSuite {

  test("some valid markup string") {
    val toParse = """*hello world*"""
    checkParser(
      parser.document,
      toParse,
      LogseqMarkdown(
        List(
          Paragraph(
            InlineContainer(
              List(
                Emphasis(
                  Emphasis.Marker.Italic("*"),
                  InlineContainer(List(Text("hello world")))
                )
              )
            )
          )
        )
      )
    )
  }

  test("some one-way nested headings - simple") {
    val toParse = """
|# Heading 1
|## Heading 2
|### Heading 3
|""".trim().stripMargin
    checkParser(
      parser.document,
      toParse,
      LogseqMarkdown(
        List(
          HeadedSection(
            h("Heading 1", 1),
            List(
              HeadedSection(
                h("Heading 2", 2),
                List(
                  HeadedSection(
                    h("Heading 3", 3),
                    List()
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  test("some one-way nested headings - two on level 2") {
    val toParse = """
|# Heading 1
|## Heading 2.1
|## Heading 2.2
|""".trim().stripMargin
    checkParser(
      parser.document,
      toParse,
      LogseqMarkdown(
        List(
          HeadedSection(
            h("Heading 1", 1),
            List(
              HeadedSection(
                h("Heading 2.1", 2),
                List()
              ),
              HeadedSection(
                h("Heading 2.2", 2),
                List()
              )
            )
          )
        )
      )
    )
  }

  test("some nested headings") {
    val toParse = """
|# Heading 1
|## Heading 2
|## Heading 2.2
|### Heading 3
|#### Heading 4
|### Heading 3.1
|##### Heading 5
|# Heading 1.1
|###### Heading 6
|""".trim().stripMargin
    checkParser(
      parser.document,
      toParse,
      LogseqMarkdown(
        List(
          HeadedSection(
            h("Heading 1", 1),
            List(
              HeadedSection(h("Heading 2", 2), List()),
              HeadedSection(
                h("Heading 2.2", 2),
                List(
                  HeadedSection(
                    h("Heading 3", 3),
                    List(
                      HeadedSection(
                        h("Heading 4", 4),
                        List()
                      )
                    )
                  ),
                  HeadedSection(
                    h("Heading 3.1", 3),
                    List(
                      HeadedSection(
                        h("Heading 5", 5),
                        List()
                      )
                    )
                  )
                )
              )
            )
          ),
          HeadedSection(
            h("Heading 1.1", 1),
            List(
              HeadedSection(
                h("Heading 6", 6),
                List()
              )
            )
          )
        )
      )
    )
  }

  test("one item list") {
    val toParse = """
|- list item 1
""".trim().stripMargin
    checkParser(
      parser.document,
      toParse,
      LogseqMarkdown(
        List(
          MarkdownList.Unordered(
            List(
              listItem("list item 1")
            )
          )
        )
      )
    )
  }

  test("simple nested list") {
    val toParse = """
|- list item 1
|  - list item 1.1
|  - list item 1.2
""".trim().stripMargin
    checkParser(
      parser.document,
      toParse,
      LogseqMarkdown(
        List(
          MarkdownList.Unordered(
            List(
              MarkdownList.Item(
                List(
                  Paragraph(InlineContainer(List(Text("list item 1")))),
                  MarkdownList.Unordered(
                    indentation = Indentation(1, "  "),
                    items = List(
                      listItem("list item 1.1"),
                      listItem("list item 1.2")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  test("lists with headings") {
    val toParse = """
|- # Heading 1
|- # Heading 2
|- # TODO todo heading
|- # [#A] DONE done heading with priority
|- # DOING doing heading with properties
|  k1:: v1
|  k2:: v2
|- # last heading
|and some text, just because
""".trim().stripMargin
    checkParser(
      parser.document,
      toParse,
      LogseqMarkdown(
        List(
          MarkdownList.Unordered(
            List(
              MarkdownList.Item(
                List(
                  HeadedSection(h("Heading 1", 1), List())
                )
              ),
              MarkdownList.Item(
                List(
                  HeadedSection(h("Heading 2", 1), List())
                )
              ),
              MarkdownList.Item(
                List(
                  HeadedSection(
                    Heading(
                      Some(InlineContainer(List(Text("todo heading")))),
                      1,
                      status = Some(Status("TODO"))
                    ),
                    List()
                  )
                )
              ),
              MarkdownList.Item(
                List(
                  HeadedSection(
                    Heading(
                      Some(InlineContainer(List(Text("done heading with priority")))),
                      1,
                      status = Some(Status("DONE")),
                      priority = Some(Priority('A'))
                    ),
                    List()
                  )
                )
              ),
              MarkdownList.Item(
                List(
                  HeadedSection(
                    Heading(
                      Some(InlineContainer(List(Text("doing heading with properties")))),
                      1,
                      status = Some(Status("DOING")),
                      propertyDrawer = Some(
                        PropertyDrawer(
                          List(
                            PropertyDrawer.Node("k1", Some(InlineContainer(List(Text("v1"))))),
                            PropertyDrawer.Node("k2", Some(InlineContainer(List(Text("v2")))))
                          )
                        )
                      )
                    ),
                    List()
                  )
                )
              ),
              MarkdownList.Item(
                List(
                  HeadedSection(
                    h("last heading", 1),
                    List(Paragraph(InlineContainer(List(Text("and some text, just because")))))
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  test("some nested headings + lists") {
    val toParse = """
|# Heading 1
|+ list item 1
|## Heading 2.1
|- list item 1
|- list item 2
|## Heading 2.2
|""".trim().stripMargin
    checkParser(
      parser.document,
      toParse,
      LogseqMarkdown(
        List(
          HeadedSection(
            h("Heading 1", 1),
            List(
              MarkdownList.Unordered(
                List(
                  listItem("list item 1")
                )
              ),
              HeadedSection(
                h("Heading 2.1", 2),
                List(
                  MarkdownList.Unordered(
                    List(
                      listItem("list item 1"),
                      listItem("list item 2")
                    )
                  )
                )
              ),
              HeadedSection(
                h("Heading 2.2", 2),
                List()
              )
            )
          )
        )
      )
    )
  }

  test("some more complex nested headings + lists") {
    val toParse = """
|# Heading 1
|+ list item 1
|  - list item 1.1
|  - list item 1.2
|+ list item 2
|## Heading 2.1
|- list item 1
|- list item 2
|## Heading 2.2
|- # list heading 1
|- ## list heading 2
|  ### list heading 2.1
|  - list item 2.1.1
|  - list item 2.1.2
|  ### list heading 2.2
|  - list item 2.2.1
|- ### list heading 3
|""".trim().stripMargin

    checkParser(
      parser.document,
      toParse,
      LogseqMarkdown(
        List(
          HeadedSection(
            h("Heading 1", 1),
            List(
              MarkdownList.Unordered(
                List(
                  MarkdownList.Item(
                    List(
                      Paragraph(InlineContainer(List(Text("list item 1")))),
                      MarkdownList.Unordered(
                        indentation = Indentation(1, "  "),
                        items = List(
                          listItem("list item 1.1"),
                          listItem("list item 1.2")
                        )
                      )
                    )
                  ),
                  listItem("list item 2")
                )
              ),
              HeadedSection(
                h("Heading 2.1", 2),
                List(
                  MarkdownList.Unordered(
                    List(
                      listItem("list item 1"),
                      listItem("list item 2")
                    )
                  )
                )
              ),
              HeadedSection(
                h("Heading 2.2", 2),
                List(
                  MarkdownList.Unordered(
                    List(
                      MarkdownList.Item(
                        List(
                          HeadedSection(
                            h("list heading 1", 1),
                            List()
                          )
                        )
                      ),
                      MarkdownList.Item(
                        List(
                          HeadedSection(
                            h("list heading 2", 2),
                            List(
                              HeadedSection(
                                h("list heading 2.1", 3),
                                List(
                                  MarkdownList.Unordered(
                                    List(
                                      listItem("list item 2.1.1"),
                                      listItem("list item 2.1.2")
                                    ),
                                    indentation = Indentation(1, "  ")
                                  )
                                ),
                                indentation = Indentation(1, "  ")
                              ),
                              HeadedSection(
                                h("list heading 2.2", 3),
                                List(
                                  MarkdownList.Unordered(
                                    List(
                                      listItem("list item 2.2.1")
                                    ),
                                    indentation = Indentation(1, "  ")
                                  )
                                ),
                                indentation = Indentation(1, "  ")
                              )
                            ),
                            indentation = Indentation(0, "")
                          )
                        )
                      ),
                      MarkdownList.Item(
                        List(
                          HeadedSection(
                            h("list heading 3", 3),
                            List()
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  test("document with properties") {
    val toParse = """
|k1:: v1
|k2:: v2
|# Heading 1
|k3:: v3
|## Heading 2
|k4:: v4
|text""".trim().stripMargin
    checkParser(
      parser.document,
      toParse,
      LogseqMarkdown(
        List(
          HeadedSection(
            h(
              "Heading 1",
              1,
              propertyDrawer = Some(
                PropertyDrawer(
                  List(PropertyDrawer.Node("k3", Some(InlineContainer(List(Text("v3"))))))
                )
              )
            ),
            List(
              HeadedSection(
                h(
                  "Heading 2",
                  2,
                  propertyDrawer = Some(
                    PropertyDrawer(
                      List(PropertyDrawer.Node("k4", Some(InlineContainer(List(Text("v4"))))))
                    )
                  )
                ),
                List(
                  Paragraph(InlineContainer(List(Text("text"))))
                )
              )
            )
          )
        ),
        propertyDrawer = Some(
          PropertyDrawer(
            List(
              PropertyDrawer.Node("k1", Some(InlineContainer(List(Text("v1"))))),
              PropertyDrawer.Node("k2", Some(InlineContainer(List(Text("v2")))))
            )
          )
        )
      )
    )
  }

  lazy val ctx = Context.defaultCtx
  lazy val parser = new parser(ctx)

  extension [T](r: POut[T])
    def isSuccess: Boolean = r match
      case _: POut.Success[?] => true
      case _: POut.Failure    => false

  export dev.vgerasimov.slowparse.POut

  def parse[T](toParse: String, parser: P[T]): POut[T] = parser(toParse)

  def checkParser[T](parser: P[T], toParse: String, expected: => T): Unit =
    parse(toParse, parser) match {
      case POut.Success(value, _, _, _) =>
        // pprint.pprintln(value)
        assertEquals(value, expected)
      case POut.Failure(message, _) => fail(s"$toParse not parsed: $message")
    }

  def checkParserFailed[T](parser: P[T], toParse: String): Unit =
    parse(toParse, parser) match {
      case POut.Success(value, _, _, _) => fail(s"$toParse parsed to $value")
      case _                            =>
    }

  def ignore(v: String)(body: => Any): Unit = (
    ()
  )
}
