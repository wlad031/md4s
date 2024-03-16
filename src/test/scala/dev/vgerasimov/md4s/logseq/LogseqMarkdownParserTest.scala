package dev.vgerasimov.md4s
package logseq

import dev.vgerasimov.slowparse.{ P, POut }

import models.*
import ops.{ *, given }
import parser.*
import dev.vgerasimov.md4s.logseq.blockElements.MarkdownList

class LogseqMarkdownParserTest extends munit.ScalaCheckSuite:

  test("some valid markup string") {
    val toParse = """*hello world*"""
    checkParser(
      parser.document,
      toParse,
      LogseqMarkdown(
        blocks = List(
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
        blocks = List(
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
        blocks = List(
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
        blocks = List(
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
        blocks = List(
          MarkdownList.Unordered(
            List(
              listItem("list item 1", "-")
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
        blocks = List(
          MarkdownList.Unordered(
            List(
              MarkdownList.Item(
                List(
                  Paragraph(InlineContainer(List(Text("list item 1")))),
                  MarkdownList.Unordered(
                    indentation = Some(Indentation(1, "  ")),
                    items = List(
                      listItem("list item 1.1", "-"),
                      listItem("list item 1.2", "-")
                    )
                  )
                ),
                marker = MarkdownList.Item.Marker("-")
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
        blocks = List(
          MarkdownList.Unordered(
            items = List(
              MarkdownList.Item(
                content = List(
                  HeadedSection(heading = h("Heading 1", 1), content = List())
                ),
                marker = MarkdownList.Item.Marker("-")
              ),
              MarkdownList.Item(
                content = List(
                  HeadedSection(heading = h("Heading 2", 1), content = List())
                ),
                marker = MarkdownList.Item.Marker("-")
              ),
              MarkdownList.Item(
                content = List(
                  HeadedSection(
                    heading = Heading(
                      level = Heading.Level(value = 1),
                      content = Some(InlineContainer(List(Text("todo heading")))),
                      status = Some(Status("TODO", spacingAfter = Some(Spacing(" "))))
                    ),
                    content = List()
                  )
                ),
                marker = MarkdownList.Item.Marker("-")
              ),
              MarkdownList.Item(
                content = List(
                  HeadedSection(
                    heading = Heading(
                      level = Heading.Level(value = 1),
                      content = Some(InlineContainer(List(Text("done heading with priority")))),
                      status = Some(Status("DONE", spacingAfter = Some(Spacing(" ")))),
                      priority = Some(Priority('A', spacingAfter = Some(Spacing(" "))))
                    ),
                    content = List()
                  )
                ),
                marker = MarkdownList.Item.Marker("-")
              ),
              MarkdownList.Item(
                content = List(
                  HeadedSection(
                    Heading(
                      level = Heading.Level(value = 1),
                      content = Some(InlineContainer(List(Text("doing heading with properties")))),
                      status = Some(Status("DOING", spacingAfter = Some(Spacing(" ")))),
                      propertyDrawer = Some(
                        PropertyDrawer(
                          List(
                            PropertyDrawer.Node(
                              "k1",
                              Some(InlineContainer(List(Text("v1")))),
                              spacingBeforeName = Some(Spacing("  ")),
                              spacingBeforeValue = Some(Spacing(" "))
                            ),
                            PropertyDrawer.Node(
                              "k2",
                              Some(InlineContainer(List(Text("v2")))),
                              spacingBeforeName = Some(Spacing("  ")),
                              spacingBeforeValue = Some(Spacing(" "))
                            )
                          )
                        )
                      )
                    ),
                    content = List()
                  )
                ),
                marker = MarkdownList.Item.Marker("-")
              ),
              MarkdownList.Item(
                content = List(
                  HeadedSection(
                    heading = h("last heading", 1),
                    content =
                      List(Paragraph(InlineContainer(List(Text("and some text, just because")))))
                  )
                ),
                marker = MarkdownList.Item.Marker("-")
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
        blocks = List(
          HeadedSection(
            heading = h("Heading 1", 1),
            content = List(
              MarkdownList.Unordered(
                items = List(
                  listItem("list item 1", "+")
                )
              ),
              HeadedSection(
                heading = h("Heading 2.1", 2),
                content = List(
                  MarkdownList.Unordered(
                    items = List(
                      listItem("list item 1", "-"),
                      listItem("list item 2", "-")
                    )
                  )
                )
              ),
              HeadedSection(
                heading = h("Heading 2.2", 2),
                content = List()
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
        blocks = List(
          HeadedSection(
            heading = h("Heading 1", 1),
            content = List(
              MarkdownList.Unordered(
                List(
                  MarkdownList.Item(
                    content = List(
                      Paragraph(InlineContainer(List(Text("list item 1")))),
                      MarkdownList.Unordered(
                        indentation = Some(Indentation(1, "  ")),
                        items = List(
                          listItem("list item 1.1", "-"),
                          listItem("list item 1.2", "-")
                        )
                      )
                    ),
                    marker = MarkdownList.Item.Marker("+")
                  ),
                  listItem("list item 2", "+")
                )
              ),
              HeadedSection(
                heading = h("Heading 2.1", 2),
                content = List(
                  MarkdownList.Unordered(
                    List(
                      listItem("list item 1", "-"),
                      listItem("list item 2", "-")
                    )
                  )
                )
              ),
              HeadedSection(
                heading = h("Heading 2.2", 2),
                content = List(
                  MarkdownList.Unordered(
                    List(
                      MarkdownList.Item(
                        List(
                          HeadedSection(
                            h("list heading 1", 1),
                            List()
                          )
                        ),
                        marker = MarkdownList.Item.Marker("-")
                      ),
                      MarkdownList.Item(
                        content = List(
                          HeadedSection(
                            heading = h("list heading 2", 2),
                            content = List(
                              HeadedSection(
                                heading = h("list heading 2.1", 3),
                                content = List(
                                  MarkdownList.Unordered(
                                    List(
                                      listItem("list item 2.1.1", "-"),
                                      listItem("list item 2.1.2", "-")
                                    ),
                                    indentation = Some(Indentation(1, "  "))
                                  )
                                ),
                                indentation = Some(Indentation(1, "  "))
                              ),
                              HeadedSection(
                                heading = h("list heading 2.2", 3),
                                content = List(
                                  MarkdownList.Unordered(
                                    items = List(
                                      listItem("list item 2.2.1", "-")
                                    ),
                                    indentation = Some(Indentation(1, "  "))
                                  )
                                ),
                                indentation = Some(Indentation(1, "  "))
                              )
                            ),
                            indentation = Some(Indentation(0, ""))
                          )
                        ),
                        marker = MarkdownList.Item.Marker("-")
                      ),
                      MarkdownList.Item(
                        content = List(
                          HeadedSection(
                            heading = h("list heading 3", 3),
                            content = List()
                          )
                        ),
                        marker = MarkdownList.Item.Marker("-")
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
        blocks = List(
          HeadedSection(
            heading = h(
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
                heading = h(
                  "Heading 2",
                  2,
                  propertyDrawer = Some(
                    PropertyDrawer(
                      List(PropertyDrawer.Node("k4", Some(InlineContainer(List(Text("v4"))))))
                    )
                  )
                ),
                content = List(
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

  test("Some more or less complex document is parsed correctly") {
    val toParse = """
|  type:: [[Media/Movie]]
|  author:: [[Кристофер Нолан]]
|  status:: [[DONE]]
|  alias:: Oppenheimer
|  rating:: 5
|  done-date:: [[2023-07-29]]
|- DONE [[Oppenheimer]] in [[Cinema City]]
|  SCHEDULED: <2023-07-29 Sat 19:00>  
|- # Cast
|	- [[Киллиан Мерфи]]
|	- [[Мэтт Дэймон]]
|	- [[Роберт Дауни мл.]]
|	- [[Эмили Блант]]
|	- [[Рами Малек]]
|	- [[Флоренс Пью]]
|	- [[Гари Олдман]]
|	- others
""".trim().stripMargin
    checkParser(
      parser.document,
      toParse,
      LogseqMarkdown(
        propertyDrawer = Some(
          PropertyDrawer(
            List(
              PropertyDrawer.Node(
                "type",
                Some(InlineContainer(List(classicInternalLink("Media/Movie")))),
                spacingBeforeName = Some(Spacing("  "))
              ),
              PropertyDrawer.Node(
                "author",
                Some(InlineContainer(List(classicInternalLink("Кристофер Нолан")))),
                spacingBeforeName = Some(Spacing("  "))
              ),
              PropertyDrawer.Node(
                "status",
                Some(InlineContainer(List(classicInternalLink("DONE")))),
                spacingBeforeName = Some(Spacing("  "))
              ),
              PropertyDrawer.Node(
                "alias",
                Some(InlineContainer(List(Text("Oppenheimer")))),
                spacingBeforeName = Some(Spacing("  "))
              ),
              PropertyDrawer.Node(
                "rating",
                Some(InlineContainer(List(Text("5")))),
                spacingBeforeName = Some(Spacing("  "))
              ),
              PropertyDrawer.Node(
                "done-date",
                Some(InlineContainer(List(classicInternalLink("2023-07-29")))),
                spacingBeforeName = Some(Spacing("  "))
              )
            )
          )
        ),
        blocks = List(
          MarkdownList.Unordered(items =
            List(
              MarkdownList.Item(
                marker = MarkdownList.Item.Marker("-"),
                content = List(
                  Paragraph(
                    content = InlineContainer(
                      List(
                        classicInternalLink("Oppenheimer"),
                        Text(" in "),
                        classicInternalLink("Cinema City")
                      )
                    ),
                    status = Some(Status("DONE", spacingAfter = Some(Spacing(" ")))),
                    planning = List(
                      Planning.Scheduled(
                        spacingBeforeKeyword = Some(Spacing("  ")),
                        spacingBeforeTimestamp = Some(Spacing(" ")),
                        spacingAfterTimestamp = Some(Spacing("  ")),
                        timestamp = Timestamp.ActiveTimestamp(
                          date = Timestamp.Date(
                            Timestamp.Date.Year(2023),
                            Timestamp.Date.Month(7),
                            Timestamp.Date.Day(29),
                            dayName = Some(Timestamp.Date.DayName.Saturday)
                          ),
                          time =
                            Some(Timestamp.Time(Timestamp.Time.Hour(19), Timestamp.Time.Minute(0)))
                        )
                      )
                    )
                  )
                )
              ),
              MarkdownList.Item(
                marker = MarkdownList.Item.Marker("-"),
                content = List(
                  HeadedSection(
                    heading = h("Cast", 1),
                    content = List(
                      MarkdownList.Unordered(
                        List(
                          listItem(classicInternalLink("Киллиан Мерфи"), "-"),
                          listItem(classicInternalLink("Мэтт Дэймон"), "-"),
                          listItem(classicInternalLink("Роберт Дауни мл."), "-"),
                          listItem(classicInternalLink("Эмили Блант"), "-"),
                          listItem(classicInternalLink("Рами Малек"), "-"),
                          listItem(classicInternalLink("Флоренс Пью"), "-"),
                          listItem(classicInternalLink("Гари Олдман"), "-"),
                          listItem("others", "-")
                        ),
                        indentation = Some(Indentation(1, "	"))
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

  lazy val ctx = Context.defaultCtx
  lazy val parser = new parser(ctx)

  extension [T](r: POut[T])
    def isSuccess: Boolean = r match
      case _: POut.Success[?] => true
      case _: POut.Failure    => false

  export dev.vgerasimov.slowparse.POut

  def parse[T](toParse: String, parser: P[T]): POut[T] = parser(toParse)

  def checkParser[T](
    parser: P[LogseqMarkdown],
    toParse: String,
    expected: => LogseqMarkdown
  ): Unit =
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

end LogseqMarkdownParserTest
