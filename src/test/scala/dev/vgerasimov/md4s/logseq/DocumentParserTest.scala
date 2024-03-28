package dev.vgerasimov.md4s
package logseq

import dev.vgerasimov.slowparse.{ P, POut }

import models.*
import ops.{ *, given }
import Parser.*

class DocumentParserTest extends munit.ScalaCheckSuite:

  test("some valid markup string") {
    val toParse = """*hello world*"""
    checkParser(
      toParse,
      Document(
        blocks = List(
          Paragraph(
            List(
              Emphasis(
                Emphasis.Marker.Italic("*"),
                List(Text("hello world"))
              )
            )
          )
        )
      )
    )
  }

  test("code block parsed") {
    val toParse = """- # heading
  - ```clojure
    (defn foo [x]
      (inc x))
    ```"""
    checkParser(
      toParse,
      Document(
        blocks = List(
          MarkdownList.Unordered(
            List(
              MarkdownList.Item(
                marker = MarkdownList.Item.Marker("-"),
                blocks = List(
                  HeadedSection(
                    h("heading", 1),
                    List(
                      MarkdownList.Unordered(
                        indentation = Some(Indentation(1, "  ")),
                        items = List(
                          MarkdownList.Item(
                            marker = MarkdownList.Item.Marker("-"),
                            blocks = List(
                              CodeBlock(
                                metadata = Some("clojure"),
                                content = "    (defn foo [x]\n      (inc x))\n    "
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
      )
    )
  }

  test("simple block - video parsed") {
    val toParse = """{{video https://youtube.com/123}}"""
    checkParser(
      toParse,
      Document(
        blocks = List(
          Paragraph(
            List(SimpleBlock.Video(Link.Location.External("https://youtube.com/123")))
          )
        )
      )
    )
  }

  test("simple block - query parsed") {
    val toParse = """{{query this is query}}"""
    checkParser(
      toParse,
      Document(
        blocks = List(Paragraph(List(SimpleBlock.Query("this is query"))))
      )
    )
  }

  test("labeled internal page link is parsed") {
    val toParse = "[text]([[page]])"
    checkParser(
      toParse,
      Document(
        blocks = List(
          Paragraph(elements =
            List(
              Link.Internal.Classic(
                Link.Location.Internal.Page("page"),
                label = Some("text")
              )
            )
          )
        )
      )
    )
  }

  test("labeled internal block link is parsed") {
    val toParse = "[label](((block-id)))"
    checkParser(
      toParse,
      Document(
        blocks = List(
          Paragraph(elements =
            List(
              Link.Internal.Classic(
                Link.Location.Internal.Block("block-id"),
                label = Some("label")
              )
            )
          )
        )
      )
    )
  }

  test("simple internal page link is parsed") {
    val toParse = "[[page]] [[page.dot]] [[pagedot.]]"
    checkParser(
      toParse,
      Document(
        blocks = List(
          Paragraph(elements =
            List(
              Link.Internal.Classic(Link.Location.Internal.Page("page"), label = None),
              Text(" "),
              Link.Internal.Classic(Link.Location.Internal.Page("page.dot"), label = None),
              Text(" "),
              Link.Internal.Classic(Link.Location.Internal.Page("pagedot."), label = None)
            ),
          )
        )
      )
    )
  }

  test("simple internal block link is parsed") {
    val toParse = "((block-id))"
    checkParser(
      toParse,
      Document(
        blocks = List(
          Paragraph(elements =
            List(
              Link.Internal.Classic(Link.Location.Internal.Block("block-id"), label = None)
            )
          )
        )
      )
    )
  }

  test("tag link is parsed") {
    val toParse = "#page #[[page2]] #[[page with space]] #.dottag #trailingdot1. #trailingdot2."
    checkParser(
      toParse,
      Document(
        blocks = List(
          Paragraph(elements =
            List(
              Link.Internal.Tag.WithoutBrackets(Link.Location.Internal.Page("page")),
              Text(" "),
              Link.Internal.Tag.WithBrackets(Link.Location.Internal.Page("page2")),
              Text(" "),
              Link.Internal.Tag.WithBrackets(Link.Location.Internal.Page("page with space")),
              Text(" "),
              Link.Internal.Tag.WithoutBrackets(Link.Location.Internal.Page(".dottag")),
              Text(" "),
              Link.Internal.Tag.WithoutBrackets(Link.Location.Internal.Page("trailingdot1")),
              Text(". "),
              Link.Internal.Tag.WithoutBrackets(Link.Location.Internal.Page("trailingdot2")),
              Text(".")
            )
          )
        )
      )
    )
  }

  test("labeled external link can be parsed") {
    val toParse = "[label](https://example.com)"
    checkParser(
      toParse,
      Document(
        blocks = List(
          Paragraph(elements =
            List(
              Link.External(Link.Location.External("https://example.com"), Some("label"))
            )
          )
        )
      )
    )
  }

  test("non labeled external link can be parsed") {
    val toParse = "https://example.com http://1.1.1.1 [label](https://example2.com)"
    checkParser(
      toParse,
      Document(
        blocks = List(
          Paragraph(elements =
            List(
              Link.External(Link.Location.External("https://example.com"), None),
              Text(" "),
              Link.External(Link.Location.External("http://1.1.1.1"), None),
              Text(" "),
              Link.External(Link.Location.External("https://example2.com"), Some("label"))
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
      toParse,
      Document(
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
      toParse,
      Document(
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
      toParse,
      Document(
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
      toParse,
      Document(
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
      toParse,
      Document(
        blocks = List(
          MarkdownList.Unordered(
            List(
              MarkdownList.Item(
                List(
                  Paragraph(List(Text("list item 1"))),
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

  test("just property drawer") {
    import PropertyDrawer.*
    import Node.*
    val toParse = "k1:: v1\nk2:: v2\nk3::\nk4:: #tag1, #tag2"
    checkParser(
      toParse,
      Document(
        blocks = List(),
        propertyDrawer = Some(
          PropertyDrawer(
            List(
              Node(Key("k1"), Value(List(Text("v1")))),
              Node(Key("k2"), Value(List(Text("v2")))),
              Node(Key("k3", spacingAfter = None), Value(List())),
              Node(
                Key("k4"),
                Value(
                  List(
                    Link.Internal.Tag.WithoutBrackets(Link.Location.Internal.Page("tag1")),
                    Text(", "),
                    Link.Internal.Tag.WithoutBrackets(Link.Location.Internal.Page("tag2"))
                  )
                ),
                indentation = None
              )
            )
          )
        )
      )
    )
  }

  test("lists with headings") {
    import PropertyDrawer.*
    import Node.*
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
      toParse,
      Document(
        blocks = List(
          MarkdownList.Unordered(
            items = List(
              MarkdownList.Item(
                blocks = List(
                  HeadedSection(heading = h("Heading 1", 1), blocks = List())
                ),
                marker = MarkdownList.Item.Marker("-")
              ),
              MarkdownList.Item(
                blocks = List(
                  HeadedSection(heading = h("Heading 2", 1), blocks = List())
                ),
                marker = MarkdownList.Item.Marker("-")
              ),
              MarkdownList.Item(
                blocks = List(
                  HeadedSection(
                    heading = Heading(
                      level = Heading.Level(value = 1),
                      elements = List(Text("todo heading")),
                      status = Some(Status("TODO", spacingAfter = Some(Spacing(" "))))
                    ),
                    blocks = List()
                  )
                ),
                marker = MarkdownList.Item.Marker("-")
              ),
              MarkdownList.Item(
                blocks = List(
                  HeadedSection(
                    heading = Heading(
                      level = Heading.Level(value = 1),
                      elements = List(Text("done heading with priority")),
                      status = Some(Status("DONE", spacingAfter = Some(Spacing(" ")))),
                      priority = Some(Priority('A', spacingAfter = Some(Spacing(" "))))
                    ),
                    blocks = List()
                  )
                ),
                marker = MarkdownList.Item.Marker("-")
              ),
              MarkdownList.Item(
                blocks = List(
                  HeadedSection(
                    Heading(
                      level = Heading.Level(value = 1),
                      elements = List(Text("doing heading with properties")),
                      status = Some(Status("DOING", spacingAfter = Some(Spacing(" ")))),
                      propertyDrawer = Some(
                        PropertyDrawer(
                          List(
                            Node(
                              Key("k1"),
                              Value(List(Text("v1"))),
                              indentation = Some(Indentation(1, "  "))
                            ),
                            Node(
                              Key("k2"),
                              Value(List(Text("v2"))),
                              indentation = Some(Indentation(1, "  "))
                            )
                          )
                        )
                      )
                    ),
                    blocks = List()
                  )
                ),
                marker = MarkdownList.Item.Marker("-")
              ),
              MarkdownList.Item(
                blocks = List(
                  HeadedSection(
                    heading = h("last heading", 1),
                    blocks = List(Paragraph(List(Text("and some text, just because"))))
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
      toParse,
      Document(
        blocks = List(
          HeadedSection(
            heading = h("Heading 1", 1),
            blocks = List(
              MarkdownList.Unordered(
                items = List(
                  listItem("list item 1", "+")
                )
              ),
              HeadedSection(
                heading = h("Heading 2.1", 2),
                blocks = List(
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
                blocks = List()
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
      toParse,
      Document(
        blocks = List(
          HeadedSection(
            heading = h("Heading 1", 1),
            blocks = List(
              MarkdownList.Unordered(
                List(
                  MarkdownList.Item(
                    blocks = List(
                      Paragraph(List(Text("list item 1"))),
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
                blocks = List(
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
                blocks = List(
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
                        blocks = List(
                          HeadedSection(
                            heading = h("list heading 2", 2),
                            blocks = List(
                              HeadedSection(
                                heading = h("list heading 2.1", 3),
                                blocks = List(
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
                                blocks = List(
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
                            indentation = None
                          )
                        ),
                        marker = MarkdownList.Item.Marker("-")
                      ),
                      MarkdownList.Item(
                        blocks = List(
                          HeadedSection(
                            heading = h("list heading 3", 3),
                            blocks = List()
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
    import PropertyDrawer.*
    import Node.*
    val toParse = """
|k1:: v1
|k2:: v2
|# Heading 1
|k3:: v3
|## Heading 2
|k4:: v4
|text""".trim().stripMargin
    checkParser(
      toParse,
      Document(
        blocks = List(
          HeadedSection(
            heading = h(
              "Heading 1",
              1,
              propertyDrawer = Some(
                PropertyDrawer(
                  List(Node(Key("k3"), Value(List(Text("v3")))))
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
                      List(Node(Key("k4"), Value(List(Text("v4")))))
                    )
                  )
                ),
                blocks = List(
                  Paragraph(List(Text("text")))
                )
              )
            )
          )
        ),
        propertyDrawer = Some(
          PropertyDrawer(
            List(
              Node(Key("k1"), Value(List(Text("v1")))),
              Node(Key("k2"), Value(List(Text("v2"))))
            )
          )
        )
      )
    )
  }

  test("Some more or less complex document is parsed correctly") {
    import PropertyDrawer.*
    import Node.*
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
      toParse,
      Document(
        propertyDrawer = Some(
          PropertyDrawer(
            List(
              Node(
                Key("type"),
                Value(List(classicInternalLink("Media/Movie"))),
                indentation = Some(Indentation(1, "  "))
              ),
              Node(
                Key("author"),
                Value(List(classicInternalLink("Кристофер Нолан"))),
                indentation = Some(Indentation(1, "  "))
              ),
              Node(
                Key("status"),
                Value(List(classicInternalLink("DONE"))),
                indentation = Some(Indentation(1, "  "))
              ),
              Node(
                Key("alias"),
                Value(List(Text("Oppenheimer"))),
                indentation = Some(Indentation(1, "  "))
              ),
              Node(
                Key("rating"),
                Value(List(Text("5"))),
                indentation = Some(Indentation(1, "  "))
              ),
              Node(
                Key("done-date"),
                Value(List(classicInternalLink("2023-07-29"))),
                indentation = Some(Indentation(1, "  "))
              )
            )
          )
        ),
        blocks = List(
          MarkdownList.Unordered(items =
            List(
              MarkdownList.Item(
                marker = MarkdownList.Item.Marker("-"),
                blocks = List(
                  Paragraph(
                    elements = List(
                      classicInternalLink("Oppenheimer"),
                      Text(" in "),
                      classicInternalLink("Cinema City")
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
                blocks = List(
                  HeadedSection(
                    heading = h("Cast", 1),
                    blocks = List(
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

  test("Logseq query block is parseable") {
    val toParse = """
|#+BEGIN_QUERY
|{:title "Movies with rating 5"
| :query [:find (pull ?b [*])
|         :where
|         [?b :movie/rating 5]]}
|#+END_QUERY""".strip().stripMargin
    checkParser(
      toParse,
      Document(
        blocks = List(
          BeginEndBlock.LogseqQuery(
            content =
              "\n{:title \"Movies with rating 5\"\n :query [:find (pull ?b [*])\n         :where\n         [?b :movie/rating 5]]}\n"
          )
        )
      )
    )
  }

  test("Custom valid begin-end block is parseable") {
    val toParse = """
|#+BEGIN_FOO
|#+BEGIN_kek
|foo bar baz
|#+END_kek
|#+END_FOO""".strip().stripMargin
    checkParser(
      toParse,
      Document(
        blocks = List(
          BeginEndBlock.Custom(
            content = "\n#+BEGIN_kek\nfoo bar baz\n#+END_kek\n",
            name = "FOO"
          )
        )
      )
    )
  }

  test("paragraph (no indentations) with property drawer and custom properties is parseable") {
    import PropertyDrawer.*
    import Node.*
    val toParse = """
|paragraph text
|k1:: v1
|k2:: v2
|:CUSTOM:
|here just a text
|:END:
""".trim().stripMargin
    checkParser(
      toParse,
      Document(
        blocks = List(
          Paragraph(
            elements = List(Text("paragraph text")),
            propertyDrawer = Some(
              PropertyDrawer(
                List(
                  Node(Key("k1"), Value(List(Text("v1")))),
                  Node(Key("k2"), Value(List(Text("v2"))))
                )
              )
            ),
            customProperties = List(
              CustomProperties(
                blockName = "CUSTOM",
                content = "\nhere just a text\n"
              )
            )
          )
        )
      )
    )
  }

  test("table - one row - is parseable") {
    import dev.vgerasimov.md4s.logseq.models.Table.*
    import dev.vgerasimov.md4s.logseq.models.Table.Row.*
    val toParse = """|header|header 1|"""
    checkParser(
      toParse,
      Document(blocks =
        List(
          Table(rows =
            List(
              Cells(
                List(
                  Cell(List(Text("header"))),
                  Cell(List(Text("header 1")))
                )
              )
            )
          )
        )
      )
    )
  }

  test("table - two rows - is parseable") {
    import dev.vgerasimov.md4s.logseq.models.Table.*
    import dev.vgerasimov.md4s.logseq.models.Table.Row.*
    val toParse = """|header|header 1|
|row 1|row 1|"""
    checkParser(
      toParse,
      Document(blocks =
        List(
          Table(rows =
            List(
              Cells(
                List(
                  Cell(List(Text("header"))),
                  Cell(List(Text("header 1")))
                )
              ),
              Cells(
                List(
                  Cell(List(Text("row 1"))),
                  Cell(List(Text("row 1")))
                )
              )
            )
          )
        )
      )
    )
  }

  test("list with table - two rows - incorrect indentation - is not correctly parseable") {
    import dev.vgerasimov.md4s.logseq.models.Table.*
    import dev.vgerasimov.md4s.logseq.models.Table.Row.*
    val toParse = """- |header|header 1|
|row 1|row 1|"""
    checkParser(
      toParse,
      Document(
        blocks = List(
          MarkdownList.Unordered(items =
            List(
              MarkdownList.Item(
                marker = MarkdownList.Item.Marker(value = "-"),
                blocks = List(
                  Table(rows =
                    List(
                      Cells(
                        List(
                          Cell(List(Text("header"))),
                          Cell(List(Text("header 1")))
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          Table(rows =
            List(
              Cells(
                List(
                  Cell(List(Text("row 1"))),
                  Cell(List(Text("row 1")))
                )
              )
            )
          )
        )
      )
    )
  }

  test("list with table - two rows - is parseable") {
    import dev.vgerasimov.md4s.logseq.models.Table.*
    import dev.vgerasimov.md4s.logseq.models.Table.Row.*
    val toParse = """- |header|header 1|
  |row 1|row 1|"""
    checkParser(
      toParse,
      Document(blocks =
        List(
          MarkdownList.Unordered(items =
            List(
              MarkdownList.Item(
                marker = MarkdownList.Item.Marker(value = "-"),
                blocks = List(
                  Table(rows =
                    List(
                      Cells(
                        List(
                          Cell(List(Text("header"))),
                          Cell(List(Text("header 1")))
                        )
                      ),
                      Cells(
                        List(
                          Cell(List(Text("row 1"))),
                          Cell(List(Text("row 1")))
                        ),
                        indentation = Some(Indentation(1, "  "))
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

  test("table - rows with elements - is parseable") {
    import dev.vgerasimov.md4s.logseq.models.Table.*
    import dev.vgerasimov.md4s.logseq.models.Table.Row.*
    val toParse = """|header|header 1|
|**row 1**|_row 1_|
|#tag|[link](http://example.com)|"""
    checkParser(
      toParse,
      Document(blocks =
        List(
          Table(rows =
            List(
              Cells(
                List(
                  Cell(List(Text("header"))),
                  Cell(List(Text("header 1")))
                )
              ),
              Cells(
                List(
                  Cell(
                    List(
                      Emphasis(Emphasis.Marker.Bold("**"), List(Text("row 1")))
                    )
                  ),
                  Cell(
                    List(
                      Emphasis(
                        Emphasis.Marker.Italic("_"),
                        List(Text("row 1"))
                      )
                    )
                  )
                )
              ),
              Cells(
                List(
                  Cell(
                    List(Link.Internal.Tag.WithoutBrackets(Link.Location.Internal.Page("tag")))
                  ),
                  Cell(
                    List(
                      Link.External(Link.Location.External("http://example.com"), Some("link"))
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

  test("table - header, separator, one row - is parseable") {
    import dev.vgerasimov.md4s.logseq.models.Table.*
    import dev.vgerasimov.md4s.logseq.models.Table.Row.*
    val toParse = """|header|header 1|
|--|--|
|row 1|row 1|"""
    checkParser(
      toParse,
      Document(blocks =
        List(
          Table(rows =
            List(
              Cells(
                List(
                  Cell(List(Text("header"))),
                  Cell(List(Text("header 1")))
                )
              ),
              Separator("|--|--|"),
              Cells(
                List(
                  Cell(List(Text("row 1"))),
                  Cell(List(Text("row 1")))
                )
              )
            )
          )
        )
      )
    )
  }

  lazy val ctx = Context.default()
  lazy val parser = new Parser(ctx)

  extension [T](r: POut[T])
    def isSuccess: Boolean = r match
      case _: POut.Success[?] => true
      case _: POut.Failure    => false

  export dev.vgerasimov.slowparse.POut

  def parse[T](toParse: String, parser: P[T]): POut[T] = parser(toParse)

  def checkParser[T](
    toParse: String,
    expected: => Document,
    parser: P[Document] = parser.document
  ): Unit =
    parse(toParse, parser) match {
      case POut.Success(value, _, _, _) =>
        // pprint.pprintln(value)
        assertEquals(value, expected)
      case POut.Failure(message, _) => fail(s"$toParse not parsed: $message")
    }

  def checkParserFailed[T](toParse: String, parser: P[T] = parser.document): Unit =
    parse(toParse, parser) match {
      case POut.Success(value, _, _, _) => fail(s"$toParse parsed to $value")
      case _                            =>
    }

  def ignore(v: String)(body: => Any): Unit = (
    ()
  )

end DocumentParserTest
