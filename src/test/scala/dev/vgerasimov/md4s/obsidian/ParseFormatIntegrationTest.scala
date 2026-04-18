package dev.vgerasimov.md4s
package obsidian

class ParseFormatIntegrationTest extends munit.ScalaCheckSuite {

  import dev.vgerasimov.md4s.obsidian.Parser.*
  import dev.vgerasimov.slowparse.{ POut }

  lazy val ctx = Context.default()
  lazy val parser = new Parser(ctx)
  lazy val formatter = Formatter()

  test("Text -> Parsing -> Formatting [obsidian core]") {
    val toParse = """---
title: Obsidian Note
tags: project, parser
---
## Links
[[Page]] [site](https://example.com)
> [!note] Remember
> use parser
- [ ] todo
- [x] done
|h1|h2|
|--|--|
|v1|v2|
Footnote[^id]
[^id]: value
%%
secret
%%"""

    parser.document(toParse) match
      case POut.Success(toFormat, _, _, _) =>
        val formatted = formatter.format(toFormat)
        assertEquals(formatted, toParse)
      case POut.Failure(message, _) => fail(s"$toParse not parsed: $message")
  }

  test("Text -> Parsing -> Formatting [math untouched]") {
    val toParse = "price is $x + y$"

    parser.document(toParse) match
      case POut.Success(toFormat, _, _, _) =>
        val formatted = formatter.format(toFormat)
        assertEquals(formatted, toParse)
      case POut.Failure(message, _) => fail(s"$toParse not parsed: $message")
  }

  test("Text -> Parsing -> Formatting [big all elements]") {
    val toParse = """---
title: Mega Note
tags: parser, obsidian
---
# H1
Intro with **bold** _italic_ ~~strike~~ ==highlight== `code` %%inline%%
Link [site](https://example.com) and bare https://example.org/x and image ![logo](https://cdn/logo.png "brand")
Wiki [[Page]] [[Page#Section|Alias]] ![[assets/image.png]] [[#HeadingRef]] [[Page#^block-id]] [[#^loose-block]]
Tag #tag/one and footnote ref[^f1]
## H2
- [ ] task one
  continuation line
  - nested bullet
    - nested deep bullet
  1. nested ordered
  2. nested ordered two
- # List heading
  ### Child heading
  paragraph under child heading
> [!tip]+ Callout title
> callout body line
### After callout
> quote line 1
> quote line 2 with [q](https://q.example)
> - quote list item
> - quote list item two
```scala
val x = 1
println(x)
```
|c1|c2|
|--|--|
|v1|[[Cell]]|
---
[^f1]: Footnote value with #tag/two
%%
hidden
multi line
%%"""

    parser.document(toParse) match
      case POut.Success(toFormat, _, _, _) =>
        val formatted = formatter.format(toFormat)
        assertEquals(formatted, toParse)
      case POut.Failure(message, _) => fail(s"$toParse not parsed: $message")
  }

}
