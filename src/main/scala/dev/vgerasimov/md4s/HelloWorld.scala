package dev.vgerasimov.md4s

import dev.vgerasimov.md4s.LogseqMarkdownParser.*
import dev.vgerasimov.md4s.LogseqMarkdownParser

object Hello:
  def apply(s: String): String = s"Hello $s!"

@main def run =
  val parser = logseq.parser().document
  var parsed = parser("""## TODO hello
  |paragraph
  |next
  |
  |### one
  |more
  |
  |#### ccccc
  |
  |### two
  |more
  |""".stripMargin)
  pprint.pprintln(parsed)
