package dev.vgerasimov.md4s

import dev.vgerasimov.md4s.LogseqMarkdownParser.*
import dev.vgerasimov.md4s.LogseqMarkdownParser

object Hello:
  def apply(s: String): String = s"Hello $s!"

@main def run =
  val parser = new LogseqMarkdownParser().document
  var parsed = parser("""type:: [[Media/Movie]]
  alias:: Uncharted
  status:: [[DONE]]
  rating:: 3
  done-date:: [[2023-11-12]]
-
- # Cast
	- [[Том Холланд]]
	- [[Марк Уолберг]]
	- [[Антонио Бандерас]]
""")
  pprint.pprintln(parsed)
