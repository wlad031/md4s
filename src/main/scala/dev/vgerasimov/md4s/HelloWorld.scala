package dev.vgerasimov.template

import dev.vgerasimov.md4s.LogseqMarkdownParser.*
import dev.vgerasimov.md4s.LogseqMarkdownParser

object Hello:
  def apply(s: String): String = s"Hello $s!"

@main def run = 
  val parser = new LogseqMarkdownParser().document
  var parsed = parser(
"""- Started new [supplements]([[Supplement]])
  - Started [[CoQ10]] 60 mg daily
  - Started [[L-Carnitine]] again, 4000 mg daily
	- Started [[Berberine]] again, 400 mg daily
""")
  pprint.pprintln(parsed)