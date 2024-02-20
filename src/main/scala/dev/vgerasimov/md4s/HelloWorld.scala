package dev.vgerasimov.md4s

import dev.vgerasimov.md4s.LogseqMarkdownParser.*
import dev.vgerasimov.md4s.LogseqMarkdownParser

object Hello:
  def apply(s: String): String = s"Hello $s!"

@main def run =
  val parser = logseq.parser().document
  var toParse = """- a
  |  asdasdasd
  | 1. a1
  |  + bbb
  | 2. a2
  |- b
  |+ c
  |""".stripMargin
  toParse = """- #### Pi.Alert
  type:: [[Software]]
	- #[[Raspberry Pi]] #Monitoring #Selfhosting
	- https://github.com/jokob-sk/Pi.Alert
	- WIFI / LAN intruder detector. Scans for devices connected to your network and alerts you if new and unknown devices are found.
- #### Mealie
  type:: [[Software]]
	- #Cooking #Selfhosting
	- Mealie is a self hosted recipe manager and meal planner with a RestAPI backend and a reactive frontend application built in Vue for a pleasant user experience for the whole family. Easily add recipes into your database by providing the url and mealie will automatically import the relevant data or add a family recipe with the UI editor
	- https://github.com/mealie-recipes/mealie
- #### grocy
  type:: [[Software]]
	- #Selfhosting #Home
	- ERP beyond your fridge - Grocy is a web-based self-hosted groceries & household management solution for your home
	- https://github.com/grocy/grocy
- ### TODO How can you export the [[Visual Studio Code]] extension list?
  collapsed:: true
	- https://stackoverflow.com/questions/35773299/how-can-you-export-the-visual-studio-code-extension-list
"""
  var parsed = parser(toParse)
  pprint.pprintln(parsed)
