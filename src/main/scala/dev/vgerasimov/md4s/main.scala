package dev.vgerasimov.md4s

import dev.vgerasimov.slowparse.POut.Success
import dev.vgerasimov.slowparse.POut.Failure

import java.nio.file.*
import upickle.default.{ ReadWriter as RW, *, given }
import dev.vgerasimov.md4s.logseq.models.*
import scala.jdk.CollectionConverters.{ *, given }

val pprint2 =
    pprint.copy(
      // additionalHandlers = {
      //   // case logseq.models.Text(s) :: Nil => pprint.Tree.Literal(s)
      //   // case logseq.models.Text(s)        => pprint.Tree.Literal(s)
      // }
    )
val parser = logseq.parser().document

@main def run = parseAllLogseq

def parseAndPrint(text: String) =
  parser(text) match
    case Success(value, parsed, remaining, parserLabel) => None
      // println(write(value))
      // pprint2.pprintln(value)
      // println(logseq.raw.toRaw(value))
    case Failure(message, parserLabel) =>
      println(text)
      println(s"Failed to parse: $message")

def runSingleParsing =
  var toParse = """## hello
  |type:: foo
  |type2:: bar
  |adasddfsf
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
  // read An Introduction to Tracking Transactions with Ledger CLI.md as string
  toParse = scala.io.Source
    .fromFile(
      "/Users/vgerasimov/Logseq/pages/An Introduction to Tracking Transactions with Ledger CLI.md"
    )
    .mkString
  // toParse = """hello `code` world""".stripMargin
  parseAndPrint(toParse)

def parseAllLogseq =
  val currentTime = System.currentTimeMillis()
  Files.walk(Paths.get("/Users/vgerasimov/Logseq/pages")).iterator().asScala.filter(Files.isRegularFile(_))
  .map(f => scala.io.Source.fromFile(f.toFile).mkString)
  .foreach(f => {
    parseAndPrint(f)
  })
  println(s"Time: ${System.currentTimeMillis() - currentTime}ms")