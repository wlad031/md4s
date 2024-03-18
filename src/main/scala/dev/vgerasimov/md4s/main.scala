package dev.vgerasimov.md4s

import dev.vgerasimov.slowparse.POut.Success
import dev.vgerasimov.slowparse.POut.Failure

import java.nio.file.*
import upickle.default.{ ReadWriter as RW, *, given }
import dev.vgerasimov.md4s.logseq.models.*
import dev.vgerasimov.md4s.logseq.formatter.*
import scala.jdk.CollectionConverters.{ *, given }

val pprint2 =
  pprint.copy(
    // additionalHandlers = {
    //   // case logseq.models.Text(s) :: Nil => pprint.Tree.Literal(s)
    //   // case logseq.models.Text(s)        => pprint.Tree.Literal(s)
    // }
  )
// val parser = logseq.parser().delayedDocument
val parser = logseq.parser().document

@main def run =
  // parseAllLogseq
  runSingleParsing

def parseAndPrint(text: String) =
  parser(text) match
    case Success(value, parsed, remaining, parserLabel) =>
      None
      // println(write(value))
      pprint2.pprintln(remaining)
    // pprint2.pprintln(value)
    // println(logseq.raw.toRaw(value))
    case Failure(message, parserLabel) =>
      println(text)
      println(s"Failed to parse: $message")

def runSingleParsing =
  var toParse = ""
  toParse = """- #[[Andrew Huberman]]
-
- https://twitter.com/nootropicguy/status/1637917960378408960
- ## SLEEP #Health/Sleep
	- Dr. Huberman has popularized the following 'sleep cocktail'
		- 30-60 minutes before sleep:
			- Apigenin - 50mg
			- Magtein - 145mg (of elemental mg)
			- [[L-Theanine]] - 100 to 400mg
			- These are relatively safe tools that turn off the brain via [[GABA]]
		- He also takes these every 3rd or 4th night:
			- Glycine - 2g
			- [[GABA]] - 100 mg
			- For those who wake up in the middle of the night, he recommends myo-inositol, which he takes for sleep in dosages of around 900mg
- ## FOCUS #[[Focus]]
	- [[L-Tyrosine]] is a precursor amino acid that increases dopamine production, which can improve focus
	  **Dosage:** 500mg (am)
	- Phenylalanine (PEA) - 300-600mg
	- [EPA]([[Omega-3/EPA]]) is a potent anti-inflammatory Omega-3 that can boost focus via modulating relevant neural circuits
	  **Dosage:** 1-3g
	- [[Alpha-GPC]] increases the synthesis & release of acetylcholine, which can help with memory, learning, focus, & other aspects of cogntion
	  **Dosage:** 300mg
	- Phosphatidylserine (PS)
	- B Vitamins
	- No dose was given for PS or B Vitamins, but I'd recommend 200mg for PS before bed & 1 daily serving Thorne's B complex if you want to try
	- PEA is also a dopamine precursor
- ## TESTOSTERONE #[[Health/Man]]
	- [[Tongkat ali]] is an Indonesian herb that upregulates enzymes in the steroid hormone cascade
	   **Dosage:** 300-1200mg
	- Fadogia - 300-600mg
	- [[Creatine]] helps with amino acid synthesis, oxidative stress, is a backup fuel tank for mitochondria, & slightly increases total testosterone & DHT
	  **Dosage:** 5g (or even 10)
	  If you don't respond to creatine , add betaine
	- [[L-Carnitine]] can upregulate androgen receptors and will be discussed further later in the thread due to its many benefits
	  **Dosage:** 1-5g
		- Note: carnitine should be taken w/ [[Garlic (Supplement)]] /allicin to offset TMAO
	- Boron - 5-12mg (elemental)
	- Fadogia agrestis is a Nigerian shrub that stimulates LH release and receptor sensitivity
	- Huberman recommends using caution with fadogia & doing routine bloodwork to monitor toxicity
	- Boron can help lower high SHBG and thus increase free testosterone
- ## FERTILITY #[[Health/Man]]
	- [[L-Carnitine]] improves sperm motility and egg quality when 1-5g is taken consistently for 30-60 days
	  **Dosage**: 1-3g daily
	- [[Garlic (Supplement)]] (especially its allicin content) can help offset the negative effects of TMAO from carnitine
	  **Dosage:** 600mg
	- [[Zinc]] can boost fertility in men by increasing testosterone and DHT levels
	  **Dosage**: 120mg (2x daily w/ meals)
	- Myo-inositol - 1-5g
	- Shilajit - 250mg (2x daily)
	- [[CoQ10]] supports mitochondrial health, essential for egg and sperm formation and fertilization, and should be taken at 100-400mg daily with a fat-containing meal
	  **Dosage:** 100-400mg (w/ fat)
	- [EPA]([[Omega-3/EPA]]) - 1-3g
	- [[Tongkat ali]] 
	  **Dosage:** 400mg
	- Acupuncture
	- Avoid sauna w/o ice pack
	- Myo-inositol improves insulin sensitivity and promotes healthier eggs and sperm, while also aiding sleep when taken at night
	- Women should consider d-chiro inositol too, to balance androgens (in 1/25 of the myo dose)
	- Shilajit enhances fertility by increasing FSH & testosterone
- ## MOOD & DEPRESSION #Mood
	- Keto
	- [EPA]([[Omega-3/EPA]])'s anti-inflammatory properties not only benefit focus, but also mood as well
	  **Dosage:** 1-3g
	- [[Creatine]] may also benefit mood as it is involved in the phosphocreatine & NMDA systems in the brain
- ## STRESS & ANXIETY  #[[Health/Stress Management]]
	- [[L-Theanine]] **-** 200mg
	- [[Ashwagandha]] - 2x 300mg (pm)
	- Both compounds can take the edge off via [GABAergic]([[GABA]]) mechanisms, but Ashwagandha has a much more potent effect on reducing cortisol. Use caution w/ ashwagandha.
	- To reduce stress-induced inflammation, two medicinal mushrooms can also be helpful:
		- [[Lion's Mane]] - 1g
		- Chaga - 500mg
- ## FAT LOSS #[[Health/Fat Loss]]
	- [[Yerba Mate]] - 16-30z drink
	- Guayusa
	- Semaglutide
	- [[Berberine]] /Metformin reduces blood glucose and thus lowers insulin levels and increases fat oxidation
	  **Dosage:** 0.5-1.5g
	- ALCAR - 0.5-2g in split doses
	- [[Caffeine]] increases epinephrine release, enhancing fat oxidation and exercise performance. Don't overdo it if sensitive to caffeine.
	  **Dosage:** 100-400mg before exercise
	- [[Yerba Mate]] & Guayusa have [[Caffeine]] but also increase GLP-1, which facilitates fat oxidation. Guayusa is a bit sweeter.
	- Similarly, Semaglutide is a GLP-1 agonist but is much more powerful. Use only with a doctor.
	- [[L-Carnitine]] / ALCAR facilitate fat oxidation and helps converts fatty acids into ATP
- ## MUSCLE GROWTH & WORKOUT #Fitness #Fitness/Bodybuilding
	- [[Creatine]] - 5g (10g if heavier)
	- [[Rhodiola Rosea]] reduces the perceived threshold of how hard you're working so that you can work harder
	  **Dosage:** 200mg
		- This means that you can generate the same amount of effort without the same amount of energy-depleting neurochemicals
	- Salt & electrolytes
		- Increasing salt intake brings more water into the muscle to
			- generate more force
			- increase lean mass
	- [[Beta-alanine]] is an amino that has been shown to
	  **Dosage:** 2-5g
		- improve muscular endurance
		- improve anaerobic running capacity
		- reduce fatigue
		- reduce body fat
		- improve lean mass
"""
  // read An Introduction to Tracking Transactions with Ledger CLI.md as string
  // toParse = scala.io.Source
  //   .fromFile(
  //     "/Users/vgerasimov/Logseq/pages/An Introduction to Tracking Transactions with Ledger CLI.md"
  //   )
  //   .mkString
  // toParse = """hello `code` world""".stripMargin
  parseAndPrint(toParse)

def parseAllLogseq =
  var times = List[(Path, Long)]()
  val currentTime = System.currentTimeMillis()
  var i = 0
  Files
    .walk(Paths.get("/Users/vgerasimov/Logseq/pages"))
    .iterator()
    .asScala
    .filter(Files.isRegularFile(_))
    .foreach(f => {
      val text = scala.io.Source.fromFile(f.toFile).mkString
      val currentTime = System.currentTimeMillis()
      val parsed = parser(text)
      times = times :+ (f, System.currentTimeMillis() - currentTime)
      parsed match
        case Success(value, parsed, remaining, parserLabel) => None
        // case Success((None, _), _, _, _) => None
        // case Success((Some(propertyDrawer), next), parsed, remaining, parserLabel) =>
        //   if (propertyDrawer.nodes.exists(n =>
        //       n.name == "type" && n.value.isDefined && n.value.get.elements.head
        //         .isInstanceOf[ClassicInternalLink] && n.value.get.elements.head
        //         .asInstanceOf[ClassicInternalLink]
        //         .location
        //         .isInstanceOf[Link.Location.Internal.Page]
        //       && n.value.get.elements.head
        //         .asInstanceOf[ClassicInternalLink]
        //         .location
        //         .asInstanceOf[Link.Location.Internal.Page]
        //         .value == "Media/Movie"
        //     ))
        //       next() match
        //         case Success(LogseqMarkdown(None, _), _, _, _) =>
        //           println(s"[ERROR] Something went wrong: $f")
        //         case Success(LogseqMarkdown(Some(propertyDrawer), blocks), _, _, _) =>
        //           if (propertyDrawer.nodes.exists(n =>
        //               n.name == "gid" && n.value.isDefined)) {
        //                 println(s"[INFO] Page already has gid: $f")
        //               } else {
        //                 val gid = java.util.UUID.randomUUID().toString
        //                 val newPropertyDrawer = PropertyDrawer(
        //                   propertyDrawer.nodes :+ PropertyDrawer.Node(
        //                     "gid",
        //                     Some(InlineContainer(List(Text(gid))))
        //                   )
        //                 )
        //                 val newDoc = LogseqMarkdown(blocks = blocks, propertyDrawer = Some(newPropertyDrawer))
        //                 val newDocText = formatter.format(newDoc)
        //                 val writer = new java.io.PrintWriter(f.toFile())
        //                 writer.write(newDocText)
        //                 writer.close()
        //                 println(s"[INFO] Added gid to: $f")
        //               }
        //             i = i + 1
        //           val newDoc = LogseqMarkdown(blocks = blocks, propertyDrawer = Some(propertyDrawer))
        //         case Failure(message, _) =>
        //           println(s"[ERROR] Failed to parse: $f")
        case Failure(message, _) =>
          println(s"[ERROR] Failed to parse: $f")
    })
  println(s"Found $i movies")
  println(s"Average time: ${times.map(_._2).sum / times.length}ms")
  println(s"Max time: ${times.max(ord = (l, r) => l._2.compare(r._2))}ms")
  println(s"Time: ${System.currentTimeMillis() - currentTime}ms")
