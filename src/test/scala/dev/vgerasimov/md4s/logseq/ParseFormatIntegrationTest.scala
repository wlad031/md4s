package dev.vgerasimov.md4s
package logseq

import models.*
import ops.{ *, given }

class ParseFormatIntegrationTest extends munit.ScalaCheckSuite {

  import dev.vgerasimov.md4s.logseq.Parser.*
  import dev.vgerasimov.slowparse.{ P, POut }

  lazy val ctx = Context.default()
  lazy val parser = new Parser(ctx)

  test("Text -> Parsing -> Formatting [1]") {
    val toParse = """
|  type:: [[Media/Movie]]
|  alias:: Thor: Ragnarok
|  status:: [[DONE]]
|  rating:: 3
|  done-date:: [[2017-11-09]],[[2024-03-08]]
|-
|- # Cast
""".trim().stripMargin
    parser.document(toParse) match
      case POut.Success(toFormat, _, _, _) =>
        val formatted = Formatter.format(toFormat)
        assertEquals(formatted, toParse)
      case POut.Failure(message, _) => fail(s"$toParse not parsed: $message")
  }

  test("Text -> Parsing -> Formatting [2]") {
    val toParse = """
- # heading
  - ```clojure
    (defn foo [x]
      (inc x))
    ```"""
    parser.document(toParse) match
      case POut.Success(toFormat, _, _, _) =>
        val formatted = Formatter.format(toFormat)
        assertEquals(formatted, toParse)
      case POut.Failure(message, _) => fail(s"$toParse not parsed: $message")
  }

  test("Text -> Parsing -> Formatting [3]") {
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
    parser.document(toParse) match
      case POut.Success(toFormat, _, _, _) =>
        val formatted = Formatter.format(toFormat)
        assertEquals(formatted, toParse)
      case POut.Failure(message, _) => fail(s"$toParse not parsed: $message")
  }

  test("Text -> Parsing -> Formatting [4]") {
    val toParse = """- #[[Andrew Huberman]]
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
		- improve lean mass"""
    parser.document(toParse) match
      case POut.Success(toFormat, _, _, _) =>
        val formatted = Formatter.format(toFormat)
        assertEquals(formatted, toParse)
      case POut.Failure(message, _) => fail(s"$toParse not parsed: $message")
  }

}
