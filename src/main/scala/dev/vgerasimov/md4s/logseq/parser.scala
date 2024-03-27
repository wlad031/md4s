package dev.vgerasimov.md4s
package logseq

import models.*
import ops.{ *, given }

import dev.vgerasimov.slowparse.*
import dev.vgerasimov.slowparse.Parsers.{ *, given }

object parser:

  /** Configuration of a Logseq Markdown document. */
  case class Context(
    statusKeywords: Set[String] = Context.default.statusKeywords,
    listMaxLevel: Int = Context.default.listMaxLevel,
    headingMinLevel: Int = Context.default.headingMinLevel,
    headingMaxLevel: Int = Context.default.headingMaxLevel,
    commaSeparatedNodeProperties: Set[String] = Context.default.commaSeparatedNodeProperties,
    aliasNodeProperty: String = Context.default.aliasNodeProperty,
    knownLinkProtocols: Set[String] = Context.default.knownLinkProtocols
  )

  object Context:

    /** Contains default values for all fields of the [[Context]]. */
    object default:
      val statusKeywords: Set[String] =
        Set("TODO", "DOING", "DONE", "LATER")
      val listMaxLevel: Int =
        20
      val headingMinLevel: Int =
        1
      val headingMaxLevel: Int =
        6
      val commaSeparatedNodeProperties: Set[String] =
        Set("tags", "file", "alias", "id", "created", "modified")
      val aliasNodeProperty: String =
        "alias"
      val knownLinkProtocols: Set[String] =
        // If http is first, it will be matched before https, and then
        // the entire parser will fail. So, the order is important.
        Set("https", "http", "mailto")

      private lazy val defaultCtx: Context = Context()
      def apply(): Context = defaultCtx

  end Context

  private enum ListType:
    case Unordered, Ordered

  private case class IntentationSymbol(value: String)

  private def maybeIndentation(indentation: Indentation): Option[Indentation] = indentation match
    case Indentation(0, _)  => None
    case Indentation(_, "") => None
    case x                  => Some(x)

import parser.*

class parser(ctx: Context = Context.default()):

  def document: P[LogseqMarkdown] = evalAndLazyThen(delayedDocument)

  def delayedDocument: AndLazyThen[Option[PropertyDrawer], LogseqMarkdown] =
    mapAndLazyThen(andLazyThen(propertyDrawer.?, block(minIndentation = 0).*)) {
      case (properties, blocks) => LogseqMarkdown(blocks = blocks, propertyDrawer = properties)
    }

  private def spacing: P[Spacing] = (tab | space).+.!.map(Spacing.apply)
  private def doubleSpace: P[String] = (space ~ space).!

  private def indentation(min: Int = 0, max: Int = Int.MaxValue): P[Indentation] =
    val c: P[String] = (tab | doubleSpace).!
    (c.map(IntentationSymbol.apply).rep(min = min, max = max) ~ !c)
      .map(sym => Indentation(sym.size, sym.map(_.value).mkString))

  private def elementsContainerWithoutEmphasis: P[ElementsContainer] =
    choice(simpleBlock, timestamp, link, !eol ~ singleCharText).+.map(_.toList)
      .map(foldTexts[Element])
      .map(ElementsContainer.apply)

  private def elementsContainer: P[ElementsContainer] =
    (choice(simpleBlock, timestamp, link, emphasis, !eol ~ singleCharText).* ~ eolOrEnd)
      .map(_.toList)
      .map(foldTexts[Element])
      .map(ElementsContainer.apply)

  private def headedSection(
    headingMinLevel: Int,
    headingMaxLevel: Int,
    minIndentation: Int,
    listMinLevel: Int
  ): P[HeadedSection] =
    if (headingMinLevel > ctx.headingMaxLevel || headingMinLevel > headingMaxLevel)
      fail[HeadedSection]
    else
      &(indentation(min = minIndentation) ~ heading(headingMinLevel, headingMaxLevel)).flatMap {
        case (preIndentation, preHeading) =>
          (
            indentation(min = minIndentation)
              ~ heading(headingMinLevel, headingMaxLevel)
              ~ block(
                listMinLevel = listMinLevel,
                headingMinLevel = preHeading.level.value + 1,
                headingMaxLevel = headingMaxLevel,
                minIndentation = minIndentation
              ).*
          ).map { case (indentation, heading, content) =>
            HeadedSection(
              heading = heading,
              content = content,
              indentation = maybeIndentation(indentation)
            )
          }
      }

  private def heading(headingMinLevel: Int, headingMaxLevel: Int): P[Heading] =
    if (headingMinLevel > ctx.headingMaxLevel || headingMinLevel > headingMaxLevel) fail[Heading]
    else
      def level: P[Heading.Level] =
        def chars: P[Int] =
          P("#").rep(min = headingMinLevel, max = headingMaxLevel, greedy = true).!.map(_.length)
        (chars ~ spacing).map { case (value, spacing) =>
          Heading.Level(value = value, spacingAfter = spacing)
        }

      (!listMarker ~ level ~ (priority ~ s0).? ~ (status ~ s0).? ~ elementsContainer.? ~ propertyDrawer.?)
        .map {
          case (
                level: Heading.Level,
                priority: Option[Priority],
                status: Option[Status],
                content: Option[ElementsContainer],
                drawer: Option[PropertyDrawer]
              ) =>
            Heading(
              content = content,
              level = level,
              status = status,
              priority = priority,
              propertyDrawer = drawer
            )
        }

  private def priority: P[Priority] =
    (((P("[#") ~ fromRange("A-Z").! ~ P("]"))
      .map(s => s.toList.head)) ~ spacing.?).map { case (v, s) => Priority(v, s) }

  private def status: P[Status] =
    (ctx.statusKeywords.map(kw => P(kw)).reduce(_ | _).! ~ spacing.?).map { case (v, s) =>
      Status(v, s)
    }

  private def propertyDrawer: P[PropertyDrawer] = {
    import PropertyDrawer.*

    def node: P[Node] = {
      import Node.*

      def key: P[Key] =
        (until(P("::") | eol).! ~ P("::") ~ spacing.?)
          .map { case (value, maybeSpacing) => Key(value, maybeSpacing) }
      def value: P[Value] = elementsContainer.map(Value.apply)

      // There is no ~ eolOrEnd becase value uses elementsContainer and it already has eolOrEnd
      (indentation(min = 0) ~ key ~ value)
        .map { case (indentation, key, value) =>
          Node(key = key, value = value, indentation = maybeIndentation(indentation))
        }
    }

    node.+.map(_.toList).map(PropertyDrawer.apply)
  }

  private def customProperties: P[CustomProperties] =
    def endBlock: P[Unit] = P(":END:")
    ((P(":") ~ !endBlock ~ alpha.+.! ~ P(":")) ~ (!endBlock ~ anyChar).*.! ~ endBlock).map {
      case (name, value) => CustomProperties(name, value)
    }

  private def orderedListMarker: P[String] = (d.+ ~ P(".")).!
  private def listMarker: P[MarkdownList.Item.Marker] =
    ((anyFrom("-+").! | orderedListMarker) ~ spacing).map { case (v, s) =>
      MarkdownList.Item.Marker(v, s)
    }

  private def list(
    listMinLevel: Int,
    listMaxLevel: Int,
    minIndentation: Int
  ): P[MarkdownList] =
    if (listMinLevel > ctx.listMaxLevel || listMinLevel > listMaxLevel)
      fail[MarkdownList]
    else
      &(indentation(min = listMinLevel) ~ listMarker).flatMap { case (preIndentation, preMarker) =>
        (indentation(min = listMinLevel).!!
          ~ listMarker.!!
          ~ (
            block(minIndentation = 0, listMinLevel = listMinLevel + 1).?
              ~ block(
                minIndentation = preIndentation.level + 1,
                listMinLevel = listMinLevel + 1
              ).*
          ).map {
            case (Some(first), next) =>
              MarkdownList.Item(first :: next, preMarker)
            case (None, next) => MarkdownList.Item(next, preMarker)
          })
          .rep(min = 1)
          .map { items =>
            MarkdownList.Unordered(items = items, indentation = maybeIndentation(preIndentation))
          }
      }

  // TODO: Make !_conditions better
  private def paragraph(minIndentation: Int): P[Paragraph] =
    !((s0 ~ listMarker) | (s0 ~ P("#").+ ~ s1))
      ~ (indentation(min =
        minIndentation
      ) ~ (priority ~ s0).? ~ (status ~ s0).? ~ elementsContainer ~ propertyDrawer.? ~ customProperties.* ~ planning.*)
        .map { case (ind, priority, status, content, drawer, customProperties, planning) =>
          Paragraph(
            content = content,
            propertyDrawer = drawer,
            customProperties = customProperties,
            indentation = Some(ind),
            planning = planning,
            priority = priority,
            status = status
          )
        }

  private def codeBlock: P[CodeBlock] =
    def surrounder = P("```")
    (indentation() ~ surrounder ~ until(eol).!.? ~ eol ~ until(surrounder).! ~ surrounder ~ eolOrEnd)
      .map {
        case (indentation, metadata, content) =>
          CodeBlock(content, metadata, indentation = maybeIndentation(indentation))
      }

  private def beginEndBlock: P[BeginEndBlock] =
    def beginBlock: P[String] = P("#+BEGIN_") ~ alpha.+.!.map(_.mkString)
    def endBlock(name: String): P[String] = P("#+END_") ~ P(name).!
    (indentation() ~ beginBlock.flatMap { name =>
      (!endBlock(name) ~ anyChar).*.! ~ endBlock(name)
    }).map {
      case (indentation, (content, "QUERY")) =>
        BeginEndBlock.LogseqQuery(content = content, indentation = maybeIndentation(indentation))
      case (indentation, (content, name)) =>
        BeginEndBlock.Custom(
          content = content,
          name = name,
          indentation = maybeIndentation(indentation)
        )
    }

  private def block(
    minIndentation: Int,
    headingMinLevel: Int = 1,
    headingMaxLevel: Int = 6,
    listMinLevel: Int = 0,
    listMaxLevel: Int = ctx.listMaxLevel
  ): P[Block] =
    if (minIndentation > 16) fail[Block]
    else
      choice(
        list(listMinLevel, listMaxLevel, minIndentation),
        headedSection(headingMinLevel, headingMaxLevel, minIndentation, listMinLevel),
        codeBlock,
        beginEndBlock,
        table(minIndentation = minIndentation, currentListIndentation = listMinLevel),
        paragraph(minIndentation = minIndentation)
      )

  private def linkLocationExternal: P[Link.Location.External] =
    def protocol: P[String] = ctx.knownLinkProtocols.map(p => P(p)).reduce(_ | _).!
    def stop = anyFrom("()[]|{}") | ws | eolOrEnd
    (protocol ~ P("://") ~ until(stop).! ~ &(stop))
      .map { case (p, l) => s"$p://$l" }
      .map(Link.Location.External.apply)

  private def link: P[Link] = {
    import Link.*

    def label: P[String] = P("[") ~ !(P("[") | P("]")) ~ until(P("]")).! ~ P("]")

    def internal: P[Internal] = {
      import Internal.*

      def tag: P[Tag] = {
        import Tag.*

        def withBrackets: P[WithBrackets] =
          (P("#[[") ~ !(P("[") | P("]")) ~ until(P("]]")).! ~ P("]]"))
            .map(Location.Internal.Page.apply)
            .map(WithBrackets.apply)

        def withoutBrackets: P[WithoutBrackets] = {
          def collector: P[String] = (alphaNum | (P(".").+ ~ alphaNum)).!
          def stop = !collector

          (P("#") ~ !stop ~ until(stop, collector = collector).! ~ &(stop))
            .map(Location.Internal.Page.apply)
            .map(WithoutBrackets.apply)
        }

        withBrackets | withoutBrackets
      }

      def location: P[Location.Internal] = {
        import Location.Internal.*

        def page: P[Page] =
          (P("[[") ~ !(P("[") | P("]")) ~ until(P("]]")).! ~ P("]]")).map(Page.apply)
        def block: P[Block] =
          (P("((") ~ !(P("(") | P(")")) ~ until(P("))")).! ~ P("))")).map(Block.apply)

        page | block
      }

      def withLabel: P[Internal] = {
        (label ~ P("(") ~ location ~ P(")")).map { case (label, location) =>
          Internal.Classic(location, Some(label))
        }
      }
      def withoutLabel: P[Internal] = location.map(Internal.Classic(_, None))

      tag | withoutLabel | withLabel
    }

    def external: P[External] = {
      def withoutLabel: P[External] = linkLocationExternal.map(External(_, label = None))
      def withLabel: P[External] =
        (label ~ P("(") ~ linkLocationExternal ~ P(")")).map { case (label, location) =>
          External(location, label = Some(label))
        }

      withLabel | withoutLabel
    }

    internal | external
  }

  private def simpleBlock: P[SimpleBlock] = {
    def query: P[SimpleBlock.Query] =
      (P("{{query ") ~ until(P("}}")).! ~ P("}}")).map(SimpleBlock.Query.apply)
    def video: P[SimpleBlock.Video] =
      (P("{{video ") ~ linkLocationExternal ~ P("}}")).map(SimpleBlock.Video.apply)
    video | query
  }

  // TODO: Refactor
  private def planning: P[Planning] =
    import Planning.*

    def section(name: String) =
      spacing.? ~ P(name) ~ P(":") ~ spacing.? ~ timestamp ~ spacing.? ~ eolOrEnd

    def closed: P[Closed] =
      section("CLOSED").map {
        case (spacingBeforeKeyword, spacingBeforeTimestamp, timestamp, spacingAfterTimestamp) =>
          Closed(
            timestamp,
            spacingBeforeKeyword = spacingBeforeKeyword,
            spacingBeforeTimestamp = spacingBeforeTimestamp,
            spacingAfterTimestamp = spacingAfterTimestamp
          )
      }

    def deadline: P[Deadline] =
      section("DEADLINE").map {
        case (spacingBeforeKeyword, spacingBeforeTimestamp, timestamp, spacingAfterTimestamp) =>
          Deadline(
            timestamp,
            spacingBeforeKeyword = spacingBeforeKeyword,
            spacingBeforeTimestamp = spacingBeforeTimestamp,
            spacingAfterTimestamp = spacingAfterTimestamp
          )
      }

    def scheduled: P[Scheduled] =
      section("SCHEDULED").map {
        case (spacingBeforeKeyword, spacingBeforeTimestamp, timestamp, spacingAfterTimestamp) =>
          Scheduled(
            timestamp,
            spacingBeforeKeyword = spacingBeforeKeyword,
            spacingBeforeTimestamp = spacingBeforeTimestamp,
            spacingAfterTimestamp = spacingAfterTimestamp
          )
      }

    closed | deadline | scheduled

  private def table(
    minIndentation: Int = 0,
    maxIndentation: Int = Int.MaxValue,
    currentListIndentation: Int = 0
  ): P[Table] = {
    import Table.*

    def row(minIndentation: Int = 0, maxIndentation: Int = Int.MaxValue): P[Row] = {
      import Row.*

      def separator(minIndentation: Int = 0, maxIndentation: Int = Int.MaxValue): P[Separator] =
        (
          indentation(min = minIndentation, max = maxIndentation)
            ~ (P("|-") ~ (P("|") | P("-")).*).!
        )
          .map { case (indentation, value) =>
            Separator(value = value, indentation = maybeIndentation(indentation))
          }

      def cells(minIndentation: Int = 0, maxIndentation: Int = Int.MaxValue): P[Cells] = {
        def cell: P[Cell] = {
          def elementsContainer: P[ElementsContainer] =
            choice(timestamp, link, emphasis, !(P("|") | eol) ~ singleCharText).*.map(_.toList)
              .map(foldTexts[Element])
              .map(ElementsContainer.apply)
          (elementsContainer ~ &(P("|"))).map(Cell.apply)
        }

        (indentation(min = minIndentation, max = maxIndentation) ~ (P("|") ~ (cell ~ P("|")).*))
          .map { case (indentation, cells) =>
            Cells(cells = cells, indentation = maybeIndentation(indentation))
          }
      }

      (
        separator(minIndentation = minIndentation, maxIndentation = maxIndentation)
          | cells(minIndentation = minIndentation, maxIndentation = maxIndentation)
      )
      ~ eolOrEnd
    }

    (
      row(minIndentation = minIndentation, maxIndentation = maxIndentation)
        ~ row(minIndentation = minIndentation + currentListIndentation).*
    )
      .map { case (first, next) =>
        Table(rows = first :: next)
      }
  }

  private def timestamp: P[Timestamp] = {
    import models.Timestamp.*
    import models.Timestamp.Date.*
    import models.Timestamp.Date.DayName.*
    import models.Timestamp.RepeaterOrDelay.*

    def time: P[Time] = {
      import models.Timestamp.Time.*

      def minute: P[Minute] =
        digit
          .rep(min = 2, max = 2)
          .!
          .map(_.toInt)
          // .filter(Minute.isdefid)
          .map(Minute.apply)

      def hour: P[Hour] =
        digit
          .rep(min = 1, max = 2)
          .!
          .map(_.toInt)
          // .filter(Hour.isdefid)
          .map(Hour.apply)

      (hour ~ P(":") ~ minute).map { case (h, m) => Time(h, m) }
    }

    def date: P[Date] = {

      def year: P[Year] =
        digit
          .rep(min = 4, max = 4)
          .!
          .map(_.toInt)
          // .filter(Year.isdefid)
          .map(Year.apply)

      def month: P[Month] =
        digit
          .rep(min = 2, max = 2)
          .!
          .map(_.toInt)
          // .filter(Month.isdefid)
          .map(Month.apply)

      def day: P[Day] =
        digit
          .rep(min = 2, max = 2)
          .!
          .map(_.toInt)
          // .filter(Day.isdefid)
          .map(Day.apply)

      def dayName: P[DayName] =
        charsUntilIn("+-]> \t\n\r0123456789").!.map {
          case x if Set("Mon") contains x => Some(Monday)
          case x if Set("Tue") contains x => Some(Tuesday)
          case x if Set("Wed") contains x => Some(Wednesday)
          case x if Set("Thu") contains x => Some(Thursday)
          case x if Set("Fri") contains x => Some(Friday)
          case x if Set("Sat") contains x => Some(Saturday)
          case x if Set("Sun") contains x => Some(Sunday)
          case _                          => None
        }
          .filter(_.isDefined)
          .map(_.get)

      (year ~ P("-") ~ month ~ P("-") ~ day ~ (P(" ") ~ dayName).?)
        // .filter {
        //   case (year, month, day, _) =>
        //     Date.isdefid(year, month, day)
        // }
        .map { case (year, month, day, dayName) =>
          Date(year, month, day, dayName)
        }
    }

    def diary: P[Diary] =
      (P("<%%(") ~ charsUntilIn("\n>") ~ P(")>")).map(Diary.apply)

    def repeaterMark: P[(RepeaterOrDelay.Value, RepeaterOrDelay.Unit) => RepeaterOrDelay] =
      (
        P("++").map(_ => RepeaterOrDelay.CatchUpRepeater.apply)
          | P("+").map(_ => RepeaterOrDelay.CumulateRepeater.apply)
          | P(".+").map(_ => RepeaterOrDelay.RestartRepeater.apply)
          | P("--").map(_ => RepeaterOrDelay.FirstTypeDelay.apply)
          | P("-").map(_ => RepeaterOrDelay.AllTypeDelay.apply)
      )

    def repeatervalue: P[RepeaterOrDelay.Value] =
      d
        .rep(1)
        .!
        .map(_.toInt)
        .map(RepeaterOrDelay.Value.apply)

    def repeaterUnit: P[RepeaterOrDelay.Unit] =
      anyFrom("hdwmy").!.map {
        case x if Set("h").contains(x) => Some(Unit.Hour)
        case x if Set("d").contains(x) => Some(Unit.Day)
        case x if Set("w").contains(x) => Some(Unit.Week)
        case x if Set("m").contains(x) => Some(Unit.Month)
        case x if Set("y").contains(x) => Some(Unit.Year)
        case _                         => None
      }
        .filter(_.isDefined)
        .map(_.get)

    def repeaterOrDelay: P[RepeaterOrDelay] =
      (repeaterMark ~ repeatervalue ~ repeaterUnit).map { case (factory, value, unit) =>
        factory(value, unit)
      }

    def activeTimestamp: P[ActiveTimestamp] =
      (P("<") ~ date ~ (s ~ time).? ~ (s ~ repeaterOrDelay).? ~ P(
        ">"
      )).map { case (d, t, r) =>
        ActiveTimestamp(d, t, r)
      }

    def inactiveTimestamp: P[InactiveTimestamp] =
      (P("[") ~ date ~ (s ~ time).? ~ (s ~ repeaterOrDelay).? ~ P(
        "]"
      )).map { case (d, t, r) =>
        InactiveTimestamp(d, t, r)
      }

    def activeTimestampRange: P[ActiveTimestampRange] =
      (
        (activeTimestamp ~ P("-")
          .rep(min = 1, max = 3)
          .!! ~ activeTimestamp).map { case (from: ActiveTimestamp, to: ActiveTimestamp) =>
          ActiveTimestampRange(from, to)
        }
          | P(
            P(
              "<"
            ) ~ date ~ s ~ time ~ s ~ time ~ (s ~ repeaterOrDelay).? ~ P(
              ">"
            )
          ).map { case (d, t1, t2, r) =>
            ActiveTimestampRange(
              ActiveTimestamp(d, Some(t1), r),
              ActiveTimestamp(d, Some(t2), r)
            )
          }
      )

    def inactiveTimestampRange: P[InactiveTimestampRange] =
      (
        (inactiveTimestamp ~ P("-")
          .rep(min = 1, max = 3)
          .!! ~ inactiveTimestamp).map { case (from: InactiveTimestamp, to: InactiveTimestamp) =>
          InactiveTimestampRange(from, to)
        }
          |
            (P(
              "["
            ) ~ date ~ s ~ time ~ s ~ time ~ (s ~ repeaterOrDelay).? ~ P(
              "]"
            )).map { case (d, t1, t2, r) =>
              InactiveTimestampRange(
                InactiveTimestamp(d, Some(t1), r),
                InactiveTimestamp(d, Some(t2), r)
              )
            }
      )

    (
      diary
      | activeTimestampRange
      | activeTimestamp
      | inactiveTimestampRange
      | inactiveTimestamp
    )
  }

  private def emphasis: P[Emphasis] =
    def nonNestable(
      markerParser: P[?],
      markerFirstCharParser: P[?],
      marker: Emphasis.Marker
    ): P[Emphasis] =
      surrounded(
        surroundingParser = markerParser,
        contentParser = !(s1 | markerFirstCharParser)
          ~ (!(markerParser | eol) ~ singleCharText).+.map(
            foldTexts[Element]
          )
      )
        .map(ElementsContainer.apply)
        .map(v => Emphasis(marker, v))
    def bold: P[Emphasis] =
      nonNestable(P("**"), P("*"), Emphasis.Marker.Bold("**"))
        | nonNestable(P("__"), P("_"), Emphasis.Marker.Bold("__"))
    def code: P[Emphasis] =
      nonNestable(P("`"), P("`"), Emphasis.Marker.Code("`"))
    def italic: P[Emphasis] =
      nonNestable(P("*"), P("*"), Emphasis.Marker.Italic("*"))
        | nonNestable(P("_"), P("_"), Emphasis.Marker.Italic("_"))
    def highlight: P[Emphasis] =
      nonNestable(P("^^"), P("^"), Emphasis.Marker.Italic("^^"))
        | nonNestable(P("=="), P("="), Emphasis.Marker.Italic("=="))
    def strikeThrough: P[Emphasis] =
      nonNestable(P("~~"), P("~"), Emphasis.Marker.Code("~~"))

    choice(bold, code, italic, highlight, strikeThrough)

  private def lineBreak: P[LineBreak.Hardbreak.type] =
    (
      // P("""\\""") ~
      anyFrom("\t ").rep() ~ eolOrEnd
    ).map(_ => LineBreak.Hardbreak)

  private def singleCharText: P[Text] = anyChar.!.map(Text.apply)
