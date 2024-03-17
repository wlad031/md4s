package dev.vgerasimov.md4s
package logseq

import models.*
import ops.{ *, given }

import dev.vgerasimov.slowparse.*
import dev.vgerasimov.slowparse.Parsers.{ *, given }

object parser:

  /** Configuration of a Logseq Markdown document. */
  case class Context(
    statusKeywords: Set[String],
    listMaxLevel: Int,
    headingMinLevel: Int,
    headingMaxLevel: Int,
    commaSeparatedNodeProperties: Set[String],
    aliasNodeProperty: String
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

    /** Default instance of [[Context]]. */
    val defaultCtx: Context = Context(
      statusKeywords = default.statusKeywords,
      listMaxLevel = default.listMaxLevel,
      headingMinLevel = default.headingMinLevel,
      headingMaxLevel = default.headingMaxLevel,
      commaSeparatedNodeProperties = default.commaSeparatedNodeProperties,
      aliasNodeProperty = default.aliasNodeProperty
    )

  end Context

  private enum ListType:
    case Unordered, Ordered

  private case class IntentationSymbol(value: String)

  private def maybeIndentation(indentation: Indentation): Option[Indentation] = indentation match
    case Indentation(0, _)  => None
    case Indentation(_, "") => None
    case x                  => Some(x)

import parser.*

class parser(ctx: Context = Context.defaultCtx):

  def document: P[LogseqMarkdown] = evalAndLazyThen(delayedDocument)

  def delayedDocument: AndLazyThen[Option[PropertyDrawer], LogseqMarkdown] =
    mapAndLazyThen(andLazyThen(propertyDrawer.?, (blockElement(minIndentation = 0).* ~ end))) {
      case (properties, blocks) => LogseqMarkdown(blocks = blocks, propertyDrawer = properties)
    }

  def spacing: P[Spacing] = (tab | space).+.!.map(Spacing.apply)

  private def indentation(min: Int = 0, max: Int = Int.MaxValue): P[Indentation] =
    (
      (tab | (space ~ space)).!.map(IntentationSymbol.apply)
        .rep(min = min, max = max)
        ~ !(tab | space)
    )
      .map(v => Indentation(v.size, v.map(_.value).mkString))

  private def inlineContainerWithoutEmphasis: P[InlineContainer] =
    (choice(timestamp, link, (!eol ~ singleCharText)).+).map(_.toList)
      .map(foldTexts[InlineElement])
      .map(InlineContainer.apply)

  private def inlineContainer: P[InlineContainer] =
    (choice(timestamp, link, emphasis, (!eol ~ singleCharText)).* ~ eolOrEnd)
      .map(_.toList)
      .map(foldTexts[InlineElement])
      .map(InlineContainer.apply)

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
        case (preI, preH) =>
          (
            indentation(min = minIndentation)
            ~ heading(headingMinLevel, headingMaxLevel)
            ~ blockElement(
              listMinLevel = listMinLevel,
              headingMinLevel = preH.level.value + 1,
              headingMaxLevel = headingMaxLevel,
              minIndentation = minIndentation
            ).*
          ).map { case (i, h, c) => HeadedSection(heading = h, content = c, indentation = Some(i)) }
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

      (!listMarker ~ level ~ (priority ~ s0).? ~ (status ~ s0).? ~ inlineContainer.? ~ propertyDrawer.?).map {
        case (
              level: Heading.Level,
              priority: Option[Priority],
              status: Option[Status],
              content: Option[InlineContainer],
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
    def nodePropertyName: P[String] = until(P("::") | eol).!
    def nodePropertyValue: P[InlineContainer] = inlineContainer
    def nodeProperty: P[PropertyDrawer.Node] =
      (
        spacing.?
          ~ nodePropertyName
          ~ P("::")
          ~ spacing.?
          ~ nodePropertyValue.?
      ).map { case (beforeNameSpacing, name, beforeValueSpacing, value) =>
        PropertyDrawer.Node(name, value, beforeNameSpacing, beforeValueSpacing)
      }

    nodeProperty.+.map(_.toList)
      .map(PropertyDrawer.apply)
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
          blockElement(minIndentation = 0, listMinLevel = listMinLevel + 1).?
          ~ blockElement(
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
            MarkdownList.Unordered(items = items, indentation = Some(preIndentation))
          }
      }

  // TODO: Make !_conditions better
  private def paragraph(minIndentation: Int): P[Paragraph] =
    !((s0 ~ listMarker) | (s0 ~ P("#").+ ~ s1))
    ~ (indentation(min =
      minIndentation
    ) ~ (priority ~ s0).? ~ (status ~ s0).? ~ inlineContainer ~ propertyDrawer.? ~ planning.*).map {
      case (ind, priority, status, content, drawer, planning) =>
        Paragraph(
          content = content,
          propertyDrawer = drawer,
          indentation = Some(ind),
          planning = planning,
          priority = priority,
          status = status
        )
    }

  private def codeBlock: P[CodeBlock] =
    def surrounder = P("```")
    s0 ~ surrounded(
      surroundingParser = surrounder,
      contentParser =
        (!(eol | surrounder) ~ anyChar.!).+.mkString.? ~ eol ~ (!surrounder ~ anyChar.!).+.mkString
    ).map { case (lang, content) => CodeBlock(content, lang) }

  private def beginEndBlock: P[BeginEndBlock] =
    def beginBlock: P[String] = P("#+BEGIN_") ~ alpha.+.!.map(_.mkString)
    def endBlock(name: String): P[String] = P("#+END_") ~ P(name).!
    (indentation() ~ beginBlock.flatMap { name => {
      println(name)
      (!endBlock(name) ~ anyChar).*.! ~ endBlock(name)
    }
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

  private def blockElement(
    minIndentation: Int,
    headingMinLevel: Int = 1,
    headingMaxLevel: Int = 6,
    listMinLevel: Int = 0,
    listMaxLevel: Int = ctx.listMaxLevel
  ): P[BlockElement] =
    if (minIndentation > 16) fail[BlockElement]
    else
      choice(
        list(listMinLevel, listMaxLevel, minIndentation),
        headedSection(headingMinLevel, headingMaxLevel, minIndentation, listMinLevel),
        codeBlock,
        beginEndBlock,
        table,
        paragraph(minIndentation),
      )

  private def link: P[Link] =
    import Link.*

    def tag: P[TagInternalLink] =
      (
        P("#") ~ !s1 ~ alphaNum.+.!
          | P("#[[") ~ !s1 ~ (alphaNum.! ~ until(P("]]")).!).map { case (first, next) =>
            first + next
          } ~ P("]]")
      )
        .map(Location.Internal.Page.apply)
        .map(TagInternalLink.apply)

    def page: P[Location.Internal.Page] =
      (
        P("[[") ~ !(P("]") | s1) ~ (anyChar.! ~ until(P("]]")).!).map { case (first, next) =>
          first + next
        } ~ P("]]")
      ).map(Location.Internal.Page.apply)

    // FIXME: This is a mess, also page and block are not the same
    def block: P[Location.Internal.Block] =
      (
        P("((") ~ !s1 ~ (alphaNum.! ~ until(P("]]")).!).map { case (first, next) =>
          first + next
        } ~ P("))")
      ).map(Location.Internal.Block.apply)

    def internalLocation: P[Location.Internal] = page | block

    def text: P[Text] =
      (P("[") ~ !P("[") ~ (!(P(
        "]"
      ) | eolOrEnd) ~ singleCharText.!).+.mkString ~ P("]") ~ !P("]"))
        .map(Text.apply)

    def internalLink: P[Internal] =
      tag | (text.? ~ internalLocation).map { case (text, location) =>
        ClassicInternalLink(location, text)
      }

    def externalLink: P[ExternalLink] =
      (
        text
          ~ (
            P("(")
            ~ (!(P(")") | eolOrEnd) ~ singleCharText.!).+.mkString
            ~ P(")")
          ).map(Location.External.apply)
      ).map { case (text, location) => ExternalLink(location, Some(text)) }

    internalLink | externalLink

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

  private def table: P[Table] = {
    import Table.*
    import Table.Row.*

    def separator: P[Separator.type] =
      (P("|-") ~ anyFrom("\\-+|").rep()).map(_ => Separator)

    def cell: P[Cell] =
      charsUntilIn("\n|").map(s => Cell(InlineContainer(List(Text(s.trim)))))

    def cells: P[Cells] =
      (P("|") ~ cell ~ (P("|") ~ cell).rep() ~ P("|").?.!!).map {
        case (first: Cell, rest: List[Cell]) => Cells((first :: rest.toList))
      }

    def row: P[Row] = s0 ~ (separator | cells)

    (row ~ eol ~ (row ~ eol).rep()).map { case (firstRow, restRows) =>
      Table(firstRow :: restRows.toList)
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
            foldTexts[InlineElement]
          )
      )
        .map(InlineContainer.apply)
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
