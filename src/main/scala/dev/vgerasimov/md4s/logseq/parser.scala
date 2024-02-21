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
    listMaxLevel: Int
  )

  object Context:

    /** Contains default values for all fields of the [[Context]]. */
    object default:
      val statusKeywords: Set[String] = Set("TODO", "DOING", "DONE")
      val listMaxLevel: Int = 20

    /** Default instance of [[Context]]. */
    def defaultCtx: Context = Context(
      statusKeywords = default.statusKeywords,
      listMaxLevel = default.listMaxLevel
    )

  private enum ListType:
    case Unordered, Ordered

  private val charsToMarkers: Map[String, TextMarkup.Marker] = Map(
    "*" -> TextMarkup.Marker.Bold,
    "=" -> TextMarkup.Marker.Verbatim,
    "/" -> TextMarkup.Marker.Italic,
    "+" -> TextMarkup.Marker.StrikeThrough,
    "_" -> TextMarkup.Marker.Underline,
    "`" -> TextMarkup.Marker.Code
  )

  private val markersToChars: Map[TextMarkup.Marker, String] =
    charsToMarkers.map(_.swap)

import parser.*

class parser(ctx: Context = Context.defaultCtx):

  def document: P[MarkdownAST] =
    blockElement()
      .rep(1)
      .map(blocks => collapseHeadedSections(blocks))
      .map(blocks => MarkdownAST(blocks))

  private def inlineContainer: P[InlineContainer] =
    (choice(timestamp, link, markup, (!eol ~ singleCharText)).+ ~ eolOrEnd)
      .map(_.toList)
      .map(foldTexts[InlineElement])
      .map(InlineContainer.apply)

  private def heading(minLevel: Int, maxLevel: Int): P[Heading] =
    def headerLevel: P[Int] =
      P("#").rep(min = minLevel, max = maxLevel).!.map(_.length)

    def priority: P[Priority] =
      (P("[#") ~ fromRange("A-Z").! ~ P("]"))
        .map(s => s.toList.head)
        .map(Priority.apply)

    def status: P[Status] =
      ctx.statusKeywords.map(kw => P(kw)).reduce(_ | _).!.map(Status.apply)

    (!listMarker ~ (headerLevel ~ s0) ~ (priority ~ s0).? ~ (status ~ s0).? ~ inlineContainer.? ~ propertyDrawer.?).map {
      case (
            headerLevel: Int,
            priority: Option[Priority],
            status: Option[Status],
            content: Option[InlineContainer],
            drawer: Option[PropertyDrawer]
          ) =>
        Heading(content, headerLevel, status, priority, drawer)
    }

  private def propertyDrawer: P[PropertyDrawer] = {

    def nodePropertyName: P[String] = until(P("::") | eol).!
    def nodePropertyValue: P[InlineContainer] = inlineContainer

    def nodeProperty: P[PropertyDrawer.Node] =
      (
        nodePropertyName
          ~ P("::")
          ~ s0
          ~ nodePropertyValue.?
      ).map { case (name: String, value: Option[InlineContainer]) =>
        PropertyDrawer.Node(name, value)
      }

    (s0 ~ (s0 ~ nodeProperty).+)
      .map(_.toList)
      .map(PropertyDrawer.apply)
  }

  private def orderedListMarker: P[Int] = d.+.!.map(_.toInt) ~ P(".")
  private def listMarker: P[String | Int] = anyFrom("-+").! | orderedListMarker

  private def list(
    listMinLevel: Int,
    listMaxLevel: Int
  ): P[MarkdownList] =
    if (listMinLevel > ctx.listMaxLevel || listMinLevel > listMaxLevel)
      fail[MarkdownList]
    else
      def indentation: P[Int] =
        P(P("\t").rep(min = listMinLevel, max = listMaxLevel) ~ !P("\t")).!.map(
          _.length
        )

      &(indentation.!! ~ listMarker).flatMap {
        case marker: String =>
          (indentation.!!
          ~ P(marker)
          ~ s0
          ~ blockElement(listMinLevel = listMinLevel + 1).rep())
            .map(items => MarkdownList.Item(items))
            .rep(1)
            .map(items => MarkdownList.Unordered(items))
        case marker: Int =>
          (indentation.!!
          ~ orderedListMarker.!!
          ~ s0
          ~ (blockElement(listMinLevel = listMinLevel + 1)
            .rep())
            .map(items => MarkdownList.Item(items)))
            .rep(1)
            .map(items => MarkdownList.Ordered(items))
      }

  private def paragraph: P[Paragraph] =
    !(s0 ~ listMarker)
    ~ (inlineContainer ~ propertyDrawer.?).map {
      case (content: InlineContainer, drawer: Option[PropertyDrawer]) =>
        Paragraph(content, drawer)
    }

  private def emptyLines(max: Option[Int] = None): P[EmptyLines] =
    max
      .map(v => eol.rep(min = 2, max = v) ~ !eol)
      .getOrElse(eol.rep(1))
      .!
      .map(s => EmptyLines(s.length))

  private def blockElement(
    headingMinLevel: Int = 1,
    headingMaxLevel: Int = 6,
    listMinLevel: Int = 0,
    listMaxLevel: Int = ctx.listMaxLevel
  ): P[BlockElement] =
    choice(
      list(listMinLevel, listMaxLevel),
      heading(headingMinLevel, headingMaxLevel),
      paragraph,
      table,
      emptyLines()
    )

  private def link: P[Link] =

    def linkProtocol: P[String] =
      (!P(":") ~ fromRange("a-zA-Z0-9"))
        .rep(1)
        .!
    // .filter(ctx.linkTypes.contains)

    // def contentsWithoutLinks: P[Contents] =
    //   (!P("]") ~ anyChar.!.map(Text.apply))
    //     .rep()
    //     .map(_.toList)
    //     .map(foldTexts[MdObject])
    //     .map(ls => Contents(ls))

    def path4: P[String] = charsUntilIn("]")

    (P("[[") ~ path4 ~ P("]]")).map { case c =>
      Link(InlineContainer(List(Text(c))), null, None)
    }

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

    def repeaterMark
      : P[(RepeaterOrDelay.Value, RepeaterOrDelay.Unit) => RepeaterOrDelay] =
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
      (repeaterMark ~ repeatervalue ~ repeaterUnit).map {
        case (factory, value, unit) => factory(value, unit)
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
          .!! ~ activeTimestamp).map {
          case (from: ActiveTimestamp, to: ActiveTimestamp) =>
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
          .!! ~ inactiveTimestamp).map {
          case (from: InactiveTimestamp, to: InactiveTimestamp) =>
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

  // FIXME: Markup is problematic and slow
  private def markup: P[TextMarkup] = {
    import TextMarkup.*
    import TextMarkup.Marker.*

    def pre: P[String] = (anyFrom("\n\r \t\\-({\'\"")).!
    def post: P[Unit] = end | anyFrom("\n\r \t\\-)}\'\".,:;!?[")

    def marker: P[Marker] =
      choice(charsToMarkers.keySet.map(P(_))).!.map(charsToMarkers.get)
        .filter(_.isDefined)
        .map(_.get)

    (pre.? ~ marker ~ !P(" "))
      .flatMap[TextMarkup] { case (pre, marker) =>
        P(
          !P(markersToChars(marker))
          ~ (if (marker.isNestable)
               (timestamp
               | markup
               | (!P(markersToChars(marker)) ~ singleCharText))
                 .rep(1)
                 .map(_.toList)
                 .map(foldTexts[TextMarkup.Content])
             else
               (!P(marker.toString) ~ singleCharText)
                 .rep(1)
                 .map(_.reduce(_ ++ _))
                 .map(List(_))).map(TextMarkup(pre.getOrElse(""), marker, _))
          ~ P(marker.toString)
          ~ &(post)
        )
      }
  }

  private def lineBreak: P[LineBreak.Hardbreak.type] =
    (
      // P("""\\""") ~
      anyFrom("\t ").rep() ~ eolOrEnd
    ).map(_ => LineBreak.Hardbreak)

  private def singleCharText: P[Text] = anyChar.!.map(Text.apply)
