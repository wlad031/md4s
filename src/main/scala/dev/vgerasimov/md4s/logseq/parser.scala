package dev.vgerasimov.md4s
package logseq

import models.*
import ops.{ *, given }

import dev.vgerasimov.slowparse.*
import dev.vgerasimov.slowparse.Parsers.{ *, given }

object parser {

  /** Configuration of a Logseq Markdown document. */
  case class Context(
    statusKeywords: Set[String]
  )

  object Context:

    /** Contains default values for all fields of the [[Context]]. */
    object default:
      def statusKeywords: Set[String] = Set("TODO", "DOING", "DONE")

    /** Default instance of [[Context]]. */
    def defaultCtx: Context = Context(
      statusKeywords = default.statusKeywords
    )

  private[md4s] object headline {

    private[md4s] def lvl(fromLevel: Int = 1, toLevel: Int = 6): P[Int] =
      P("#").rep(min = fromLevel, max = toLevel).!.map(_.length)

    private[md4s] def priority: P[Priority] =
      (P("[#") ~ fromRange("A-Z").! ~ P("]"))
        .map(s => s.toList.head)
        .map(Priority.apply)

    // TODO: refactor
    def status: P[Headline.Status] =
      (!(P(" ") | eolOrEnd) ~ anyChar)
        .rep(1)
        .!
        .filter(v => ctx.statusKeywords.contains(v))
        .map(Headline.Status.apply)

    def title: P[Title] =
      (
        timestamp.timestamp
          | markup.textMarkup
          | (!(eol | tags) ~ anyChar.!.map(Text.apply))
      ).rep(1)
        .map(_.toList)
        .map(foldTexts[Title.Content])
        .map(Title.apply)

    def headline(fromLevel: Int = 1): P[Headline] =
      (
        stars(fromLevel)
          ~ (s.!! ~ status).?
          ~ (s.!! ~ priority).?
          ~ (s.!! ~ title).?
          ~ s0
          ~ eolOrEnd
      ).map {
        case (
              stars: Int,
              status: Option[Headline.Status],
              priority: Option[Priority],
              title: Option[Title]
            ) =>
          Headline(
            stars,
            status,
            priority,
            title
          )
      }
  }

  private[md4s] object paragraph {
    private def anyParagraphObject: P[MdObject] =
      timestamp.timestamp | link.link | markup.textMarkup | lineBreak | (!eol ~ singleCharText)

    def paragraph: P[Paragraph] =
      (
        (!headline.headline() ~ anyParagraphObject).rep(1)
          ~ (end.map(_ => Text("")) | eol.!.map(Text.apply))
      ).map { case (ls, last) => ls.toList ++ List(last) }
        .map(foldTexts[MdObject])
        .map(Paragraph.apply)
  }

  private[md4s] def table: P[Table] = {
    import models.Table.*
    import models.Table.Row.*

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

  private[md4s] def timestamp: P[Timestamp] = {
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

  private[md4s] def markup: P[TextMarkup] = {
    import models.TextMarkup.*
    import models.TextMarkup.Marker.*

    def pre: P[String] = (anyFrom("\n\r \t\\-({\'\"")).!
    def post: P[Unit] = end | anyFrom("\n\r \t\\-)}\'\".,:;!?[")

    def marker: P[Marker] =
      anyFrom("*=/+_~").!.map(s => {
        s match {
          case "*" => Some(Bold)
          case "=" => Some(Verbatim)
          case "/" => Some(Italic)
          case "+" => Some(StrikeThrough)
          case "_" => Some(Underline)
          case "~" => Some(Code)
          case _   => None
        }
      })
        .filter(_.isDefined)
        .map(_.get)

    (pre.? ~ marker ~ !P(" "))
      .flatMap[TextMarkup] { case (pre, marker) =>
        P(
          !P(marker.toString)
          ~ (if (marker.isNestable)
               (timestamp
               | markup
               | (!P(marker.toString) ~ singleCharText))
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

  private def singleCharText: P[Text] = anyChar.!.map(Text.apply)
}
