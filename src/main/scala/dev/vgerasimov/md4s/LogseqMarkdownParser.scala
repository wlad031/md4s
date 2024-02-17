package dev.vgerasimov.md4s

import dev.vgerasimov.md4s.models.*
import dev.vgerasimov.md4s.models.elements.{ EmptyLines, Paragraph, PlainList }
import dev.vgerasimov.md4s.models.objects.*
import dev.vgerasimov.md4s.ops.*

import dev.vgerasimov.slowparse.*
import dev.vgerasimov.slowparse.Parsers.{ *, given }

class LogseqMarkdownParser(ctx: LogseqMarkdownContext = LogseqMarkdownContext.defaultCtx) {

  private[md4s] object timestamp {
    import models.objects.Timestamp.Date.*
    import models.objects.Timestamp.Time.*
    import models.objects.Timestamp.*

    def minute: P[Minute] =
      digit
        .rep(min = 2, max = 2)
        .!
        .map(_.toInt)
        .filter(Minute.isValid)
        .map(Minute.apply)

    def hour: P[Hour] =
      digit
        .rep(min = 1, max = 2)
        .!
        .map(_.toInt)
        .filter(Hour.isValid)
        .map(Hour.apply)

    def time: P[Time] =
      (hour ~ P(":") ~ minute).map { case (hour, minute) => Time(hour, minute) }

    def year: P[Year] =
      digit
        .rep(min = 4, max = 4)
        .!
        .map(_.toInt)
        .filter(Year.isValid)
        .map(Year.apply)

    def month: P[Month] =
      digit
        .rep(min = 2, max = 2)
        .!
        .map(_.toInt)
        .filter(Month.isValid)
        .map(Month.apply)

    def day: P[Day] =
      digit
        .rep(min = 2, max = 2)
        .!
        .map(_.toInt)
        .filter(Day.isValid)
        .map(Day.apply)

    def dayName: P[DayName] =
      charsUntilIn("+-]> \t\n\r0123456789").!.map(DayName.fromString)
        .filter(_.isDefined)
        .map(_.get)

    def date: P[Date] =
      (year ~ P("-") ~ month ~ P("-") ~ day ~ (P(" ") ~ dayName).?).filter { case (year, month, day, _) =>
        Date.isValid(year, month, day)
      }.map { case (year, month, day, dayName) => Date(year, month, day, dayName) }

    def diary: P[Diary] = (P("<%%(") ~ charsUntilIn("\n>") ~ P(")>")).map(Diary.apply)

    def repeaterMark: P[(RepeaterOrDelay.Value, RepeaterOrDelay.Unit) => RepeaterOrDelay] =
      (
        P("++").map(_ => RepeaterOrDelay.CatchUpRepeater.apply)
        | P("+").map(_ => RepeaterOrDelay.CumulateRepeater.apply)
        | P(".+").map(_ => RepeaterOrDelay.RestartRepeater.apply)
        | P("--").map(_ => RepeaterOrDelay.FirstTypeDelay.apply)
        | P("-").map(_ => RepeaterOrDelay.AllTypeDelay.apply)
      )

    def repeaterValue: P[RepeaterOrDelay.Value] =
      d
        .rep(1)
        .!
        .map(_.toInt)
        .map(RepeaterOrDelay.Value.apply)

    def repeaterUnit: P[RepeaterOrDelay.Unit] =
      anyFrom("hdwmy").!.map(RepeaterOrDelay.Unit.fromString)
        .filter(_.isDefined)
        .map(_.get)

    def repeaterOrDelay: P[RepeaterOrDelay] =
      (repeaterMark ~ repeaterValue ~ repeaterUnit).map { case (factory, value, unit) => factory(value, unit) }

    def timestamp: P[Timestamp] =
      (
        diary
        | activeTimestampRange
        | activeTimestamp
        | inactiveTimestampRange
        | inactiveTimestamp
      )

    def activeTimestamp: P[ActiveTimestamp] =
      (P("<") ~ date ~ (s ~ time).? ~ (s ~ repeaterOrDelay).? ~ P(">")).map { case (d, t, r) =>
        ActiveTimestamp(d, t, r)
      }

    def inactiveTimestamp: P[InactiveTimestamp] =
      (P("[") ~ date ~ (s ~ time).? ~ (s ~ repeaterOrDelay).? ~ P("]")).map { case (d, t, r) =>
        InactiveTimestamp(d, t, r)
      }

    def activeTimestampRange: P[ActiveTimestampRange] =
      (
        (activeTimestamp ~ P("-").rep(min = 1, max = 3).!! ~ activeTimestamp).map {
          case (from: ActiveTimestamp, to: ActiveTimestamp) => ActiveTimestampRange(from, to)
        }
        | P(P("<") ~ date ~ s ~ time ~ s ~ time ~ (s ~ repeaterOrDelay).? ~ P(">")).map { case (d, t1, t2, r) =>
          ActiveTimestampRange(ActiveTimestamp(d, Some(t1), r), t2)
        }
      )

    def inactiveTimestampRange: P[InactiveTimestampRange] =
      (
        (inactiveTimestamp ~ P("-").rep(min = 1, max = 3).!! ~ inactiveTimestamp).map {
          case (from: InactiveTimestamp, to: InactiveTimestamp) => InactiveTimestampRange(from, to)
        }
        |
        (P("[") ~ date ~ s ~ time ~ s ~ time ~ (s ~ repeaterOrDelay).? ~ P("]")).map { case (d, t1, t2, r) =>
          InactiveTimestampRange(InactiveTimestamp(d, Some(t1), r), t2)
        }
      )
  }

  private[md4s] object planning {
    private def info[A <: Planning.Info](keyword: String, f: Timestamp => A): P[A] =
      (s.rep().!! ~ P(keyword) ~ P(": ") ~ timestamp.timestamp ~ eolOrEnd).map(f)

    def deadlineInfo: P[Planning.Info.Deadline] =
      info("DEADLINE", Planning.Info.Deadline.apply)
    def scheduledInfo: P[Planning.Info.Scheduled] =
      info("SCHEDULED", Planning.Info.Scheduled.apply)
    def closedInfo: P[Planning.Info.Closed] =
      info("CLOSED", Planning.Info.Closed.apply)

    def planning: P[Planning] =
      (deadlineInfo | scheduledInfo | closedInfo).rep(1).map(_.toList).map(Planning.apply)
  }

  private[md4s] object table {
    import models.elements.{ Table, TableRow }
    import models.elements.TableRow.*
    import models.objects.TableCell

    def table: P[Table] = P(tableOrg)

    def tableOrg: P[Table] =
      (tableRow ~ eol ~ (tableRow ~ eol).rep()).map { case (firstRow, restRows) =>
        Table(firstRow :: restRows.toList)
      }

    def tableRow: P[TableRow] = s0 ~ (tableRowSep | tableRowCells)
    def tableRowSep: P[TableSep.type] = (P("|-") ~ anyFrom("\\-+|").rep()).map(_ => TableSep)

    def tableRowCells: P[TableRowCells] =
      (P("|") ~ tableCell ~ (P("|") ~ tableCell).rep() ~ P("|").?.!!).map {
        case (first: TableCell, rest: List[TableCell]) => TableRowCells((first :: rest.toList).filter(_.value.nonEmpty))
      }

    def tableCell: P[TableCell] = charsUntilIn("\n|").map(s => TableCell(s.trim))
  }

  private[md4s] object target {
    import models.objects.{ RadioTarget, Target }

    def radioTarget: P[RadioTarget] =
      (P("<<<") ~ !P(" ") ~ charsUntilIn("<>\n") ~ !P(" ") ~ P(">>>"))
        .map(RadioTarget.apply)

    def target: P[Target] =
      (P("<<") ~ !P(" ") ~ charsUntilIn("<>\n") ~ !P(" ") ~ P(">>"))
        .map(Target.apply)
  }

  private[md4s] object markup {

    import models.objects.{ Marker, TextMarkup }

    private def pre: P[String] = (anyFrom("\n\r \t\\-({\'\"")).!
    private def post: P[Unit] = end | anyFrom("\n\r \t\\-)}\'\".,:;!?[")

    private def marker: P[Marker] =
      anyFrom("*=/+_~").!.map(Marker.fromString)
        .filter(_.isDefined)
        .map(_.get)

    def textMarkup: P[TextMarkup] =
      (pre.? ~ marker ~ !P(" "))
        .flatMap[TextMarkup] { case (pre, marker) =>
          P(
            !P(marker.toString)
            ~ (if (marker.isNestable)
                 (timestamp.timestamp
                 | markup.textMarkup
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

  private[md4s] object link {
    import models.objects.Link

    def linkProtocol: P[String] =
      (!P(":") ~ fromRange("a-zA-Z0-9"))
        .rep(1)
        .!
        .filter(ctx.linkTypes.contains)

    private def contentsWithoutLinks: P[Contents] =
      (!P("]") ~ anyChar.!.map(Text.apply))
        .rep()
        .map(_.toList)
        .map(foldTexts[MdObject])
        .map(ls => Contents(ls))

    private def path4: P[String] = charsUntilIn("]")

    def link: P[Link] =
      (
        P("[[") ~ path4 ~ P("]]")
      ).map { case c => Link(None, c, None) }
  }

  private[md4s] object headline {
    import models.Headline.*

    private[md4s] def stars(fromLevel: Int = 1): P[Int] =
      P("#").rep(min = fromLevel, max = 6).!.map(_.length)

    private[md4s] def priority: P[Priority] =
      (P("[#") ~ fromRange("A-Z").! ~ P("]")).map(s => s.toList.head).map(Priority.apply)

    // TODO: refactor
    def status: P[Headline.Status] =
      (!(P(" ") | eolOrEnd) ~ anyChar)
        .rep(1)
        .!
        .filter(v => ctx.statusKeywords.contains(v))
        .map(Headline.Status.apply)

    def tags: P[List[String]] =
      (P(":") ~ choice(fromRange("a-z"), fromRange("A-Z"), fromRange("0-9"), anyFrom("%@#_"))
        .rep(1)
        .!
        .rep(min = 1, sep = Some(P(":"))) ~ P(":"))
        .map(_.toList)

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

  private[md4s] object plainList {
    import models.elements.PlainList.*

    def counter: P[Counter] = (d.rep(1) | fromRange("a-zA-Z")).!.map(Counter.apply)

    def counterSet: P[Counter] = P("[@") ~ counter ~ P("]")

    def checkbox: P[Checkbox] =
      (
        P("[")
        ~ (P(" ").map(_ => Checkbox.Empty)
        | P("X").map(_ => Checkbox.Checked)
        | P("-").map(_ => Checkbox.Unchecked))
        ~ P("]")
      )

    def tag: P[String] =
      !(P(" :: ") | eolOrEnd) ~ anyChar.rep(1).! ~ P(" :: ")

    def orderedBullet: P[Bullet.Ordered] =
      (counter ~ anyFrom(".)").!).map { case (i, c) => Bullet.Ordered(i, c(0)) } ~ (s | eolOrEnd)

    def charBullet: P[Bullet.Character] =
      anyFrom("*+\\-").!.map(_(0)).map(Bullet.Character.apply) ~ (s | eolOrEnd)

    private def anyItemObject: P[Content] =
      timestamp.timestamp | link.link | markup.textMarkup | (!eol ~ singleCharText)

    private def anyItemElement(minIndent: Int, maxIndent: Int): P[Element] =
      (
        table.table
        | plainList(minIndent, maxIndent)
        | emptyLines(Some(1))
        | (!indentation(0, minIndent - 1) ~ paragraph.paragraph)
      )
    def foo: P[Any] = P("asd").!.~(P("asd").!).~(P("asd").!)
    private def indentation(minIndent: Int, maxIndent: Int): P[Int] =
      P(P(" ").rep(min = minIndent, max = maxIndent) ~ !P(" ")).!.map(_.length)

    def item(minIndent: Int, maxIndent: Int): P[Item] =
      for {
        lvl    <- indentation(minIndent, maxIndent)
        bullet <- charBullet | orderedBullet
        t <- (
          (counterSet ~ s).?
          ~ (checkbox ~ s).?
          ~ (tag ~ s).?
          ~ anyItemObject.rep().map(_.toList).map(foldTexts[Content])
          ~ (
            end.map(_ => None)
            | (eol ~ anyItemElement(lvl + 1, maxIndent)
              .rep()
              .map(_.toList)
              .map(foldParagraphs[Element])
              .?)
          )
        )
      } yield t match {
        case (counterSet, checkbox, tag, content, elements) =>
          Item(lvl, bullet, checkbox, counterSet, tag, content, elements.getOrElse(Nil))
      }

    def plainList(minIndent: Int = 0, maxIndent: Int = 48): P[PlainList] =
      for {
        head <- !headline.headline() ~ item(minIndent, maxIndent)
        tail <- (!headline.headline() ~ item(head.indentation, maxIndent)).rep()
      } yield PlainList.UnorderedList(head :: tail.toList)
  }

  private[md4s] object propertyDrawer {
    import models.elements.NodeProperty
    import models.elements.PropertyDrawer

    private def nodePropertyName: P[String] = until(P("::")).!
    private def nodePropertyValue: P[String] = charsUntilEol

    def nodeProperty: P[NodeProperty] =
      (
        nodePropertyName
          ~ P("::")
          ~ s0
          ~ nodePropertyValue.?.map(_.filter(_.nonEmpty))
          ~ eol
      ).map { case (name: String, value: Option[String]) => NodeProperty(name, value) }

    def propertyDrawer: P[PropertyDrawer] =
      (s0 ~ (s0 ~ nodeProperty).+ ~ eolOrEnd)
        .map(_.toList)
        .map(PropertyDrawer.apply)
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

  private[md4s] def lineBreak: P[LineBreak.type] =
    (P("""\\""") ~ anyFrom("\t ").rep() ~ eolOrEnd).map(_ => LineBreak)

  private[md4s] def duration: P[Duration] =
    (P("=>") ~ s ~ d.rep(1).! ~ P(":") ~ d.rep(min = 2, max = 2).!).map { case (h, m) => (h.toInt, m.toInt) }.map {
      case (h, m) => Duration(h, m)
    }

  private[md4s] def clock: P[Clock] =
    P("CLOCK:") ~ s0 ~ (
      timestamp.inactiveTimestamp.map(Clock.Simple.apply)
      | (timestamp.inactiveTimestampRange ~ s ~ duration).map { case (tr, d) =>
        Clock.WithDuration(tr, d)
      }
    )

  private def singleCharText: P[Text] = anyChar.!.map(Text.apply)

  private def anySectionElement: P[Element] =
    table.table | plainList.plainList() | emptyLines() | paragraph.paragraph

  private def anyDocumentSectionElement: P[Element] =
    propertyDrawer.propertyDrawer | anySectionElement

  private def emptyLines(max: Option[Int] = None): P[EmptyLines] =
    max
      .map(v => eol.rep(min = 1, max = v) ~ !eol)
      .getOrElse(eol.rep(1))
      .!
      .map(s => EmptyLines(s.length))

  private[md4s] def section(fromLevel: Int = 1): P[Section] =
    for {
      headline       <- headline.headline(fromLevel)
      planning       <- planning.planning.?
      propertyDrawer <- propertyDrawer.propertyDrawer.?
      elements       <- anySectionElement.rep()
      childSections  <- section(headline.level + 1).rep()
    } yield {
      Section(
        headline,
        foldParagraphs[Element](elements.toList),
        childSections.toList,
        planning,
        propertyDrawer
      )
    }

  def document: P[Document] = {
    for {
      elements <- anyDocumentSectionElement.rep()
      parser = new LogseqMarkdownParser()
      sections <- parser.section().rep() ~ end
    } yield Document(elements.toList, sections.toList)
  }
}
