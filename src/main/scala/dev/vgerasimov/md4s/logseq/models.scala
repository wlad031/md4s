package dev.vgerasimov.md4s
package logseq

import scala.sys.Prop

object models:

  /** Represents the root of a Markdown document AST. */
  sealed trait MarkdownDocument

  /** A Markdown document can now be constructed by assembling these elements
    * into a hierarchy. For example, a document might contain a list of block
    * elements where each element corresponds to items such paragraphs, code
    * blocks, and so on.
    */
  case class MarkdownAST(blocks: List[BlockElement]) extends MarkdownDocument

  sealed trait BlockElement extends MarkdownDocument

  case class EmptyLines(length: Int) extends BlockElement

  case class Status(value: String)
  case class Priority(value: Char)
  case class PropertyDrawer(nodes: List[PropertyDrawer.Node])
  object PropertyDrawer:
    case class Node(name: String, value: Option[InlineContainer] = None) 

  sealed trait InlineElement

  case class InlineContainer(
    elements: List[InlineElement]
  ) extends InlineElement

  case class HeadedSection(
    heading: Heading,
    content: List[BlockElement]
  ) extends BlockElement

  case class Heading(
    content: Option[InlineContainer],
    headerLevel: Int,
    status: Option[Status] = None,
    priority: Option[Priority] = None,
    propertyDrawer: Option[PropertyDrawer] = None
  ) extends BlockElement

  case class Paragraph(
    content: InlineContainer,
    propertyDrawer: Option[PropertyDrawer] = None
  ) extends BlockElement

  sealed trait MarkdownList extends BlockElement
  object MarkdownList:
    case class Ordered(items: List[Item]) extends MarkdownList
    case class Unordered(items: List[Item]) extends MarkdownList
    case class Item(content: List[BlockElement])

  case class Blockquote(content: List[BlockElement]) extends BlockElement

  case class CodeBlock(
    codeText: String,
    language: Option[String] = None
  ) extends BlockElement

  case class HorizontalRuler() extends BlockElement

  case class Table(
    rows: List[Table.Row],
  ) extends BlockElement
  object Table:
    sealed trait Row
    object Row:
      case object Separator extends Row
      case class Cells(cells: List[Cell]) extends Row
    case class Cell(content: InlineContainer)

  case class HTMLBlock(
    htmlContent: String
  ) extends BlockElement

  case class LinkReferenceDefinition(
    label: String,
    url: String,
    title: Option[String]
  ) extends BlockElement

  case class Text(content: String) extends InlineElement with TextMarkup.Content

  case class TextMarkup(
    pre: String,
    marker: TextMarkup.Marker,
    contents: List[TextMarkup.Content]
  ) extends InlineElement
      with TextMarkup.Content

  object TextMarkup:
    enum Marker:
      case Bold, Verbatim, Italic, StrikeThrough, Underline, Code

    sealed trait Content

  case class Link(text: InlineContainer, url: String, title: Option[String])
      extends InlineElement

  case class Image(altText: String, url: String, title: Option[String])
      extends InlineElement

  sealed trait LineBreak extends InlineElement
  object LineBreak:
    case object Softbreak extends LineBreak
    case object Hardbreak extends LineBreak

  case class HTMLInline(htmlContent: String) extends InlineElement

  case class Autolink(url: String) extends InlineElement

  case class EmailAutolink(email: String) extends InlineElement

  case class LinkReference(
    label: String,
    reference: String,
    content: InlineContainer
  ) extends InlineElement

  case class ImageReference(label: String, reference: String, altText: String)
      extends InlineElement

  case class Footnote(content: InlineContainer) extends InlineElement

  case class FootnoteReference(identifier: String) extends InlineElement

  sealed trait Timestamp extends InlineElement with TextMarkup.Content
  object Timestamp:
    sealed trait Active extends Timestamp
    sealed trait Inactive extends Timestamp
    sealed trait Range extends Timestamp

    case class Diary(value: String) extends Timestamp

    case class ActiveTimestamp(
      date: Date,
      time: Option[Time],
      repeaterOrDelay: Option[RepeaterOrDelay] = None
    ) extends Timestamp
        with Timestamp.Active

    case class InactiveTimestamp(
      date: Date,
      time: Option[Time],
      repeaterOrDelay: Option[RepeaterOrDelay] = None
    ) extends Timestamp
        with Timestamp.Inactive

    case class ActiveTimestampRange(
      from: ActiveTimestamp,
      to: ActiveTimestamp
    ) extends Timestamp
        with Timestamp.Active
        with Timestamp.Range

    // object ActiveTimestampRange {
    // def apply(from: ActiveTimestamp, toTime: Time): ActiveTimestampRange =
    // ActiveTimestampRange(from, from.copy(time = Some(toTime)))
    // }

    case class InactiveTimestampRange(
      from: InactiveTimestamp,
      to: InactiveTimestamp
    ) extends Timestamp
        with Timestamp.Inactive
        with Timestamp.Range

    // object InactiveTimestampRange {
    // def apply(
    //   from: InactiveTimestamp,
    //   toTime: Time
    // ): InactiveTimestampRange =
    //   InactiveTimestampRange(from, from.copy(time = Some(toTime)))
    // }

    case class Time(hour: Time.Hour, minute: Time.Minute)
    // override def toString: String = s"$hour:$minute"
    // }

    object Time:
      // def of(hour: Int, minute: Int): Time = Time(Hour(hour), Minute(minute))

      case class Hour(value: Int)
      // require(Hour.isValid(value), s"Invalid hour value: $value")

      // override def toString: String = f"$value%02d"
      // }

      // object Hour {

      /** Checks that provided value is valid hour. */
      // def isValid(hour: Int): Boolean = 0 <= hour && hour <= 23
      // }

      case class Minute(value: Int)
    // require(Minute.isValid(value), s"Invalid minute value: $value")

    // override def toString: String = f"$value%02d"
    // }

    // object Minute {

    /** Checks that provided value is valid minute. */
    // def isValid(minute: Int): Boolean = 0 <= minute && minute <= 59
    // }

    case class Date(
      year: Date.Year,
      month: Date.Month,
      day: Date.Day,
      dayName: Option[Date.DayName] = None
    )
    // require(
    //   Date.isValid(year, month, day),
    //   s"Invalid date: year=$year, month=$month, day=$day"
    // )

    // override def toString: String =
    //   dayName match {
    //     case Some(dn) => s"$year-$month-$day $dn"
    //     case None     => s"$year-$month-$day"
    //   }
    // }

    object Date:

      // def of(
      //   year: Int,
      //   month: Int,
      //   day: Int,
      //   dayName: Option[String] = None
      // ): Date =
      //   Date(
      //     Year(year),
      //     Month(month),
      //     Day(day),
      //     dayName.flatMap(DayName.fromString)
      //   )

      // /** Checks that provided values represent a valid date. */
      // def isValid(year: Year, month: Month, day: Day): Boolean =
      //   day.value <= month.getNumberOfDays(year.isLeap)

      case class Year(value: Int)
      // require(Year.isValid(value), s"Invalid year value: $value")

      // /** Indicates whether this year is leap or not. */
      // def isLeap: Boolean =
      //   if (value % 4 != 0) false
      //   else if (value % 100 != 0) true
      //   else if (value % 400 != 0) false
      //   else true

      // override def toString: String = f"$value%04d"
      // }

      // object Year {

      //   /** Checks that provided value is valid year. */
      //   def isValid(year: Int): Boolean = 0 <= year
      // }

      case class Month(value: Int)
      // require(Month.isValid(value), s"Invalid month value: $value")

      /** Returns the number of days in this month. */
      // def getNumberOfDays(isLeapYear: Boolean = false): Int =
      //   if (value == 2 && isLeapYear) Month.days(value - 1) + 1
      //   else Month.days(value - 1)

      // override def toString: String = f"$value%02d"
      // }

      // object Month {

      /** Checks that provided value is valid month. */
      // def isValid(month: Int): Boolean = 1 <= month && month <= 12

      // /** Numbers of days in all 12 months for non-leap year. */
      // private val days: Array[Int] =
      //   Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      // }

      case class Day(value: Int)
      // require(Day.isValid(value), s"Invalid day value: $value")

      // override def toString: String = f"$value%02d"
      // }

      // object Day {

      /** Checks that provided value is valid day. */
      // def isValid(day: Int): Boolean = 1 <= day && day <= 31
      // }

      // override def toString: String =
      //   this match {
      //     case DayName.Monday    => "Mon"
      //     case DayName.Tuesday   => "Tue"
      //     case DayName.Wednesday => "Wed"
      //     case DayName.Thursday  => "Thu"
      //     case DayName.Friday    => "Fri"
      //     case DayName.Saturday  => "Sat"
      //     case DayName.Sunday    => "Sun"
      //   }
      // }

      enum DayName:
        case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday

    // def fromString(dayName: String): Option[DayName] =
    //   dayName match {
    //     case x if Set("Mon") contains x => Some(Monday)
    //     case x if Set("Tue") contains x => Some(Tuesday)
    //     case x if Set("Wed") contains x => Some(Wednesday)
    //     case x if Set("Thu") contains x => Some(Thursday)
    //     case x if Set("Fri") contains x => Some(Friday)
    //     case x if Set("Sat") contains x => Some(Saturday)
    //     case x if Set("Sun") contains x => Some(Sunday)
    //     case _                          => None
    //   }
    // }
    // }

    sealed trait RepeaterOrDelay

    object RepeaterOrDelay:
      sealed trait Repeater extends RepeaterOrDelay
      sealed trait Delay extends RepeaterOrDelay
      case class And(repeater: Repeater, delay: Delay) extends RepeaterOrDelay
      case class CumulateRepeater(value: Value, unit: Unit) extends Repeater
      case class CatchUpRepeater(value: Value, unit: Unit) extends Repeater
      case class RestartRepeater(value: Value, unit: Unit) extends Repeater
      case class AllTypeDelay(value: Value, unit: Unit) extends Delay
      case class FirstTypeDelay(value: Value, unit: Unit) extends Delay

      case class Value(value: Int)

      sealed trait Unit

      object Unit:
        case object Hour extends Unit
        case object Day extends Unit
        case object Week extends Unit
        case object Month extends Unit
        case object Year extends Unit

// def fromString(unit: String): Option[Unit] =
//   unit match {
//     case x if Set("h").contains(x) => Some(Hour)
//     case x if Set("d").contains(x) => Some(Day)
//     case x if Set("w").contains(x) => Some(Week)
//     case x if Set("m").contains(x) => Some(Month)
//     case x if Set("y").contains(x) => Some(Year)
//     case _                         => None
//   }
