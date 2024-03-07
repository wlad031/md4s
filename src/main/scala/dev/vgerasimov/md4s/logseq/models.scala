package dev.vgerasimov.md4s
package logseq

// TODO: remove it
import upickle.default.{ ReadWriter }

import dev.vgerasimov.md4s.models.MarkdownDocument

object models:

  /** A Markdown document can now be constructed by assembling these elements into a hierarchy. For
    * example, a document might contain a list of block elements where each element corresponds to
    * items such paragraphs, code blocks, and so on.
    */
  case class LogseqMarkdown(
    blocks: List[BlockElement],
    propertyDrawer: Option[PropertyDrawer] = None
  ) extends MarkdownDocument
      derives ReadWriter

  sealed trait BlockElement extends MarkdownDocument derives ReadWriter

  case class Status(value: String) derives ReadWriter
  case class Priority(value: Char) derives ReadWriter
  case class PropertyDrawer(nodes: List[PropertyDrawer.Node]) derives ReadWriter
  object PropertyDrawer:
    case class Node(name: String, value: Option[InlineContainer] = None) derives ReadWriter
  case class Indentation(level: Int, value: String) derives ReadWriter
  object Indentation:
    val zero: Indentation = Indentation(0, "")

  sealed trait InlineElement derives ReadWriter

  case class InlineContainer(
    elements: List[InlineElement]
  ) extends InlineElement
      derives ReadWriter

  case class HeadedSection(
    heading: Heading,
    content: List[BlockElement],
    indentation: Indentation = Indentation.zero
  ) extends BlockElement
      derives ReadWriter

  case class Heading(
    content: Option[InlineContainer],
    headerLevel: Int,
    status: Option[Status] = None,
    priority: Option[Priority] = None,
    propertyDrawer: Option[PropertyDrawer] = None,
    indentation: Indentation = Indentation.zero
  ) extends BlockElement
      derives ReadWriter

  case class Paragraph(
    content: InlineContainer,
    propertyDrawer: Option[PropertyDrawer] = None,
    indentation: Indentation = Indentation.zero
  ) extends BlockElement
      derives ReadWriter

  sealed trait MarkdownList extends BlockElement derives ReadWriter
  object MarkdownList:
    case class Ordered(items: List[Item], indentation: Indentation = Indentation.zero)
        extends MarkdownList derives ReadWriter
    case class Unordered(items: List[Item], indentation: Indentation = Indentation.zero)
        extends MarkdownList derives ReadWriter
    case class Item(content: List[BlockElement]) derives ReadWriter

  case class Blockquote(content: List[BlockElement]) extends BlockElement derives ReadWriter

  case class CodeBlock(
    codeText: String,
    language: Option[String] = None
  ) extends BlockElement
      derives ReadWriter

  case class HorizontalRuler() extends BlockElement derives ReadWriter

  case class Table(
    rows: List[Table.Row],
    indentation: Indentation = Indentation.zero
  ) extends BlockElement
      derives ReadWriter
  object Table:
    sealed trait Row derives ReadWriter
    object Row:
      case object Separator extends Row derives ReadWriter
      case class Cells(cells: List[Cell]) extends Row derives ReadWriter
    case class Cell(content: InlineContainer) derives ReadWriter

  case class Text(content: String) extends InlineElement derives ReadWriter

  case class Emphasis(
    marker: Emphasis.Marker,
    contents: InlineContainer
  ) extends InlineElement
      derives ReadWriter
  object Emphasis:
    sealed trait Marker derives ReadWriter:
      def value: String
    object Marker:
      case class Bold(override val value: String) extends Marker derives ReadWriter
      case class Italic(override val value: String) extends Marker derives ReadWriter
      case class StrikeThrough(override val value: String) extends Marker derives ReadWriter
      case class Code(override val value: String) extends Marker derives ReadWriter
      case class Highlight(override val value: String) extends Marker derives ReadWriter

  sealed trait Link extends InlineElement derives ReadWriter
  object Link:
    sealed trait Internal extends Link derives ReadWriter
    case class ClassicInternalLink(
      location: Link.Location.Internal,
      text: Option[Text]
    ) extends Internal
        derives ReadWriter
    case class TagInternalLink(location: Location.Internal.Page) extends Internal derives ReadWriter
    case class ExternalLink(
      location: Location.External,
      text: Option[Text]
    ) extends Link
        derives ReadWriter

    sealed trait Location derives ReadWriter
    object Location:
      case class External(value: String) extends Location derives ReadWriter
      sealed trait Internal extends Location derives ReadWriter
      object Internal:
        case class Page(value: String) extends Internal derives ReadWriter
        case class Block(value: String) extends Internal derives ReadWriter

  case class Image(altText: String, url: String, title: Option[String]) extends InlineElement
      derives ReadWriter

  sealed trait LineBreak extends InlineElement derives ReadWriter
  object LineBreak:
    case object Softbreak extends LineBreak derives ReadWriter
    case object Hardbreak extends LineBreak derives ReadWriter

  sealed trait Timestamp extends InlineElement derives ReadWriter
  object Timestamp:
    sealed trait Active extends Timestamp derives ReadWriter
    sealed trait Inactive extends Timestamp derives ReadWriter
    sealed trait Range extends Timestamp derives ReadWriter

    case class Diary(value: String) extends Timestamp derives ReadWriter

    case class ActiveTimestamp(
      date: Date,
      time: Option[Time],
      repeaterOrDelay: Option[RepeaterOrDelay] = None
    ) extends Timestamp
        with Timestamp.Active
        derives ReadWriter

    case class InactiveTimestamp(
      date: Date,
      time: Option[Time],
      repeaterOrDelay: Option[RepeaterOrDelay] = None
    ) extends Timestamp
        with Timestamp.Inactive
        derives ReadWriter

    case class ActiveTimestampRange(
      from: ActiveTimestamp,
      to: ActiveTimestamp
    ) extends Timestamp
        with Timestamp.Active
        with Timestamp.Range
        derives ReadWriter

    case class InactiveTimestampRange(
      from: InactiveTimestamp,
      to: InactiveTimestamp
    ) extends Timestamp
        with Timestamp.Inactive
        with Timestamp.Range
        derives ReadWriter

    case class Time(hour: Time.Hour, minute: Time.Minute) derives ReadWriter

    object Time:
      case class Hour(value: Int) derives ReadWriter
      case class Minute(value: Int) derives ReadWriter

    case class Date(
      year: Date.Year,
      month: Date.Month,
      day: Date.Day,
      dayName: Option[Date.DayName] = None
    ) derives ReadWriter

    object Date:
      case class Year(value: Int) derives ReadWriter
      case class Month(value: Int) derives ReadWriter
      case class Day(value: Int) derives ReadWriter

      enum DayName derives ReadWriter:
        case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday

    sealed trait RepeaterOrDelay derives ReadWriter

    object RepeaterOrDelay:
      sealed trait Repeater extends RepeaterOrDelay derives ReadWriter
      sealed trait Delay extends RepeaterOrDelay derives ReadWriter
      case class And(repeater: Repeater, delay: Delay) extends RepeaterOrDelay derives ReadWriter
      case class CumulateRepeater(value: Value, unit: Unit) extends Repeater derives ReadWriter
      case class CatchUpRepeater(value: Value, unit: Unit) extends Repeater derives ReadWriter
      case class RestartRepeater(value: Value, unit: Unit) extends Repeater derives ReadWriter
      case class AllTypeDelay(value: Value, unit: Unit) extends Delay derives ReadWriter
      case class FirstTypeDelay(value: Value, unit: Unit) extends Delay derives ReadWriter

      case class Value(value: Int) derives ReadWriter

      sealed trait Unit derives ReadWriter
      object Unit:
        case object Hour extends Unit derives ReadWriter
        case object Day extends Unit derives ReadWriter
        case object Week extends Unit derives ReadWriter
        case object Month extends Unit derives ReadWriter
        case object Year extends Unit derives ReadWriter
