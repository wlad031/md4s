package dev.vgerasimov.md4s
package logseq

import dev.vgerasimov.md4s.models.*

object models:
  import blockElements.*
  import inlineElements.*

  export blockElements.*
  export inlineElements.*

  /** A Markdown document can now be constructed by assembling these elements into a hierarchy. For
    * example, a document might contain a list of block elements where each element corresponds to
    * items such paragraphs, code blocks, and so on.
    */
  case class LogseqMarkdown(
    propertyDrawer: Option[PropertyDrawer] = None,
    blocks: List[BlockElement] = Nil
  ) extends MarkdownDocument

  case class Spacing(value: String = Spacing.defaultSpaceValue)
  object Spacing:
    private val defaultSpaceValue = " "
    val defaultSpacing = Spacing()
  trait RightSpaced:
    def spacingAfter: Spacing = Spacing.defaultSpacing
  trait MaybeRightSpaced:
    def spacingAfter: Option[Spacing] = None

  case class Indentation(
    level: Int = Indentation.defaultLevel,
    value: String = Indentation.defaultValue
  )
  object Indentation:
    private val defaultLevel: Int = 0
    private val defaultValue: String = ""
    val defaultIndentation: Indentation = Indentation()
  trait Indentable:
    def indentation: Indentation = Indentation.defaultIndentation
  trait MaybeIndentable:
    def indentation: Option[Indentation] = None

  case class Status(value: String, override val spacingAfter: Option[Spacing] = None)
      extends MaybeRightSpaced
  case class Priority(value: Char, override val spacingAfter: Option[Spacing] = None)
      extends MaybeRightSpaced

  case class PropertyDrawer(nodes: List[PropertyDrawer.Node])
  object PropertyDrawer:
    case class Node(
      name: String,
      value: Option[InlineContainer] = None,
      spacingBeforeName: Option[Spacing] = None,
      spacingBeforeValue: Option[Spacing] = Some(Spacing(" "))
    )

  case class CustomProperties(name: String, value: String)

  sealed trait Planning:
    def timestamp: Timestamp
  object Planning:
    case class Scheduled(
      override val timestamp: Timestamp,
      spacingBeforeKeyword: Option[Spacing] = None,
      spacingBeforeTimestamp: Option[Spacing] = None,
      spacingAfterTimestamp: Option[Spacing] = None
    ) extends Planning
    case class Deadline(
      override val timestamp: Timestamp,
      spacingBeforeKeyword: Option[Spacing] = None,
      spacingBeforeTimestamp: Option[Spacing] = None,
      spacingAfterTimestamp: Option[Spacing] = None
    ) extends Planning
    case class Closed(
      override val timestamp: Timestamp,
      spacingBeforeKeyword: Option[Spacing] = None,
      spacingBeforeTimestamp: Option[Spacing] = None,
      spacingAfterTimestamp: Option[Spacing] = None
    ) extends Planning

end models

private[logseq] object blockElements:
  import models.*
  import inlineElements.*

  sealed trait BlockElement extends MarkdownDocument

  case class HeadedSection(
    heading: Heading,
    content: List[BlockElement] = Nil,
    override val indentation: Option[Indentation] = None
  ) extends BlockElement
      with MaybeIndentable

  case class Heading(
    level: Heading.Level,
    content: Option[InlineContainer] = None,
    status: Option[Status] = None,
    priority: Option[Priority] = None,
    propertyDrawer: Option[PropertyDrawer] = None
  ) extends BlockElement
  object Heading:
    case class Level(value: Int, override val spacingAfter: Spacing = Spacing.defaultSpacing)
        extends RightSpaced

  case class Paragraph(
    content: InlineContainer = InlineContainer.empty,
    propertyDrawer: Option[PropertyDrawer] = None,
    customProperties: Option[CustomProperties] = None,
    planning: List[Planning] = Nil,
    status: Option[Status] = None,
    priority: Option[Priority] = None,
    override val indentation: Option[Indentation] = Some(
      Indentation.defaultIndentation
    ) // TODO: It should be rather None
  ) extends BlockElement
      with MaybeIndentable

  sealed trait MarkdownList extends BlockElement with MaybeIndentable
  object MarkdownList:
    case class Ordered(
      items: List[Item] = Nil,
      override val indentation: Option[Indentation] = None
    ) extends MarkdownList
    case class Unordered(
      items: List[Item] = Nil,
      override val indentation: Option[Indentation] = None
    ) extends MarkdownList
    case class Item(
      content: List[BlockElement],
      marker: Item.Marker
    ) extends RightSpaced
    object Item:
      case class Marker(value: String, override val spacingAfter: Spacing = Spacing.defaultSpacing)
          extends RightSpaced

  case class Blockquote(content: List[BlockElement]) extends BlockElement

  case class CodeBlock(
    content: String,
    metadata: Option[String] = None
  ) extends BlockElement

  sealed trait BeginEndBlock extends BlockElement with MaybeIndentable:
    def name: String
    def content: String
  object BeginEndBlock:
    case class LogseqQuery(
      override val content: String,
      override val indentation: Option[Indentation] = None
    ) extends BeginEndBlock:
      override val name: String = "QUERY"
    case class Custom(
      override val content: String,
      override val name: String,
      override val indentation: Option[Indentation] = None
    ) extends BeginEndBlock

  case class HorizontalRuler(value: String = "---") extends BlockElement with MaybeIndentable

  case class Table(
    rows: List[Table.Row],
    override val indentation: Option[Indentation] = None
  ) extends BlockElement
      with MaybeIndentable

  object Table:
    sealed trait Row
    object Row:
      case object Separator extends Row
      case class Cells(cells: List[Cell]) extends Row
    case class Cell(content: InlineContainer)

end blockElements

private[logseq] object inlineElements:
  sealed trait InlineElement

  case class InlineContainer(
    elements: List[InlineElement] = Nil
  ) extends InlineElement
  object InlineContainer:
    val empty: InlineContainer = InlineContainer()

  case class Text(content: String) extends InlineElement

  case class Emphasis(
    marker: Emphasis.Marker,
    contents: InlineContainer
  ) extends InlineElement

  object Emphasis:
    sealed trait Marker:
      def value: String
    object Marker:
      case class Bold(override val value: String) extends Marker
      case class Italic(override val value: String) extends Marker
      case class StrikeThrough(override val value: String) extends Marker
      case class Code(override val value: String) extends Marker
      case class Highlight(override val value: String) extends Marker

  sealed trait Link extends InlineElement
  object Link:

    sealed trait Internal extends Link
    object Internal:

      case class Classic(
        location: Link.Location.Internal,
        text: Option[String] = None
      ) extends Internal

      sealed trait Tag extends Internal:
        def location: Location.Internal.Page
      object Tag:
        case class WithBrackets(override val location: Location.Internal.Page) extends Tag
        case class WithoutBrackets(override val location: Location.Internal.Page) extends Tag

    end Internal

    case class External(
      location: Location.External,
      text: Option[String] = None
    ) extends Link

    sealed trait Location
    object Location:
      case class External(value: String) extends Location
      sealed trait Internal extends Location
      object Internal:
        case class Page(value: String) extends Internal
        case class Block(value: String) extends Internal

  case class Image(altText: String, url: String, title: Option[String]) extends InlineElement

  sealed trait LineBreak extends InlineElement
  object LineBreak:
    case object Softbreak extends LineBreak
    case object Hardbreak extends LineBreak

  sealed trait Timestamp extends InlineElement
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

    case class InactiveTimestampRange(
      from: InactiveTimestamp,
      to: InactiveTimestamp
    ) extends Timestamp
        with Timestamp.Inactive
        with Timestamp.Range

    case class Time(hour: Time.Hour, minute: Time.Minute)

    object Time:
      case class Hour(value: Int)
      case class Minute(value: Int)

    case class Date(
      year: Date.Year,
      month: Date.Month,
      day: Date.Day,
      dayName: Option[Date.DayName] = None
    )

    object Date:
      case class Year(value: Int)
      case class Month(value: Int)
      case class Day(value: Int)

      enum DayName:
        case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday

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

end inlineElements
