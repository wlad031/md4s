package dev.vgerasimov.md4s
package logseq

import dev.vgerasimov.md4s.models.*
import dev.vgerasimov.md4s.logseq.elements.Timestamp.RepeaterOrDelay.Value
import dev.vgerasimov.md4s.logseq.blocks.BeginEndBlock.Custom

object models:
  import blocks.*
  import elements.*

  export blocks.*
  export elements.*

  case class LogseqMarkdown(
    propertyDrawer: Option[PropertyDrawer] = None,
    blocks: List[Block] = Nil
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
    def fromLevel(level: Int, char: String = "  "): Indentation =
      Indentation(level, value = char * level)
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
      key: Node.Key,
      value: Node.Value,
      override val indentation: Option[Indentation] = None
    ) extends MaybeIndentable
    object Node:
      case class Key(
        value: String,
        override val spacingAfter: Option[Spacing] = Some(Spacing(" "))
      ) extends MaybeRightSpaced
      case class Value(value: ElementsContainer)

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

private[logseq] object blocks:
  import models.*
  import elements.*

  sealed trait Block extends MarkdownDocument

  case class HeadedSection(
    heading: Heading,
    content: List[Block] = Nil,
    override val indentation: Option[Indentation] = None
  ) extends Block
      with MaybeIndentable

  case class Heading(
    level: Heading.Level,
    content: Option[ElementsContainer] = None,
    status: Option[Status] = None,
    priority: Option[Priority] = None,
    propertyDrawer: Option[PropertyDrawer] = None
  ) extends Block
  object Heading:
    case class Level(value: Int, override val spacingAfter: Spacing = Spacing.defaultSpacing)
        extends RightSpaced

  case class Paragraph(
    content: ElementsContainer = ElementsContainer.empty,
    propertyDrawer: Option[PropertyDrawer] = None,
    customProperties: List[CustomProperties] = Nil,
    planning: List[Planning] = Nil,
    status: Option[Status] = None,
    priority: Option[Priority] = None,
    override val indentation: Option[Indentation] = Some(
      Indentation.defaultIndentation
    ) // TODO: It should be rather None
  ) extends Block
      with MaybeIndentable

  sealed trait MarkdownList extends Block with MaybeIndentable
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
      content: List[Block],
      marker: Item.Marker
    ) extends RightSpaced
    object Item:
      case class Marker(value: String, override val spacingAfter: Spacing = Spacing.defaultSpacing)
          extends RightSpaced

  case class Blockquote(content: List[Block]) extends Block

  case class CodeBlock(
    content: String,
    metadata: Option[String] = None,
    override val indentation: Option[Indentation] = None
  ) extends Block
      with MaybeIndentable

  sealed trait BeginEndBlock extends Block with MaybeIndentable:
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
      override val name: String, // TODO: name is not a good name, maybe type?
      override val indentation: Option[Indentation] = None
    ) extends BeginEndBlock

  case class HorizontalRuler(value: String = "---") extends Block with MaybeIndentable

  case class Table(
    rows: List[Table.Row]
  ) extends Block

  object Table:
    sealed trait Row extends MaybeIndentable
    object Row:
      case class Separator(value: String, override val indentation: Option[Indentation] = None)
          extends Row
      case class Cells(cells: List[Cell], override val indentation: Option[Indentation] = None)
          extends Row
    case class Cell(content: ElementsContainer)

end blocks

private[logseq] object elements:
  sealed trait Element

  case class ElementsContainer(
    elements: List[Element] = Nil
  ) extends Element
  object ElementsContainer:
    val empty: ElementsContainer = ElementsContainer()

  case class Text(content: String) extends Element

  case class Emphasis(
    marker: Emphasis.Marker,
    contents: ElementsContainer
  ) extends Element

  object Emphasis:
    sealed trait Marker:
      def value: String
    object Marker:
      case class Bold(override val value: String) extends Marker
      case class Italic(override val value: String) extends Marker
      case class StrikeThrough(override val value: String) extends Marker
      case class Code(override val value: String) extends Marker
      case class Highlight(override val value: String) extends Marker

  sealed trait Link extends Element
  object Link:

    sealed trait Internal extends Link:
      def location: Location.Internal
    object Internal:

      case class Classic(
        override val location: Link.Location.Internal,
        label: Option[String] = None
      ) extends Internal

      sealed trait Tag extends Internal:
        override def location: Location.Internal.Page
      object Tag:
        case class WithBrackets(override val location: Location.Internal.Page) extends Tag
        case class WithoutBrackets(override val location: Location.Internal.Page) extends Tag

    end Internal

    case class External(
      location: Location.External,
      label: Option[String] = None
    ) extends Link

    sealed trait Location
    object Location:
      case class External(value: String) extends Location
      sealed trait Internal extends Location
      object Internal:
        case class Page(value: String) extends Internal
        case class Block(value: String) extends Internal

  sealed trait SimpleBlock extends Element
  object SimpleBlock:
    case class Query(value: String) extends SimpleBlock
    case class Video(location: Link.Location.External) extends SimpleBlock

  case class Image(altText: String, url: String, title: Option[String]) extends Element

  sealed trait LineBreak extends Element
  object LineBreak:
    case object Softbreak extends LineBreak
    case object Hardbreak extends LineBreak

  sealed trait Timestamp extends Element
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

end elements
