package dev.vgerasimov.md4s
package logseq
package models

import elements.*
import properties.*
import spacing.*

object blocks {

  sealed trait Block

  case class HeadedSection(
    heading: Heading,
    blocks: List[Block] = Nil,
    override val indentation: Option[Indentation] = None
  ) extends Block
      with MaybeIndentable

  case class Heading(
    level: Heading.Level,
    elements: List[models.elements.Element] = Nil,
    status: Option[Status] = None,
    priority: Option[Priority] = None,
    planning: List[Planning] = Nil,
    propertyDrawer: Option[PropertyDrawer] = None,
    customProperties: List[CustomProperties] = Nil,
  ) extends Block
  object Heading:
    case class Level(value: Int, override val spacingAfter: Spacing = Spacing.default())
        extends RightSpaced

  case class Paragraph(
    elements: List[models.elements.Element] = Nil,
    status: Option[Status] = None,
    priority: Option[Priority] = None,
    planning: List[Planning] = Nil,
    propertyDrawer: Option[PropertyDrawer] = None,
    customProperties: List[CustomProperties] = Nil,
    override val indentation: Option[Indentation] = None
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
      blocks: List[Block],
      marker: Item.Marker
    ) extends RightSpaced
    object Item:
      case class Marker(value: String, override val spacingAfter: Spacing = Spacing.default())
          extends RightSpaced

  case class Blockquote(blocks: List[Block]) extends Block

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
    case class Cell(elements: List[Element])

}
