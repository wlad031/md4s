package dev.vgerasimov.md4s
package obsidian
package models

import elements.*
import spacing.*

object blocks {

  sealed trait Block

  case class Heading(
    level: Int,
    elements: List[Element] = Nil,
    override val indentation: Option[Indentation] = None
  ) extends Block
      with MaybeIndentable

  case class Paragraph(
    elements: List[Element] = Nil,
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
      marker: Item.Marker,
      checkbox: Option[Item.Checkbox] = None
    ) extends RightSpaced

    object Item:
      case class Marker(value: String, override val spacingAfter: Spacing = Spacing.default())
          extends RightSpaced
      case class Checkbox(
        checked: Boolean,
        override val spacingAfter: Option[Spacing] = Some(Spacing.default())
      ) extends MaybeRightSpaced

  case class Blockquote(blocks: List[Block]) extends Block

  case class Callout(
    calloutType: String,
    title: List[Element] = Nil,
    blocks: List[Block] = Nil,
    foldable: Option[Boolean] = None
  ) extends Block

  case class CodeBlock(
    content: String,
    metadata: Option[String] = None,
    override val indentation: Option[Indentation] = None
  ) extends Block
      with MaybeIndentable

  case class HorizontalRule(value: String = "---") extends Block

  case class Table(rows: List[Table.Row]) extends Block

  object Table:
    sealed trait Row extends MaybeIndentable
    object Row:
      case class Separator(value: String, override val indentation: Option[Indentation] = None)
          extends Row
      case class Cells(cells: List[Cell], override val indentation: Option[Indentation] = None)
          extends Row
    case class Cell(elements: List[Element])

  case class FootnoteDefinition(
    id: String,
    elements: List[Element] = Nil,
    override val indentation: Option[Indentation] = None
  ) extends Block
      with MaybeIndentable

  case class CommentBlock(content: String) extends Block

}
