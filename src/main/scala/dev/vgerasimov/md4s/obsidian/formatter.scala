package dev.vgerasimov.md4s
package obsidian

import models.*

import scala.collection.mutable.StringBuilder

trait Formatter {
  def format(document: Document): String
  def format(blocks: List[Block]): String
}

object Formatter {
  def apply(): Formatter = FormatterImpl
}

private[obsidian] object FormatterImpl extends Formatter {

  override def format(document: Document): String =
    document match
      case Document(maybeFrontmatter, blocks) =>
        val res = StringBuilder()
        maybeFrontmatter.foreach { frontmatter =>
          res.append("---\n")
          res.append(frontmatter.raw)
          res.append("\n---")
        }
        if blocks.nonEmpty then
          if res.nonEmpty then res.append("\n")
          res.append(format(blocks))
        res.toString()

  override def format(blocks: List[Block]): String =
    blocks.map(format(_, indentLevel = 0)).mkString("\n")

  def format(block: Block): String =
    format(block, indentLevel = 0)

  def format(element: Element): String =
    element match
      case Text(content) => content
      case Emphasis(marker, elements) => marker.value + formatElements(elements) + marker.value
      case Link.External(location, Some(label)) => s"[$label]($location)"
      case Link.External(location, None)        => location
      case Link.Wiki(target, alias, embed) =>
        val value = formatWikiTarget(target)
        val aliasPart = alias.map(v => s"|$v").getOrElse("")
        val marker = if embed then "!" else ""
        s"$marker[[$value$aliasPart]]"
      case Tag(value) => s"#$value"
      case Image(altText, url, maybeTitle) =>
        val titlePart = maybeTitle.map(v => s" \"$v\"").getOrElse("")
        s"![$altText]($url$titlePart)"
      case FootnoteReference(id) => s"[^$id]"
      case Comment(content)      => s"%%$content%%"

  private[md4s] def formatElements(elements: List[Element]): String =
    elements.map(format).mkString("")

  private def formatWikiTarget(target: Link.Wiki.Target): String =
    target match
      case Link.Wiki.Target(path, heading, blockId) =>
        val base = path.getOrElse("")
        blockId match
          case Some(id) => s"$base#^$id"
          case None =>
            heading match
              case Some(h) => s"$base#$h"
              case None    => base

  private def format(block: Block, indentLevel: Int): String =
    val indent = "  " * indentLevel
    block match
      case Heading(level, elements, _) =>
        s"$indent${"#" * level} ${formatElements(elements)}"

      case Paragraph(elements, _) =>
        s"$indent${formatElements(elements)}"

      case MarkdownList.Ordered(items, _) =>
        items.map(formatListItem(_, indentLevel)).mkString("\n")

      case MarkdownList.Unordered(items, _) =>
        items.map(formatListItem(_, indentLevel)).mkString("\n")

      case Blockquote(blocks) =>
        val content = formatNestedBlocks(blocks, indentLevel = 0)
        prefixEachLine(content, s"$indent> ")

      case Callout(calloutType, title, blocks, foldable) =>
        val foldMarker = foldable match
          case Some(true)  => "+"
          case Some(false) => "-"
          case None        => ""
        val titlePart = if title.isEmpty then "" else s" ${formatElements(title)}"
        val header = s"$indent> [!$calloutType]$foldMarker$titlePart"
        if blocks.isEmpty then header
        else
          val body = prefixEachLine(formatNestedBlocks(blocks, 0), s"$indent> ")
          s"$header\n$body"

      case CodeBlock(content, metadata, _) =>
        val meta = metadata.getOrElse("")
        val body = if content.isEmpty then "" else s"\n${prefixEachLine(content, indent)}"
        s"$indent```$meta$body\n$indent```"

      case HorizontalRule(value) => s"$indent$value"

      case Table(rows) =>
        rows.map {
          case Table.Row.Separator(value, _) => s"$indent$value"
          case Table.Row.Cells(cells, _) =>
            val renderedCells = cells.map(cell => formatElements(cell.elements))
            s"$indent${renderedCells.mkString("|", "|", "|")}"
        }.mkString("\n")

      case FootnoteDefinition(id, elements, _) =>
        s"$indent[^$id]: ${formatElements(elements)}"

      case CommentBlock(content) =>
        s"$indent%%\n${prefixEachLine(content, indent)}\n$indent%%"

  private def formatListItem(item: MarkdownList.Item, indentLevel: Int): String =
    val indent = "  " * indentLevel
    val marker = item.marker.value + item.marker.spacingAfter.value
    val checkboxPart = item.checkbox.map { box =>
      val value = if box.checked then "[x]" else "[ ]"
      value + box.spacingAfter.map(_.value).getOrElse("")
    }.getOrElse("")

    val firstLine = item.blocks.headOption match
      case Some(Paragraph(elements, _)) =>
        s"$indent$marker$checkboxPart${formatElements(elements)}"
      case Some(Heading(level, elements, _)) =>
        s"$indent$marker$checkboxPart${"#" * level} ${formatElements(elements)}"
      case Some(block) =>
        s"$indent$marker$checkboxPart${format(block, indentLevel = 0)}"
      case None =>
        s"$indent$marker$checkboxPart"

    val tailBlocks = item.blocks.drop(1)
    if tailBlocks.isEmpty then firstLine
    else s"$firstLine\n${formatNestedBlocks(tailBlocks, indentLevel + 1)}"

  private def formatNestedBlocks(blocks: List[Block], indentLevel: Int): String =
    blocks.map(format(_, indentLevel)).mkString("\n")

  private def prefixEachLine(content: String, prefix: String): String =
    content.split("\n", -1).map(line => s"$prefix$line").mkString("\n")

}
