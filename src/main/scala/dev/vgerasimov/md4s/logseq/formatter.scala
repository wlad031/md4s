package dev.vgerasimov.md4s
package logseq

import models.*
import ops.{ *, given }

object formatter:

  def format(document: LogseqMarkdown): String = ???

  def format(inlineElement: InlineElement): String = inlineElement match
    case InlineContainer(elements) => elements.map(format).mkString
    case Text(content)             => content
    case Emphasis(marker: Emphasis.Marker, contents) =>
      s"${marker.value}${format(contents)}${marker.value}"
    case _ => ???

  def format(blockElement: BlockElement): String = blockElement match
    case Paragraph(content, None, indentation) =>
      s"${format(indentation)}${format(content)}"
    case Paragraph(content, Some(propertyDrawer), indentation) =>
      s"${format(indentation)}${format(content)}\n${format(propertyDrawer)}"
    case Heading(Some(content), headerLevel, None, None, None, indentation) =>
      s"${format(indentation)}${"#" * headerLevel} ${format(content)}"
    case HeadedSection(heading, content, indentation) =>
      s"${format(heading)}\n${content.map(format).mkString("\n")}"
    case _ => ???

  private def format(indentation: Indentation): String = indentation.value
  private def format(propertyDrawer: PropertyDrawer): String = propertyDrawer.nodes
    .map(node => {
      node.value match
        case Some(value) => s"${node.name}:: ${format(value)}"
        case None        => s"${node.name}::"
    })
    .mkString("\n")

end formatter
