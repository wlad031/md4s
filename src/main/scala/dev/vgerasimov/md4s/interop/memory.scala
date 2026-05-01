package dev.vgerasimov.md4s
package interop

import java.util.{List as JList, Optional}

import scala.jdk.CollectionConverters.*

import dev.vgerasimov.md4s.obsidian
import dev.vgerasimov.md4s.obsidian.models.*

object ObsidianMarkdownOps:

  private val parser = new obsidian.Parser()
  private val formatter = obsidian.Formatter()

  def hasLinkOrTag(content: String): Boolean =
    val doc = parse(content)
    doc.blocks.exists(blockHasLinkOrTag)

  def frontmatterValue(content: String, key: String): Option[String] =
    val doc = parse(content)
    doc.frontmatter.flatMap(_.values.get(key))

  def appendLinksAndTags(content: String, links: List[String], tags: List[String]): String =
    val doc = parse(content)
    val linkElements = normalizeLinks(links).flatMap { link =>
      List(Link.Wiki(target = Link.Wiki.Target(path = Some(link))), Text(" "))
    }
    val tagElements = normalizeTags(tags).flatMap { tag =>
      List(Tag(tag), Text(" "))
    }
    val merged = (linkElements ++ tagElements).reverse.dropWhile {
      case Text(content) if content.trim.isEmpty => true
      case _                                      => false
    }.reverse

    val paragraph = Paragraph(elements = merged)
    val updated = doc.copy(blocks = doc.blocks :+ paragraph)
    formatter.format(updated)

  private def parse(content: String): Document =
    parser.document(Option(content).getOrElse("")) match
      case dev.vgerasimov.slowparse.POut.Success(value, _, _, _) => value
      case dev.vgerasimov.slowparse.POut.Failure(message, parserLabel) =>
        throw ParseException(message, parserLabel.orNull)

  private def blockHasLinkOrTag(block: Block): Boolean = block match
    case Heading(_, elements, _)              => elements.exists(elementHasLinkOrTag)
    case Paragraph(elements, _)               => elements.exists(elementHasLinkOrTag)
    case MarkdownList.Ordered(items, _)       => items.exists(item => item.blocks.exists(blockHasLinkOrTag))
    case MarkdownList.Unordered(items, _)     => items.exists(item => item.blocks.exists(blockHasLinkOrTag))
    case Blockquote(blocks)                   => blocks.exists(blockHasLinkOrTag)
    case Callout(_, title, blocks, _)         => title.exists(elementHasLinkOrTag) || blocks.exists(blockHasLinkOrTag)
    case FootnoteDefinition(_, elements, _)   => elements.exists(elementHasLinkOrTag)
    case Table(rows)                          => rows.exists(rowHasLinkOrTag)
    case _                                    => false

  private def rowHasLinkOrTag(row: Table.Row): Boolean = row match
    case Table.Row.Separator(_, _) => false
    case Table.Row.Cells(cells, _) => cells.exists(cell => cell.elements.exists(elementHasLinkOrTag))

  private def elementHasLinkOrTag(element: Element): Boolean = element match
    case _: Link         => true
    case _: Tag          => true
    case Emphasis(_, es) => es.exists(elementHasLinkOrTag)
    case _               => false

  private def normalizeLinks(links: List[String]): List[String] =
    links
      .map(Option(_).map(_.trim).getOrElse(""))
      .filter(_.nonEmpty)
      .map { raw =>
        val unwrapped = raw.stripPrefix("[[").stripSuffix("]]")
        unwrapped.split("\\|").headOption.getOrElse(unwrapped).trim
      }
      .filter(_.nonEmpty)

  private def normalizeTags(tags: List[String]): List[String] =
    tags
      .map(Option(_).map(_.trim).getOrElse(""))
      .filter(_.nonEmpty)
      .map(_.stripPrefix("#"))
      .map(_.replace(" ", "-"))
      .filter(_.nonEmpty)

object ObsidianMarkdownOpsInteropApi:

  def hasLinkOrTag(content: String): Boolean =
    ObsidianMarkdownOps.hasLinkOrTag(content)

  def frontmatterValue(content: String, key: String): Optional[String] =
    Optional.ofNullable(ObsidianMarkdownOps.frontmatterValue(content, key).orNull)

  def appendLinksAndTags(content: String, links: JList[String], tags: JList[String]): String =
    ObsidianMarkdownOps.appendLinksAndTags(
      content,
      Option(links).map(_.asScala.toList).getOrElse(Nil),
      Option(tags).map(_.asScala.toList).getOrElse(Nil)
    )
