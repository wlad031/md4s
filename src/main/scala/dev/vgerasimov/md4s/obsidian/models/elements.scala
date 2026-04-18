package dev.vgerasimov.md4s
package obsidian
package models

object elements {

  sealed trait Element

  case class Text(content: String) extends Element

  case class Emphasis(
    marker: Emphasis.Marker,
    elements: List[Element]
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

    case class External(location: String, label: Option[String] = None) extends Link

    case class Wiki(
      target: Wiki.Target,
      alias: Option[String] = None,
      embed: Boolean = false
    ) extends Link

    object Wiki:
      case class Target(
        path: Option[String] = None,
        heading: Option[String] = None,
        blockId: Option[String] = None
      )

  case class Tag(value: String) extends Element

  case class Image(altText: String, url: String, title: Option[String] = None) extends Element

  case class FootnoteReference(id: String) extends Element

  case class Comment(content: String) extends Element

}
