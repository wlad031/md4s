package dev.vgerasimov.md4s
package logseq
package models

import elements.*
import spacing.*

object properties {

  case class Status(value: String, override val spacingAfter: Option[Spacing] = None)
      extends MaybeRightSpaced

  case class Priority(value: Char, override val spacingAfter: Option[Spacing] = None)
      extends MaybeRightSpaced

  case class PropertyDrawer(nodes: List[PropertyDrawer.Node])

  object PropertyDrawer {

    case class Node(
      key: Node.Key,
      value: Node.Value,
      override val indentation: Option[Indentation] = None
    ) extends MaybeIndentable

    object Node {

      case class Key(
        value: String,
        override val spacingAfter: Option[Spacing] = Some(Spacing(" "))
      ) extends MaybeRightSpaced

      case class Value(elements: List[Element])
    }
  }

  case class CustomProperties(blockName: String, content: String)

  sealed trait Planning {
    def timestamp: Timestamp
  }

  object Planning {

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
  }
}
