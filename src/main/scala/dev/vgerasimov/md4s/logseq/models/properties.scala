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

      sealed trait Value

      object Value {

        def apply(elements: List[Element]): Value.Classic = Value.Classic(elements)

        case class Classic(elements: List[Element]) extends Value

        case class CommaSeparated(elements: List[CommaSeparated.SpacedElement]) extends Value

        object CommaSeparated {
          case class SpacedElement(
            element: Element,
            override val spacingBefore: Option[Spacing] = None,
            override val spacingAfter: Option[Spacing] = None
          ) extends MaybeLeftSpaced,
                MaybeRightSpaced
        }
      }
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
