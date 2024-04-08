package dev.vgerasimov.md4s.logseq

import dev.vgerasimov.common.syntax.{ *, given }
import dev.vgerasimov.md4s.logseq.models.*

object Updater {

  sealed trait Change
  object Change {
    case class Replacement(originalBlock: Block, newBlock: Block) extends Change
    case class Append(block: Block) extends Change
  }
}

/** Updates a [[Document]] with a list of [[Updater.Change]]s. */
class Updater {
  import Updater.*

  /** Updates a [[Document]] with a list of [[Updater.Change]]s.
    *
    * @param document
    *   The document to be updated.
    * @param changes
    *   The list of changes to be made.
    * @return
    *   The updated document.
    */
  def update(
    document: Document,
    changes: List[Change]
  ): Document = {
    document.copy(blocks = document.blocks.map(updateBlock(_, changes)) ++ changes.map {
      case Change.Append(block) => Some(block)
      case _                    => None
    }.filterNonEmpty)
  }

  private def updateBlock(block: Block, changes: List[Updater.Change]): Block = {
    changes.find {
      case Change.Replacement(originalBlock, newBlock) if block == originalBlock => true
      case _                                                                     => false
    } match {
      case Some(Change.Replacement(_, newBlock)) => newBlock
      case None =>
        block match {
          case b @ HeadedSection(_, children, _) =>
            b.copy(blocks = children.map(updateBlock(_, changes)))
          case b @ MarkdownList.Unordered(children, _) =>
            b.copy(items =
              children.map(item => item.copy(blocks = item.blocks.map(updateBlock(_, changes))))
            )
          case b => b
        }
      case _ => sys.error("Not reachable")
    }
  }
}
