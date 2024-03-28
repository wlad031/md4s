package dev.vgerasimov.md4s.logseq

import dev.vgerasimov.md4s.logseq.models.*

object Updater {

  /** Represents a change to be made to a [[Document]].
    *
    * @param originalBlock
    *   The block to be replaced.
    * @param newBlock
    *   The block to replace the original block.
    */
  case class Change(originalBlock: Block, newBlock: Block)
}

/** Updates a [[Document]] with a list of [[Updater.Change]]s. */
class Updater {
  import Updater.*

  /** Updates a [[Document]] with a list of [[Updater.Change]]s.
    *
    * The method iterates over the list of document blocks, trying to find any block which is equal
    * to some of [[Updater.Change.originalBlock]]. If the block is found, it is replaced with the
    * corresponding [[Updater.Change.newBlock]]. If current block can be a parent of other blocks,
    * the method recursively updates all children blocks. Hovewer, if such block itself is being
    * replaced with a new block, the algorithm will not try to update its children.
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
    document.copy(blocks = document.blocks.map(updateBlock(_, changes)))
  }

  private def updateBlock(block: Block, inputs: List[Updater.Change]): Block = {
    inputs.find {
      case Change(originalBlock, newBlock) if block == originalBlock => true
      case _                                                         => false
    } match {
      case Some(Change(_, newBlock)) => newBlock
      case None =>
        block match {
          case b @ HeadedSection(_, children, _) =>
            b.copy(blocks = children.map(updateBlock(_, inputs)))
          case b @ MarkdownList.Unordered(children, _) =>
            b.copy(items =
              children.map(item => item.copy(blocks = item.blocks.map(updateBlock(_, inputs))))
            )
          case b => b
        }
    }
  }
}
