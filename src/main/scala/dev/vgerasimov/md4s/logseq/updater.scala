package dev.vgerasimov.md4s.logseq

import dev.vgerasimov.md4s.logseq.models.*
import dev.vgerasimov.md4s.logseq.Updater.Input

class Updater extends ((LogseqMarkdown, List[Updater.Input]) => LogseqMarkdown) {

  override def apply(
    originalDocument: LogseqMarkdown,
    unputs: List[Updater.Input]
  ): LogseqMarkdown = {
    originalDocument.copy(blocks = originalDocument.blocks.map(updateBlock(_, unputs)))
  }

  private def updateBlock(block: Block, inputs: List[Updater.Input]): Block = {
    inputs.find {
      case Input(originalBlock, newBlock) if block == originalBlock => true
      case _                                                        => false
    } match {
      case Some(Input(_, newBlock)) => newBlock
      case None =>
        block match {
          case b @ HeadedSection(_, children, _) =>
            b.copy(content = children.map(updateBlock(_, inputs)))
          case b @ MarkdownList.Unordered(children, _) =>
            b.copy(items =
              children.map(item => item.copy(content = item.content.map(updateBlock(_, inputs))))
            )
          case b => b
        }
    }
  }
}

object Updater {
  case class Input(originalBlock: Block, newBlock: Block)
}
