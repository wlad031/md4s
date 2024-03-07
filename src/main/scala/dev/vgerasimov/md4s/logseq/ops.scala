package dev.vgerasimov.md4s
package logseq

import models.*

import scala.reflect.{ ClassTag, classTag }

object ops:

  object table:
    import models.Table
    import models.Table.*
    import models.Table.Row.*

    extension (table: Table) def + (that: Row): Table = Table(table.rows ++ List(that))

    extension (row: Row)
      def + (that: Row): Table = Table(List(row, that))
      def asTable: Table = Table(List(row))

    extension (cells: Cells) def | (cell: Cell): Cells = Cells(cells.cells ++ List(cell))

    extension (cell: Cell)
      def | (that: Cell): Cells = Cells(List(cell, that))
      def asRow: Cells = Cells(List(cell))

    def $ : String => Cell = content => Cell(InlineContainer(List(Text(content))))
    def sep: Separator.type = Separator

  private def fold[A >: B, B : ClassTag](
    objects: List[A],
    op: (B, B) => B
  ): List[A] = {
    def aIsB(a: A): Boolean = classTag[B].runtimeClass.isInstance(a)
    objects
      .foldLeft[List[A]](Nil)((ls, item) =>
        item match {
          case x if aIsB(x) =>
            ls match {
              case ::(head, next) =>
                if (aIsB(head))
                  op(head.asInstanceOf[B], x.asInstanceOf[B]) :: next
                else x :: head :: next
              case Nil => List(x)
            }
          case x => x :: ls
        }
      )
      .reverse
  }

  extension (text: Text) def ++ (that: Text): Text = Text(text.content ++ that.content)

  def listItem(content: String): MarkdownList.Item =
    MarkdownList.Item(List(Paragraph(InlineContainer(List(Text(content))))))

  def h(content: String, level: Int, propertyDrawer: Option[PropertyDrawer] = None): Heading =
    Heading(Some(InlineContainer(List(Text(content)))), level, propertyDrawer = propertyDrawer)

  private[md4s] def foldTexts[A >: Text](objects: List[A]): List[A] =
    fold[A, Text](objects, _ ++ _)
