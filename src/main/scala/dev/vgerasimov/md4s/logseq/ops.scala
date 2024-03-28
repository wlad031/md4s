package dev.vgerasimov.md4s
package logseq

import models.*

import scala.reflect.{ ClassTag, classTag }

object ops {

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

  def listItem(content: Element, marker: String): MarkdownList.Item =
    MarkdownList.Item(
      List(Paragraph(List(content))),
      MarkdownList.Item.Marker(marker, spacingAfter = Spacing.default())
    )

  def listItem(content: String, marker: String): MarkdownList.Item = listItem(Text(content), marker)

  def classicInternalLink(
    page: String,
    text: Option[String] = None
  ): Link.Internal.Classic =
    Link.Internal.Classic(Link.Location.Internal.Page(page), text)

  def h(
    content: String,
    level: Int,
    status: Option[String] = None,
    propertyDrawer: Option[PropertyDrawer] = None
  ): Heading = h(List(Text(content)), level, status, propertyDrawer)

  def h(
    content: List[Element],
    level: Int,
    status: Option[String],
    propertyDrawer: Option[PropertyDrawer]
  ): Heading =
    Heading(
      Heading.Level(level),
      content,
      status = status.map(s => Status(s, spacingAfter = Some(Spacing.default()))),
      propertyDrawer = propertyDrawer
    )

  private[md4s] def foldTexts[A >: Text](objects: List[A]): List[A] =
    fold[A, Text](objects, _ ++ _)

  object properties {
    import PropertyDrawer.*

    extension (propertyDrawer: PropertyDrawer) {

      def append(node: Node): PropertyDrawer =
        propertyDrawer.copy(nodes = propertyDrawer.nodes :+ node)
      def :+ (node: Node): PropertyDrawer = append(node)

      def prepend(node: Node): PropertyDrawer =
        propertyDrawer.copy(nodes = node :: propertyDrawer.nodes)
      def :: (node: Node): PropertyDrawer = prepend(node)
    }

    extension (maybePropertyDrawer: Option[PropertyDrawer]) {
      def append(node: PropertyDrawer.Node): PropertyDrawer = maybePropertyDrawer match {
        case None                 => PropertyDrawer(List(node))
        case Some(propertyDrawer) => propertyDrawer.append(node)
      }
    }

    def node(key: String, value: String): Node =
      Node(Node.Key(key), Node.Value(List(Text(value))))
  }

  object blocks {
    import ops.properties.{ *, given }

    extension (block: Block) {
      def appendPropertyNode(node: PropertyDrawer.Node): Block = block match {
        case b @ HeadedSection(h @ Heading(_, _, _, _, maybePropertyDrawer), _, _) =>
          b.copy(heading = h.copy(propertyDrawer = Some(maybePropertyDrawer.append(node))))
        case b @ Paragraph(_, maybePropertyDrawer, _, _, _, _, _) =>
          b.copy(propertyDrawer = Some(maybePropertyDrawer.append(node)))
        case b => b
      }
    }

    object table {
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

      def $ : String => Cell = content => Cell(List(Text(content)))
      def sep: Separator.type = Separator
    }
  }

}
