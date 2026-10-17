/*  Title:      site/src/source.scala

The defining command of an entity (statement + proof) from a theory, rendered
from its PIDE markup with entity links and typing hovers.
*/

package site

import isabelle._


object Source {
  /* theory elements, from the source text of a snapshot */

  type Element = Thy_Element.Element_Command

  sealed case class Elements(snapshot: Document.Snapshot, elements: List[Element], start: Map[Command, Text.Offset]) {
    def range(e: Element): Text.Range =
      Text.Range(start(e.head), start(e.last) + e.last.span.length)

    def containing(offset: Text.Offset): Option[Element] =
      elements.find(e => range(e).contains(offset))
  }

  def elements(syntax: Outer_Syntax, snapshot: Document.Snapshot): Elements = {
    val commands =
      syntax.parse_spans(snapshot.node.source).map(span =>
        Command(Document_ID.none, snapshot.node_name, Command.Blobs_Info.empty, span))
    val start = commands.scanLeft(0)(_ + _.span.length).zip(commands).map(_.swap).toMap
    Elements(snapshot, Thy_Element.parse_elements(commands), start)
  }


  /* HTML rendering of PIDE markup */

  trait Links {
    def entity(kind: String, name: String): Option[String]  // href
  }
  object No_Links extends Links { def entity(kind: String, name: String): Option[String] = None }

  val hover_markup: Set[String] = Set(Markup.TYPING, Markup.ML_TYPING, Markup.SORTING)
  val entity_kinds: Set[String] =
    Set(Markup.THEORY, Markup.TYPE_NAME, Markup.CONSTANT, Markup.FACT, Markup.CLASS, Markup.LOCALE)

  def html(xml: XML.Body, links: Links): XML.Body = {
    def attr(name: String, value: String): HTML.Attribute = new HTML.Attribute(name, value)

    def body(trees: XML.Body): XML.Body = trees.flatMap(tree)

    def tree(t: XML.Tree): XML.Body =
      t match {
        case XML.Wrapped_Elem(markup, tip, inner) if hover_markup(markup.name) =>
          List(attr("title", Symbol.decode(XML.content(tip)))(HTML.span("typed", body(inner))))
        case XML.Wrapped_Elem(markup, _, inner) => tree(XML.Elem(markup, inner))
        /* a reference links to the definition; the defining occurrence itself is plain text,
           as in browser_info: a package (datatype, class) reports every entity it derives at
           the one binding, and nested links are no HTML */
        case XML.Elem(markup @ Markup.Entity(kind, name), inner) if entity_kinds(kind) && !name.startsWith("local.") =>
          val html = body(inner)
          markup match {
            case Markup.Entity.Def(_) => html
            case _ =>
              links.entity(kind, name) match {
                case Some(href) => List(attr("title", kind + " " + name)(HTML.link(href, html)))
                case None => List(attr("title", kind + " " + name)(HTML.span("entity", html)))
              }
          }
        case XML.Elem(Markup.Url(href), inner) => List(HTML.link(href, body(inner)))
        case XML.Elem(markup, inner) =>
          val html = body(inner)
          val c =
            markup.properties match {
              /* a command keyword is both, as in browser_info: keyword1 colours it, command
                 makes it bold (isabelle.css) */
              case Markup.Kind(kind) if kind == Markup.COMMAND || kind == Markup.KEYWORD =>
                Rendering.get_foreground_text_color(markup) match {
                  case Some(color) => color.toString + " " + kind
                  case None => kind
                }
              case _ if Markup.has_syntax(markup.properties) => ""
              case _ => Rendering.get_foreground_text_color(markup).map(_.toString).getOrElse("")
            }
          html match {
            /* no duplicate wrapper: the inner span's classes are already among this one's */
            case List(XML.Elem(Markup("span", List(("class", c1))), inner))
                if c1.split(" ").forall(c.split(" ").contains) => List(HTML.span(c, inner))
            case _ => if (c.isEmpty) html else List(HTML.span(c, html))
          }
        case XML.Text(text) => HTML.text(Symbol.decode(text))
      }

    body(xml)
  }

  /* kind: what the block is, for its label ("Isabelle/HOL") */
  def render(snapshot: Document.Snapshot, range: Text.Range, links: Links,
      id: Option[String] = None, kind: Option[String] = None): XML.Elem = {
    val pre0 = HTML.source(html(snapshot.xml_markup(range = range, elements = Markup.Elements.full), links))
    val pre = kind match { case Some(k) => new HTML.Attribute("data-kind", k)(pre0) case None => pre0 }
    id match { case Some(s) => HTML.id(s)(pre) case None => pre }
  }

  /* the entities (kind, name) whose defining occurrence lies in the range: what a source
     display shows the definition of, so a page reference to any of them can point there */
  def defined(snapshot: Document.Snapshot, range: Text.Range): List[(String, String)] = {
    def defs(t: XML.Tree): List[(String, String)] =
      t match {
        case XML.Elem(markup @ Markup.Entity(kind, name), inner) if entity_kinds(kind) =>
          (markup match { case Markup.Entity.Def(_) => List((kind, name)) case _ => Nil }) :::
            inner.flatMap(defs)
        case XML.Elem(_, inner) => inner.flatMap(defs)
        case _ => Nil
      }
    snapshot.xml_markup(range = range, elements = Markup.Elements.full).flatMap(defs).distinct
  }
}
