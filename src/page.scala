/*  Title:      site/src/page.scala

Page sources: theories whose top-level commands are page items. The site's own
commands export their checked item tree (thy/site.ML); the document commands
of the enclosing system (text, section, ...) are read from the source. Both
come from Isabelle's parser, which gives every command the symbol offset that
keys its export.
*/

package site

import isabelle._


object Page {
  val headings: Map[String, Int] = Map("page" -> 1, "section" -> 2, "subsection" -> 3)


  /* model */

  sealed abstract class Item { def line: Int }
  case class Heading(level: Int, title: String, attrs: List[(String, String)], line: Int) extends Item
  case class Text(text: String, line: Int) extends Item
  case class Elem(tag: String, attrs: List[(String, String)], items: List[Item], line: Int) extends Item
  case class Block(audience: String, attrs: List[(String, String)], items: List[Item], line: Int) extends Item
  case class Verbatim(tag: String, attrs: List[(String, String)], text: String, line: Int) extends Item
  // a body its command checked: rendered from the markup reported on range (symbol offsets)
  case class Checked(name: String, attrs: List[(String, String)], text: String, range: Properties.T, line: Int) extends Item
  case class Html(text: String, line: Int) extends Item
  case class Thm(thms: XML.Body, line: Int) extends Item   // the facts as printed (Thm_Display)
  case class Source(name: String, kind: Option[String], line: Int) extends Item

  /* the outermost blocks, at any container depth: what a reader without a profile misses */
  def outer_blocks(items: List[Item]): List[Block] =
    items.flatMap({
      case b: Block => List(b)
      case Elem(_, _, inner, _) => outer_blocks(inner)
      case _ => Nil
    })

  /* all items in document order, descending into containers */
  def flatten(items: List[Item]): List[Item] =
    items.flatMap({
      case Elem(_, _, inner, _) => flatten(inner)
      case Block(_, _, inner, _) => flatten(inner)
      case it => List(it)
    })

  /* optional content: characters of prose inside blocks over characters of all prose */
  def optional_ratio(items: List[Item]): Double = {
    def chars(items: List[Item]): Int =
      flatten(items).collect({ case Text(text, _) => text.length }).sum
    val all = chars(items)
    val optional = outer_blocks(items).map(b => chars(b.items)).sum
    if (all == 0) 0.0 else optional.toDouble / all
  }

  val default_optional_limit = 0.3

  sealed case class Page(path: Path, name: String, theory: String, title: String,
      items: List[Item], is_index: Boolean, optional_limit: Double,
      part: Option[String],               // the part of the site this page belongs to
      order: Int,                         // its place within that part
      control: Option[List[String]]       // axes whose dials the header offers; None = those its blocks mention
  ) {
    def where(line: Int): String = path.implode + ":" + line + ": "
    def file_name: String = (if (is_index) "index" else name) + ".html"   // the landing page
    lazy val optional_ratio: Double = site.Page.optional_ratio(items)
  }


  /* the exported tree of an item: name, kind and line, attr elements, then the content */

  def error_at(path: Path, line: Int, msg: String): Nothing =
    error(path.implode + ":" + line + ": " + msg)

  private def decode(path: Path)(tree: XML.Tree): Item = {
    def prop(props: Properties.T, name: String): String =
      props.find(_._1 == name).map(_._2) getOrElse error("Malformed page item export: no " + name)
    def line(props: Properties.T): Int = Value.Int.parse(prop(props, "line"))
    def is_attr(t: XML.Tree): Boolean = t match { case XML.Elem(Markup("attr", _), _) => true case _ => false }

    tree match {
      case XML.Elem(Markup("text", props), body) => Text(XML.content(body), line(props))
      case XML.Elem(Markup("item", props), body) =>
        val name = prop(props, "name")
        val ln = line(props)
        val attrs =
          body.collect({ case XML.Elem(Markup("attr", a), _) => prop(a, "key") -> prop(a, "value") })
        val content = body.filterNot(is_attr)
        def text: String =
          content match {
            case List(XML.Elem(Markup("body", _), b)) => XML.content(b)
            case _ => error_at(path, ln, "Malformed page item export: " + name + " without a body")
          }
        def items: List[Item] = content.map(decode(path))
        def classes: List[String] =
          attrs.find(_._1 == "class").map(_._2.split(" ").toList).getOrElse(Nil)
        headings.get(name) match {
          case Some(level) => Heading(level, text, attrs, ln)
          case None =>
            prop(props, "kind") match {
              case "prose" => Text(text, ln)
              case "container" => Elem(name, attrs, items, ln)
              case "block" =>
                val attrs1 = ("class" -> ("block " + (name :: classes).distinct.mkString(" ")).trim) ::
                  attrs.filterNot(a => a._1 == "audience" || a._1 == "class")
                Block(prop(props, "audience"), attrs1, items, ln)
              case "thms" => Thm(content, ln)
              case "verbatim" if name == "html" => Html(text, ln)
              case "verbatim" if name == "source" =>
                Source(text.trim, attrs.find(_._1 == "kind").map(_._2), ln)
              case "verbatim" => Verbatim(name, attrs, text, ln)
              case "checked" =>
                val range = props.filter(p => p._1 == Markup.OFFSET || p._1 == Markup.END_OFFSET)
                Checked(name, attrs, text, range, ln)
              case kind => error_at(path, ln, "Malformed page item export: kind " + quote(kind))
            }
        }
      case t => error("Malformed page item export: " + XML.string_of_tree(t).take(80))
    }
  }


  /* the page: its top-level commands in document order; None for a theory without a page
     command, which is one of the theories that configure the site */

  def parse(store: Site_Store, config: Config.Config, path: Path, name: String, theory: String)
      : Option[Page] = {
    val exports = store.page_items(theory)

    def item(span: Command_Span.Span, pos: Token.Pos): List[Item] = {
      val cmd = span.name
      val line = pos.line
      def body: String =
        span.content.filter(_.is_proper).drop(1) match {
          case List(t) if t.kind == Token.Kind.CARTOUCHE || t.is_string => t.content
          case _ => error_at(path, line, "Command " + cmd + " needs a cartouche body")
        }
      if (cmd == "theory" || cmd == "end") Nil
      else config.commands.get(cmd) match {
        case None => error_at(path, line, "Command " + quote(cmd) + " is not a page command")
        case Some(command) =>
          exports.get(pos.offset) match {
            case Some(List(tree)) => List(decode(path)(tree))
            case Some(_) => error_at(path, line, "Malformed page item export for " + cmd)
            case None if command.external =>
              headings.get(cmd) match {
                case Some(level) => List(Heading(level, body, Nil, line))
                case None => List(Text(body, line))
              }
            case None =>
              error_at(path, line, "No export for " + cmd + ": session " + store.session +
                " is stale, rebuild it")
          }
      }
    }

    /* every command is read, so a theory without a page command is known to be no page
       before its other commands are judged, and a page reports all its errors together */
    val results = {
      var pos = Token.Pos.start
      for (span <- store.syntax.parse_spans(File.read(path))) yield {
        val result =
          span.kind match {
            case _: Command_Span.Command_Span => Exn.capture(item(span, span.keyword_pos(pos)))
            case Command_Span.Malformed_Span =>
              Exn.capture(error_at(path, pos.line, "Malformed command"))
            case _ => Exn.Res(Nil)
          }
        pos = span.content.foldLeft(pos)(_ advance _)
        result
      }
    }
    val items = results.collect({ case Exn.Res(items) => items }).flatten
    val errors = results.collect({ case Exn.Exn(ERROR(msg)) => msg })

    /* the page command is a top-level command, once: its attributes were checked by
       Site.check_page_attrs, which runs for top-level page commands only */
    def is_title(it: Item): Boolean = it match { case Heading(1, _, _, _) => true case _ => false }
    val titles = items.collect({ case h @ Heading(1, _, _, _) => h })
    val rest = items.filterNot(is_title)
    for (h <- flatten(rest).collect({ case h @ Heading(1, _, _, _) => h }))
      error_at(path, h.line, "The page command belongs at the top level, not in a container")
    titles match {
      case Nil => None
      case _ if errors.nonEmpty => error(cat_lines(errors))
      case List(h) =>
        def attr(key: String): Option[String] = h.attrs.find(_._1 == key).map(_._2)
        /* "page (optional: 55)": how much of the prose may sit in blocks, as a percentage */
        val limit = attr("optional").map(Value.Int.parse(_) / 100.0) getOrElse default_optional_limit
        val part = attr("part").map(id => config.part(id).id)
        val order = attr("order").map(Value.Int.parse) getOrElse 0
        val control =
          attr("control").map(spec =>
            if (spec == "none") Nil
            else spec.split("\\s+").toList.filter(_.nonEmpty).map(a => config.axis(a).name))
        Some(Page(path, name, theory, h.title, rest,
          is_index = name == config.index, optional_limit = limit,
          part = part, order = order, control = control))
      case hs => error_at(path, 1, "More than one page command: lines " + commas(hs.map(_.line.toString)))
    }
  }
}
