/*  Title:      site/src/prose.scala

Prose inside cartouches: paragraphs, \<^item> lists, control-symbol markup
(\<^emph> \<^bold> \<^verbatim> \<^url> \<^cite>), inline cartouches as code, and
@{thm|const|type ...} antiquotations.
*/

package site

import isabelle._


object Prose {
  trait Refs {
    def inline(kind: String, name: String): XML.Body
    def link(target: String): (String, String)   // internal "Page#anchor" -> (href, title)
    def cite(keys: String): XML.Body             // \<^cite>‹key› or ‹key1 and key2›
  }

  private val scheme = """^[a-zA-Z][a-zA-Z0-9+.-]*:""".r

  private val paragraph_break = "\n[ \t]*\n"

  def html(text: String, refs: Refs): XML.Body = {
    /* paragraphs: split Text pieces at blank lines */
    sealed abstract class Piece
    case object Break extends Piece
    case class Inline(a: Antiquote.Antiquote) extends Piece

    val item_controls = Set("\\<^item>", "\\<^enum>")

    val pieces =
      Antiquote.read(text).flatMap({
        case Antiquote.Text(s) =>
          Library.separate(Break, s.split(paragraph_break, -1).toList.map(t => Inline(Antiquote.Text(t))))
        case a @ Antiquote.Control(c) if item_controls(c) => List(Break, Inline(a))  // each item is a paragraph
        case a => List(Inline(a))
      })

    def paragraphs(ps: List[Piece]): List[List[Antiquote.Antiquote]] =
      ps.span(_ != Break) match {
        case (par, Nil) => List(par.collect({ case Inline(a) => a }))
        case (par, _ :: rest) => par.collect({ case Inline(a) => a }) :: paragraphs(rest)
      }

    def blank(par: List[Antiquote.Antiquote]): Boolean =
      par.forall({ case Antiquote.Text(s) => s.isBlank case _ => false })

    def item_kind(par: List[Antiquote.Antiquote]): Option[String] =
      par.dropWhile({ case Antiquote.Text(s) => s.isBlank case _ => false }) match {
        case Antiquote.Control(c) :: _ if c == "\\<^item>" => Some("ul")
        case Antiquote.Control(c) :: _ if c == "\\<^enum>" => Some("ol")
        case _ => None
      }

    def item_body(par: List[Antiquote.Antiquote]): List[Antiquote.Antiquote] =
      par.dropWhile({ case Antiquote.Text(s) => s.isBlank case _ => false }).tail

    def blocks(pars: List[List[Antiquote.Antiquote]]): XML.Body =
      pars match {
        case Nil => Nil
        case par :: rest if blank(par) => blocks(rest)
        case par :: _ if item_kind(par).isDefined =>
          val kind = item_kind(par).get
          val (items, rest) = pars.span(p => item_kind(p).contains(kind))
          XML.elem(kind, items.map(p => HTML.item(inline_html(item_body(p))))) :: blocks(rest)
        case par :: rest => HTML.par(inline_html(par)) :: blocks(rest)
      }

    def inline_html(par: List[Antiquote.Antiquote]): XML.Body = par.flatMap(inline_one)

    def inline_one(a: Antiquote.Antiquote): XML.Body =
      a match {
        case Antiquote.Text(s) => HTML.text(Symbol.decode(s))
        case Antiquote.Control(source) =>
          val syms = Symbol.explode(source)
          val (control, cartouche) =
            if (Symbol.is_control(syms.head)) (syms.head, syms.tail.mkString) else ("", source)
          val body = if (cartouche.isEmpty) None else Some(Scan.Parsers.cartouche_content(cartouche))
          val name = if (control.isEmpty) "" else control.substring(3, control.length - 1)
          (name, body) match {
            case ("", Some(b)) => List(HTML.code(HTML.text(Symbol.decode(b))))
            case ("emph", Some(b)) => List(HTML.emph(html(b, refs).flatMap(unpar)))
            case ("bold", Some(b)) => List(XML.elem("strong", html(b, refs).flatMap(unpar)))
            case ("verbatim", Some(b)) => List(HTML.code(HTML.text(Symbol.decode(b))))
            case ("url", Some(b)) if scheme.findFirstIn(b).isDefined => List(HTML.link(b, HTML.text(b)))
            case ("url", Some(b)) =>
              val (href, title) = refs.link(b)
              List(HTML.link(href, HTML.text(Symbol.decode(title))))
            case ("cite", Some(b)) => refs.cite(b)
            case _ => error("Unsupported markup " + quote(source))
          }
        case Antiquote.Antiq(source) =>
          val inner = source.substring(2, source.length - 1)
          Token.explode(Keyword.Keywords.empty, inner).filter(_.is_proper) match {
            case List(kind, name) if kind.is_ident && name.is_name =>
              kind.content match {
                case "thm" => refs.inline(Facts.THM, name.content)
                case "const" => refs.inline(Facts.CONST, name.content)
                case "type" => refs.inline(Facts.TYPE, name.content)
                case k => error("Unsupported inline antiquotation " + quote(k) + " in " + quote(source))
              }
            case _ => error("Malformed antiquotation " + quote(source))
          }
      }

    /* a paragraph nested inside inline markup contributes its content only */
    def unpar(t: XML.Tree): XML.Body =
      t match { case XML.Elem(Markup("p", _), body) => body  case _ => List(t) }

    blocks(paragraphs(pieces))
  }
}
