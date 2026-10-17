/*  Title:      site/src/thm.scala

doc_thm items: the facts as semantic XML (thy/site.ML, check_thms), laid out
with Isabelle's pretty printer and rendered as HTML.
*/

package site

import isabelle._


object Thm_Display {
  val margin = 76.0

  /* one figure per thm element; anchor: the id a figure gets for its fact, when the page
     links to it */
  def html(thms: XML.Body, links: Source.Links, anchor: String => Option[String] = _ => None): XML.Body =
    thms.flatMap(thm(_, links, anchor))

  private def thm(t: XML.Tree, links: Source.Links, anchor: String => Option[String]): XML.Body = {
    def caption(name: String): XML.Elem = {
      val text = HTML.text(Symbol.decode(name))
      XML.elem("figcaption",
        links.entity(Facts.THM, name) match {
          case Some(href) => List(HTML.link(href, text))
          case None => text
        })
    }
    def figure(name: String, body: XML.Body): XML.Body = {
      val fig = XML.Elem(Markup("figure", List("class" -> "thm")), caption(name) :: body)
      List(anchor(name) match { case Some(id) => HTML.id(id)(fig) case None => fig })
    }
    /* one child: the pre is a grid that centres its content as a whole (site.css) */
    def term(pp: XML.Body): XML.Elem =
      HTML.pre("t", List(XML.elem("span", to_html(Pretty.formatted(pp, margin = margin), links))))

    t match {
      case XML.Elem(Markup("thm", Markup.Name(name)), List(XML.Elem(Markup("normal", _), pp))) =>
        figure(name, List(term(pp)))
      case XML.Elem(Markup("thm", Markup.Name(name)),
          List(XML.Elem(Markup("rule", _),
            List(XML.Elem(Markup("prems", _), prems), XML.Elem(Markup("concl", _), List(concl)))))) =>
        def cell(c: XML.Tree): XML.Elem =
          c match {
            case XML.Elem(Markup("t", _), pp) => term(pp)
            case _ => error("Malformed rule cell in doc_thm item for " + quote(name))
          }
        /* no premises, no premises element: the serialiser would write it as <div/>, which
           a browser reads as an opening tag that swallows the conclusion */
        val prems_div = if (prems.isEmpty) Nil else List(HTML.div("prems", prems.map(cell)))
        figure(name, List(HTML.div("rule", prems_div ::: List(HTML.div("concl", List(cell(concl)))))))
      case _ => error("Malformed doc_thm item: " + XML.string_of_tree(t).take(80))
    }
  }

  /* the span vocabulary of the export (see thy/site.ML, spans) */
  def to_html(body: XML.Body, links: Source.Links): XML.Body = {
    def attr(name: String, value: String): HTML.Attribute = new HTML.Attribute(name, value)
    def prop(props: Properties.T, name: String): String = props.find(_._1 == name).map(_._2).getOrElse("")

    body.flatMap({
      case XML.Elem(Markup("span", props), inner) =>
        val html = to_html(inner, links)
        prop(props, "class") match {
          case "typed" => List(attr("title", Symbol.decode(prop(props, "tip")))(HTML.span("typed", html)))
          case "entity" =>
            val (kind, name) = (prop(props, "kind"), prop(props, "name"))
            links.entity(kind, name) match {
              case Some(href) => List(attr("title", kind + " " + name)(HTML.link(href, html)))
              case None => List(attr("title", kind + " " + name)(HTML.span("entity", html)))
            }
          case c => List(HTML.span(c, html))
        }
      case XML.Elem(_, inner) => to_html(inner, links)  // layout markup left by the formatter
      case XML.Text(s) => HTML.text(Symbol.decode(s))
    })
  }
}
