/*  Title:      site/src/config.scala

Site configuration as declared in the site's theory (thy/site.ML) and exported
by every theory that imports it: reader axes, named profiles, commands.
*/

package site

import isabelle._


object Config {
  /* tags: per level, its name in the header control, "" for a level that adds nothing;
     hints: per level, what it adds, "" for nothing */
  sealed case class Axis(name: String, title: String, levels: List[String],
      tags: List[String], hints: List[String]) {
    def level(s: String): Int =
      levels.indexOf(s) match {
        case -1 =>
          Value.Int.unapply(s).filter(i => 0 <= i && i < levels.length) getOrElse
            error("Bad level " + quote(s) + " for axis " + name + ": expected " +
              commas(levels) + " or 0.." + (levels.length - 1))
        case i => i
      }
  }

  /* a page command: the site's own, which exports its items, or a document command of the
     enclosing system (text, section, ...), whose top-level body is read from the source */
  sealed case class Command(name: String, external: Boolean)

  sealed case class Part(id: String, label: String)
  sealed case class Link(label: String, url: String)

  sealed case class Config(
    title: String,
    logo: String,               // site-root relative image for the header, or ""
    icon: String,               // site-root relative image for the favicon, or ""
    index: String,
    parts: List[Part],
    links: List[Link],
    repository: String,        // the repository URL for the footer, or "" for no footer
    sources: List[String],     // sessions presented as HTML under src/
    badge: Boolean,            // the "Machine checked" corner badge
    bib: String,               // session-dir relative .bib file the citations resolve in, or ""
    cite_audience: String,     // the audience of a citation marker, or "" for everyone
    cite_style: String,        // "number": [1]; "author-year": [White et al. 2014]
    local_links: Boolean,      // prose references link to the page's own displays first
    control_label: String,     // before the tags in the header control
    axes: List[Axis],
    profiles: List[(String, List[Int])],
    commands: Map[String, Command]
  ) {
    def axis(name: String): Axis =
      axes.find(_.name == name) getOrElse
        error("Unknown axis " + quote(name) + ": expected " + commas(axes.map(_.name)))

    def part(id: String): Part =
      parts.find(_.id == id) getOrElse
        error("Unknown part " + quote(id) + ": expected " + commas(parts.map(_.id)))
  }

  val export_name = "site/config.xml"

  def read(store: Site_Store, theory: String): Config = {
    val body =
      store.session_context.theory(theory).get(export_name).map(_.yxml()) getOrElse
        error("No site configuration exported by theory " + theory + ": session " +
          store.session + " is not built")
    def prop(props: Properties.T, name: String): String =
      props.find(_._1 == name).map(_._2) getOrElse error("Malformed site configuration: no " + name)
    body match {
      case List(XML.Elem(Markup("config", config_props), items)) =>
        val parts =
          items.collect({ case XML.Elem(Markup("part", props), _) =>
            Part(prop(props, "id"), prop(props, "label")) })
        val links =
          items.collect({ case XML.Elem(Markup("link", props), _) =>
            Link(prop(props, "label"), prop(props, "url")) })
        val sources =
          items.collect({ case XML.Elem(Markup("source", props), _) => prop(props, "session") })
        val axes =
          items.collect({ case XML.Elem(Markup("axis", props), body) =>
            val levels = body.collect({ case XML.Elem(Markup("level", lp), _) => lp })
            Axis(prop(props, "name"), prop(props, "title"),
              levels.map(prop(_, "name")), levels.map(prop(_, "tag")), levels.map(prop(_, "hint"))) })
        val profiles =
          items.collect({ case XML.Elem(Markup("profile", props), values) =>
            val vs = values.collect({ case XML.Elem(Markup("value", vp), _) => prop(vp, "axis") -> Value.Int.parse(prop(vp, "level")) }).toMap
            prop(props, "name") -> axes.map(a => vs(a.name)) })
        val commands =
          items.collect({ case XML.Elem(Markup("command", props), _) =>
            val name = prop(props, "name")
            name -> Command(name, external = prop(props, "external") == "true") }).toMap
        if (axes.isEmpty) error("Site configuration declares no axes (Site.axes in the site theory)")
        Config(prop(config_props, "title"), prop(config_props, "logo"), prop(config_props, "icon"),
          prop(config_props, "index"), parts, links, repository = prop(config_props, "repository"), sources = sources,
          badge = prop(config_props, "badge") == "true",
          bib = prop(config_props, "bib"), cite_audience = prop(config_props, "cite_audience"),
          cite_style = prop(config_props, "cite_style"),
          local_links = prop(config_props, "local_links") == "true",
          control_label = prop(config_props, "control_label"),
          axes, profiles, commands)
      case _ => error("Malformed site configuration export")
    }
  }
}
