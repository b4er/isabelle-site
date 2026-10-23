/*  Title:      site/src/render.scala

Page rendering: the item tree to HTML once, then one document per profile state.
*/

package site

import isabelle._


class Render(
  store: Site_Store,
  config: Config.Config,
  site: Map[String, Page.Page],
  out_dir: Path,
  links: Source.Links,
  bib: Map[String, Bib.Entry] = Map.empty,
  index: Bib.Index = Bib.Index(Nil, "number")
) {
  val profiles: Profiles = new Profiles(config)

  /* the citations of the site: for each bib key, where it is cited, in rendering order */
  val citations: Synchronized[Map[String, List[Bib.Site_Ref]]] = Synchronized(Map.empty)

  /* heading anchors: unique slugs of the titles, in order */

  def slug(title: String): String =
    Symbol.decode(title).toLowerCase.replaceAll("[^a-z0-9]+", "-").stripPrefix("-").stripSuffix("-")

  def anchors(page: Page.Page): List[(Page.Heading, String)] = {
    var seen = Set.empty[String]
    for (h <- Page.flatten(page.items).collect({ case h: Page.Heading => h })) yield {
      val base = slug(h.title) match { case "" => "section" case s => s }
      val id = Iterator.from(1).map(i => if (i == 1) base else base + "-" + i).find(!seen(_)).get
      seen += id
      (h, id)
    }
  }


  /* the displays of a page: what its doc_thm and source items show, by entity, with the
     blocks the item sits in and the id its element gets. A name displayed twice has an id
     each; a reference goes to the first it may */
  sealed case class Display(item: Page.Item, scope: List[String], kind: String, name: String, id: String)

  def displays(page: Page.Page, taken: Set[String]): List[Display] = {
    var seen = taken
    def fresh(name: String): String = {
      val id = Iterator.from(1).map(i => if (i == 1) name else name + "-" + i).find(!seen(_)).get
      seen += id
      id
    }
    def rec(scope: List[String], items: List[Page.Item]): List[Display] =
      items.flatMap({
        case Page.Elem(_, _, inner, _) => rec(scope, inner)
        case Page.Block(aud, _, inner, _) =>
          Exn.capture(audience(profiles.parse_pred(aud))) match {
            case Exn.Res(printed) => rec(scope :+ printed, inner)
            case Exn.Exn(_) => Nil   // the item reports its bad audience itself
          }
        case it @ Page.Thm(thms, _) =>
          thms.collect({ case XML.Elem(Markup("thm", Markup.Name(name)), _) =>
            Display(it, scope, Facts.THM, name, fresh(name)) })
        case it @ Page.Source(name, kind, _) =>
          Exn.capture {
            val ref = Facts.resolve_any(store, name, kind)
            val (elems, range) = def_range(ref)
            val id = fresh(ref.name)
            ((ref.kind, ref.name) :: Source.defined(elems.snapshot, range)).distinct
              .map({ case (k, n) => Display(it, scope, k, n, id) })
          } match { case Exn.Res(ds) => ds case Exn.Exn(_) => Nil }   // reported at the item
        case _ => Nil
      })
    rec(Nil, page.items)
  }

  /* the displays of every page, in site order: the index, then the parts in their order,
     each page by its order. A reference its own page does not serve may link to another
     page's display, under the same rule */
  lazy val site_displays: List[(Page.Page, List[Display])] = {
    def part_index(p: Page.Page): Int =
      p.part.map(id => config.parts.indexWhere(_.id == id)).getOrElse(-1)
    site.values.toList.sortBy(p => (part_index(p), p.order, p.name))
      .map(p => (p, displays(p, anchors(p).map(_._2).toSet)))
  }

  /* the defining command of an entity: the theory's elements and the command's range */
  def def_range(ref: Facts.Ref) = {
    val elems = store.elements(ref.theory)
    val range = ref.def_range(elems.snapshot) getOrElse error("No position for " + ref.toString)
    val elem = elems.containing(range.start) getOrElse error("No defining command for " + ref.toString)
    (elems, elems.range(elem))
  }


  /* the predicate a block carries, printed for the page and kept for apply_state */

  private val audiences = Synchronized(Map.empty[String, Profile.Pred])

  private def audience(pred: Profile.Pred): String = {
    val printed = Profile.print(pred)
    audiences.change(_ + (printed -> pred))
    printed
  }


  /* references */

  class Page_Refs(page: Page.Page, val displays: List[Display]) extends Prose.Refs {
    /* the blocks the item being rendered sits in, by audience */
    var scope: List[String] = Nil
    /* the heading the item being rendered falls under: its anchor and title */
    var heading: Option[(String, String)] = None

    /* a citation: a marker linking to the entry on the References page, recorded there
       with the page and section it sits in. The site's cite audience gates the marker as
       a block is gated; the sentence around it stays for everyone */
    def cite(keys: String): XML.Body = {
      val ks = Bib.keys(keys)
      if (ks.isEmpty) error("Empty citation")
      val entries =
        ks.map(k => bib.getOrElse(k, error("Unknown citation key " + quote(k) +
          (if (bib.isEmpty) ": the site declares no Site.bib" else " (known: " + commas(bib.keys.toList.sorted) + ")"))))
      val here = Bib.Site_Ref(page.name, page.title, heading)
      citations.change(m => entries.foldLeft(m)((m, e) =>
        m + (e.key -> (m.getOrElse(e.key, Nil) :+ here).distinct)))
      val links =
        entries.map(e =>
          new HTML.Attribute("title", e.text)(
            HTML.link(References.page.file_name + "#" + e.key, HTML.text(index.label(e)))))
      val body = HTML.text("[") ::: Library.separate(HTML.text(", "), links.map(List(_))).flatten ::: HTML.text("]")
      val attrs =
        if (config.cite_audience.isEmpty) Nil
        else List("data-audience" -> audience(profiles.parse_pred(config.cite_audience)))
      List(XML.Elem(Markup("span", ("class" -> "cite") :: attrs), body))
    }

    /* the href of a display of the entity that whoever reads the reference is sure to
       see: outside every block, or in the same blocks as the reference; on this page
       first, then on the other pages in site order */
    def local(kind: String, name: String): Option[String] =
      if (!config.local_links) None
      else {
        def visible(d: Display): Boolean =
          d.kind == kind && d.name == name && (d.scope.isEmpty || d.scope == scope)
        displays.find(visible).map(d => "#" + d.id) orElse
          site_displays.iterator.filter(_._1.name != page.name)
            .flatMap({ case (p, ds) => ds.find(visible).map(d => p.file_name + "#" + d.id) })
            .nextOption()
      }

    def inline(kind: String, name: String): XML.Body = {
      val ref = Facts.resolve(store, kind, name)
      val text = HTML.text(Long_Name.base_name(ref.name))
      val title = new HTML.Attribute("title", Symbol.decode(Facts.hover(store, ref)))
      (local(ref.kind, ref.name) orElse links.entity(kind, ref.name)) match {
        case Some(href) => List(title(HTML.link(href, text)))
        case None => List(title(HTML.span("entity", text)))
      }
    }

    def link(target: String): (String, String) = {
      val (page_name, anchor) =
        target.split("#", 2) match {
          case Array(p) => (p, "")
          case Array(p, a) => (p, a)
        }
      /* a page by name, or a part by id: a part is entered at its first page and the
         link reads as the part's label, as in the header */
      val (target_page, text) =
        if (page_name.isEmpty) (page, page.title)
        else site.get(page_name) match {
          case Some(p) => (p, p.title)
          case None =>
            config.parts.find(_.id == page_name).flatMap(s => part_pages(s.id).headOption.map((_, s.label)))
              .getOrElse(error("Unknown page " + quote(page_name) + " (known pages: " +
                commas(site.keys.toList.sorted) + "; parts: " + commas(config.parts.map(_.id)) + ")"))
        }
      val href = target_page.file_name
      if (anchor.isEmpty) (href, text)
      else {
        val known = anchors(target_page)
        known.find(_._2 == anchor) match {
          case Some((h, _)) => (href + "#" + anchor, h.title)
          case None => error("Unknown anchor " + quote(anchor) + " in page " + target_page.name +
            " (known: " + commas(known.map(_._2)) + ")")
        }
      }
    }

    /* the defining command of an entity, as source, labelled with the logic its session is
       built on: the ancestor whose parent is Pure, so "Isabelle/HOL" or "Isabelle/ZF" */
    def defining(ref: Facts.Ref, id: Option[String]): XML.Body = {
      val (elems, range) = def_range(ref)
      val structure = store.background.sessions_structure
      def logic(session: String): String =
        structure.get(session).flatMap(_.parent) match {
          case Some(Thy_Header.PURE) | None => session
          case Some(parent) => logic(parent)
        }
      val kind = "Isabelle/" + logic(structure.theory_qualifier(elems.snapshot.node_name.theory))
      List(Source.render(elems.snapshot, range, links, id, Some(kind)))
    }

    def source(name: String, kind: Option[String], id: Option[String]): XML.Body =
      defining(Facts.resolve_any(store, name, kind), id)
  }


  /* items */

  def item(page: Page.Page, refs: Page_Refs, ids: List[(Page.Heading, String)], item: Page.Item): XML.Body = {
    def rec(items: List[Page.Item]): XML.Body = items.flatMap(it => this.item(page, refs, ids, it))
    /* an error is reported at the item whose own work raised it; nested items report themselves */
    def here[A](body: => A): A =
      try { body } catch { case ERROR(msg) => error(page.where(item.line) + msg) }
    item match {
      case heading @ Page.Heading(level, title, _, _) =>
        val h = if (level == 2) HTML.section else HTML.subsection
        val id = ids.find(_._1 eq heading).map(_._2) getOrElse error("No anchor for heading " + quote(title))
        refs.heading = Some((id, Symbol.decode(title)))
        List(HTML.id(id)(h(HTML.text(Symbol.decode(title)))))
      case Page.Text(text, _) => here(Prose.html(text, refs))
      case Page.Elem(cmd, attrs, inner, _) =>
        val tag = attrs.find(_._1 == "tag").map(_._2).getOrElse(cmd)
        /* an empty element must still close: the serialiser writes an empty body as <div/>,
           which a browser reads as an opening tag that swallows the rest of the page */
        val body = rec(inner) match { case Nil => List(XML.Text("")) case b => b }
        List(XML.Elem(Markup(tag, attrs.filterNot(_._1 == "tag")), body))
      /* a block is prose in place: the profile decides whether the reader gets it,
         not whether it is folded away behind a widget */
      case Page.Block(audience, attrs, inner, _) =>
        val pred = here(profiles.parse_pred(audience))
        val printed = this.audience(pred)
        refs.scope = refs.scope :+ printed
        val body = try rec(inner) finally { refs.scope = refs.scope.dropRight(1) }
        List(XML.Elem(Markup("div", ("data-audience" -> printed) :: attrs), body))
      case Page.Verbatim(cmd, attrs, text, _) =>
        val tag = attrs.find(_._1 == "tag").map(_._2).getOrElse(cmd)
        val classes = attrs.find(_._1 == "class").map(_._2.split(" ").toList).getOrElse(Nil)
        val body = HTML.text(Symbol.decode(text))
        List(XML.Elem(Markup(tag, attrs.filterNot(_._1 == "tag")),
          if (classes.contains("code")) List(HTML.code(body)) else body))
      /* the text as the checker marked it up, from the page theory's own snapshot; the
         block is a pre.code like a verbatim one, so the two read alike */
      case Page.Checked(cmd, attrs, text, range, _) =>
        val tag = attrs.find(_._1 == "tag").map(_._2).getOrElse("pre")
        val classes = ("code" :: cmd :: attrs.find(_._1 == "class").map(_._2.split(" ").toList).getOrElse(Nil)).distinct
        val snapshot = store.snapshot(page.theory)
        val body =
          Position.Range.unapply(range) match {
            case Some(pos_range) =>
              val text_range = Symbol.Text_Chunk(snapshot.node.source).decode(pos_range)
              /* the command's and the cartouche token's own markup span the whole body and say
                 nothing about it */
              val outer = Set("command_range", "command_span", Markup.CARTOUCHE, Markup.LANGUAGE, Markup.TOKEN_RANGE)
              def peel(body: XML.Body): XML.Body =
                body match {
                  case List(XML.Elem(m, inner)) if outer(m.name) => peel(inner)
                  case _ => body
                }
              Source.html(peel(snapshot.xml_markup(range = text_range, elements = Markup.Elements.full)), links)
            case None => HTML.text(Symbol.decode(text))
          }
        List(XML.Elem(Markup(tag, ("class" -> classes.mkString(" ")) ::
          attrs.filterNot(a => a._1 == "tag" || a._1 == "class")), List(HTML.code(body))))
      case Page.Html(text, _) => List(HTML.raw(HTML.text(Symbol.decode(text))))
      case it @ Page.Thm(thms, _) =>
        here(Thm_Display.html(thms, links,
          name => refs.displays.find(d => (d.item eq it) && d.name == name).map(_.id)))
      case it @ Page.Source(name, kind, _) =>
        here(refs.source(name, kind, refs.displays.find(_.item eq it).map(_.id)))
    }
  }

  /* heading levels must not skip (page = h1, section = h2, subsection = h3) */
  def heading_errors(page: Page.Page): List[String] = {
    var level = 1
    for (h <- Page.flatten(page.items).collect({ case h: Page.Heading => h })
      if { val bad = h.level > level + 1; level = h.level; bad })
      yield page.where(h.line) + "subsection without a preceding section"
  }

  /* all errors of a page are reported together */
  def items(page: Page.Page): XML.Body = {
    val ids = anchors(page)
    val refs =
      new Page_Refs(page, site_displays.find(_._1.name == page.name).map(_._2) getOrElse Nil)
    val results = page.items.map(it => Exn.capture(item(page, refs, ids, it)))
    val errors = heading_errors(page) ::: results.collect({ case Exn.Exn(ERROR(msg)) => msg })
    if (errors.nonEmpty) error(cat_lines(errors))
    results.collect({ case Exn.Res(body) => body }).flatten
  }


  /* profile states: blocks start open when the state says so */

  /* out of audience is out of the page: hidden, never removed, so the source stays one document */
  def apply_state(items: XML.Body, state: Profile.State): XML.Body =
    items.map({
      case XML.Elem(Markup(name, props), body) if props.exists(_._1 == "data-audience") =>
        val printed = props.find(_._1 == "data-audience").get._2
        val pred = audiences.value.getOrElse(printed, error("Unknown audience " + quote(printed)))
        val props1 = props.filterNot(_._1 == "hidden")
        XML.Elem(Markup(name, if (state.open(pred)) props1 else props1 ::: List("hidden" -> "hidden")),
          apply_state(body, state))
      case XML.Elem(markup, body) => XML.Elem(markup, apply_state(body, state))
      case t => t
    })

  def state_dir(state: Profile.State): Path =
    state match {
      case Profile.Closed => out_dir
      case s => out_dir + Path.explode("profiles/" + s.name)
    }


  /* navigation: the parts of the site, the pages of a part, the page's own headings */

  val by_part: Map[String, List[Page.Page]] =
    site.values.toList.flatMap(p => p.part.map(_ -> p)).groupBy(_._1)
      .view.mapValues(_.map(_._2).sortBy(p => (p.order, p.name))).toMap

  /* a part is entered at its first page */
  def part_pages(id: String): List[Page.Page] = by_part.getOrElse(id, Nil)

  def nav(page: Page.Page): XML.Elem = {
    val part_items =
      for (s <- config.parts; first <- part_pages(s.id).headOption) yield {
        val current = page.part.contains(s.id)
        XML.Elem(Markup("a", ("href" -> first.file_name) ::
          (if (current) List("aria-current" -> "true") else Nil)), HTML.text(s.label))
      }
    val link_items =
      config.links.map(l =>
        XML.Elem(Markup("a", List("href" -> l.url, "class" -> "external")), HTML.text(l.label)))
    XML.Elem(Markup("nav", List("class" -> "site", "aria-label" -> "Site")), part_items ::: link_items)
  }

  /* the rail: this part's pages, with the current page's own headings under it. A part
     of one page lists that page's headings directly under the label, since the page's
     title would only repeat it; a part of one page without headings has no rail, since
     it would list nothing, and the page is centred like the landing page; so is a page
     marked "rail: none", whose own content already maps it */
  def rail(page: Page.Page): Option[XML.Elem] =
    page.part.filter(_ => page.rail).filterNot(id =>
      part_pages(id) match {
        case List(only) => !anchors(only).exists(_._1.level == 2)
        case _ => false
      }).map { id =>
      val label = config.part(id).label
      def headings(p: Page.Page): XML.Body =
        if (p.name != page.name) Nil
        else anchors(p).filter(_._1.level == 2).map({ case (h, anchor) =>
          HTML.item(List(HTML.link("#" + anchor, HTML.text(Symbol.decode(h.title))))) })
      def entry(p: Page.Page): XML.Body = {
        val link =
          XML.Elem(Markup("a", ("href" -> p.file_name) ::
            (if (p.name == page.name) List("aria-current" -> "page") else Nil)),
            HTML.text(Symbol.decode(p.title)))
        val subs = headings(p)
        List(XML.elem("li", link :: (if (subs.isEmpty) Nil else List(XML.elem("ul", subs)))))
      }
      val pages = part_pages(id)
      val list =
        pages match {
          case List(only) => XML.Elem(Markup("ul", List("class" -> "headings")), headings(only))
          case _ => XML.elem("ul", pages.flatMap(entry))
        }
      XML.Elem(Markup("nav", List("class" -> "rail", "aria-label" -> label)),
        List(XML.elem("h2", HTML.text(label)), list,
          /* revealed by the script once there is a way back; "#top" is the header */
          XML.Elem(Markup("a", List("class" -> "to-top", "href" -> "#top", "hidden" -> "hidden")),
            HTML.text("Top"))))
    }

  /* previous and next within the part */
  def pager(page: Page.Page): Option[XML.Elem] =
    page.part.flatMap { id =>
      val pages = part_pages(id)
      val i = pages.indexWhere(_.name == page.name)
      /* the arrows are the stylesheet's, so they are drawn alike and not copied with the title */
      def link(rel: String, p: Page.Page): XML.Elem =
        XML.Elem(Markup("a", List("rel" -> rel, "class" -> rel, "href" -> p.file_name)),
          HTML.text(Symbol.decode(p.title)))
      val prev = if (i > 0) List(link("prev", pages(i - 1))) else Nil
      val next = if (i >= 0 && i < pages.length - 1) List(link("next", pages(i + 1))) else Nil
      if (prev.isEmpty && next.isEmpty) None
      else Some(XML.Elem(Markup("nav", List("class" -> "pager", "aria-label" -> "Pages")), prev ::: next))
    }


  /* the reader-profile control: the dials this page offers, and the pre-rendered readings */

  /* the dials a page offers: the ones its control names, else the axes its blocks mention,
     at any depth; a page whose blocks mention no axis, or that has none, offers no control */
  def control_axes(page: Page.Page): List[Config.Axis] =
    page.control match {
      case Some(names) => names.map(config.axis)
      case None =>
        def blocks(items: List[Page.Item]): List[Page.Block] =
          items.flatMap({
            case b @ Page.Block(_, _, inner, _) => b :: blocks(inner)
            case Page.Elem(_, _, inner, _) => blocks(inner)
            case _ => Nil
          })
        val mentioned =
          blocks(page.items).flatMap(b =>
            Exn.capture(profiles.parse_pred(b.audience)) match {
              case Exn.Res(pred) => Profile.dnf(pred).flatten.map(_.axis.name)
              case Exn.Exn(_) => Nil   // the block reports its bad audience itself
            }).toSet
        config.axes.filter(a => mentioned(a.name))
    }

  /* the summary names the profile by the tags of its levels, a faint "none" when no level
     adds anything; the form gives each axis a pill per level and what the chosen level adds */
  def profile_control(page: Page.Page, states: List[Profile.State], state: Profile.State,
      href: Path => String): Option[XML.Elem] = {
    val axes = control_axes(page)
    if (axes.isEmpty) None
    else {
      val tags =
        state match {
          case Profile.Of(_, p) => p.tags
          case _ => Nil
        }
      val chips =
        if (tags.nonEmpty) tags.map(t => HTML.span("tag", HTML.text(t)))
        else List(HTML.span("tag empty", HTML.text(if (state == Profile.Open) "everything" else "none")))
      def page_href(s: Profile.State): String =
        href(state_dir(s) + Path.basic(page.file_name))
      val state_links =
        states.map(s => {
          val text = HTML.text(if (s == Profile.Open) "everything" else if (s == Profile.Closed) "the core" else s.name)
          if (s == state) XML.Elem(Markup("a", List("aria-current" -> "page", "href" -> page_href(s))), text)
          else HTML.link(page_href(s), text)
        })
      def axis(a: Config.Axis): XML.Elem = {
        val value = state match { case Profile.Of(_, p) => p(a) case _ => -1 }
        val pills =
          a.levels.zipWithIndex.map({ case (level, i) =>
            XML.elem("label", List(
              XML.Elem(Markup("input",
                List("type" -> "radio", "name" -> a.name, "value" -> i.toString, "data-tag" -> a.tags(i)) :::
                (if (i == value) List("checked" -> "checked") else Nil)), Nil),
              XML.elem("span", HTML.text(level)))) })
        val hints =
          a.hints.zipWithIndex.collect({ case (h, i) if h.nonEmpty =>
            XML.Elem(Markup("span", List("data-level" -> i.toString)), HTML.text(h)) })
        XML.Elem(Markup("fieldset", List("class" -> "axis")),
          XML.elem("legend", HTML.text(a.title)) ::
          XML.Elem(Markup("div", List("class" -> "seg")), pills) ::
          (if (hints.isEmpty) Nil else List(XML.Elem(Markup("p", List("class" -> "hint")), hints))))
      }
      Some(XML.Elem(Markup("details", List("class" -> "profile")), List(
        XML.elem("summary", List(
          HTML.span("profile-label", HTML.text(config.control_label)),
          HTML.span("profile-current", chips))),
        XML.Elem(Markup("p", List("class" -> "profile-links")),
          HTML.text("Read ") ::: Library.separate(XML.Text(" · "), state_links)),
        XML.Elem(Markup("form", List("class" -> "profile-form", "hidden" -> "hidden")),
          XML.Elem(Markup("p", List("class" -> "profile-head")), HTML.text("Reader profile")) ::
          axes.map(axis) :::
          List(XML.Elem(Markup("button", List("type" -> "button", "class" -> "profile-reset")),
            HTML.text("Forget my profile")))))))
    }
  }



  /* documents: written like Isabelle/AFP browser_info (cf. HTML.write_document), i.e. each
     output directory gets isabelle.css with the @font-face rules for the Isabelle fonts
     (Theories.fonts_root), and pages link it relatively; the generator's site.css and site.js and the site's own assets
     layer on top.
     The page shell is site/template.html when the site has one, else Render.default_template. */

  /* the directory of a state, with the isabelle.css that finds the fonts from there */
  def prepare_state(state: Profile.State): Unit = {
    val base_dir = Isabelle_System.make_directory(state_dir(state))
    val fonts_prefix =
      HTML.relative_href(Theories.fonts_root(out_dir, config.sources), base = Some(base_dir))
    File.write(base_dir + HTML.isabelle_css.base,
      HTML.fonts_css_dir(fonts_prefix) + "\n\n" + File.read(HTML.isabelle_css))
  }

  /* the badge is the page's: the statistics page it links to is written without one */
  def write(page: Page.Page, items: XML.Body, states: List[Profile.State], state: Profile.State,
      assets: List[String], template: String, badge: Option[XML.Elem]): Unit = {
    val base_dir = state_dir(state)
    def href(target: Path): String = HTML.relative_href(target, base = Some(base_dir))
    def asset(name: String): String = href(out_dir + Path.explode("assets/" + name))

    /* site-root relative paths in items and links, made relative to this directory */
    val root_prefix = href(out_dir + Path.basic("x")).stripSuffix("x")

    def rooted(s: String): String = if (Render.is_rooted(s)) root_prefix + s else s
    def relativize(t: XML.Tree): XML.Tree =
      t match {
        case XML.Elem(Markup(Markup.RAW_HTML, props), body) =>
          XML.Elem(Markup(Markup.RAW_HTML, props),
            body.map({
              case XML.Text(s) =>
                XML.Text(Render.root_url.replaceAllIn(s, m =>
                  java.util.regex.Matcher.quoteReplacement(m.group(1) + root_prefix + m.group(2))))
              case t => t }))
        case XML.Elem(markup, body) =>
          val markup1 =
            Markup(markup.name, markup.properties.map({
              case (k, v) if k == "src" || k == "href" => (k, rooted(v))
              case p => p
            }))
          XML.Elem(markup1, body.map(relativize))
        case t => t
      }

    def mime(path: String): String =
      if (path.endsWith(".svg")) "image/svg+xml"
      else if (path.endsWith(".png")) "image/png"
      else if (path.endsWith(".ico")) "image/x-icon"
      else ""

    val head =
      List(
        XML.Elem(Markup("meta", List("charset" -> "utf-8")), Nil),
        XML.Elem(Markup("meta", List("name" -> "viewport", "content" -> "width=device-width, initial-scale=1")), Nil),
        HTML.style_file(HTML.isabelle_css.file_name)) :::
      (if (config.icon.isEmpty) Nil
       else List(XML.Elem(Markup("link", List("rel" -> "icon", "href" -> rooted(config.icon)) :::
         (if (mime(config.icon).isEmpty) Nil else List("type" -> mime(config.icon)))), Nil))) :::
      assets.filter(_.endsWith(".css")).map(css => HTML.style_file(asset(css))) :::
      // a non-empty body: <script/> is not a closed element in HTML
      assets.filter(_.endsWith(".js")).map(js =>
        XML.Elem(Markup("script", List("src" -> asset(js), "defer" -> "defer")), List(XML.Text(" "))))

    def out(body: XML.Body): String =
      HTML.output(body, hidden = true, structural = true)
    def text(s: String): String = HTML.output(HTML.text(Symbol.decode(s)), hidden = true, structural = false)

    /* the brand: the site's logo beside its name, or just the name */
    val brand =
      XML.Elem(Markup("span", List("class" -> "brand-mark")),
        (if (config.logo.isEmpty) Nil
         else List(XML.Elem(Markup("img", List("class" -> "logo", "src" -> rooted(config.logo),
           "alt" -> "")), Nil))) :::
        List(HTML.span("brand-name", HTML.text(Symbol.decode(config.title)))))

    /* the script reads the site's axes and their tags from the body, because a page's
       control may show only some of them while the reader's profile always has them all */
    val body_attrs =
      ("data-profile" -> state.name) ::
      ("data-axes" -> config.axes.map(_.name).mkString(",")) ::
      ("data-levels" -> config.axes.map(_.levels.length).mkString(",")) ::
      ("data-tags" -> JSON.Format(config.axes.map(_.tags))) ::
      (state match { case Profile.Of(_, p) => List("data-profile-query" -> p.query) case _ => Nil })

    val holes: Map[String, String] =
      Map(
          "head" -> out(head),
          "title" -> text(if (page.is_index || config.title.isEmpty) page.title
            else config.title + " · " + page.title),
          "body_attrs" -> body_attrs.map({ case (k, v) => " " + k + "=\"" + XML.text(v) + "\"" }).mkString,
          "index" -> "index.html",                    // Page.file_name of the landing page
          "site" -> text(config.title),
          "brand" -> out(List(brand)),
          "nav" -> out(List(relativize(nav(page)))),
          "control" -> profile_control(page, states, state, href).map(c => out(List(c))).getOrElse(""),
          "rail" -> rail(page).map(r => out(List(r))).getOrElse(""),
          "page_title" -> text(page.title),
          "content" -> out(apply_state(items, state).map(relativize)),
          "pager" -> pager(page).map(p => out(List(p))).getOrElse(""),
          "badge" -> badge.map(b => out(List(relativize(b)))).getOrElse(""))
    /* one pass, so a hole's text inside a page's content is content, not a hole */
    val document =
      Render.hole.replaceAllIn(template, m =>
        java.util.regex.Matcher.quoteReplacement(
          holes.getOrElse(m.group(1), error("Unknown hole {{" + m.group(1) + "}} in the template"))))

    File.write(base_dir + Path.basic(page.file_name), document)
  }


}


/* the default page shell: a site overrides it with site/template.html */

object Render {
  /* paths in a page that are relative to the site root, not to the page's directory: the
     directories the generator itself writes at the root */
  val root_dirs: List[String] = List("assets", Theories.dir)
  def is_rooted(path: String): Boolean = root_dirs.exists(d => path.startsWith(d + "/"))

  /* such a path as the src or href of raw HTML */
  val root_url: scala.util.matching.Regex =
    ("""\b((?:src|href)=")((?:""" + root_dirs.mkString("|") + ")/)").r

  /* {{name}} in the template */
  val hole: scala.util.matching.Regex = """\{\{([a-z_]+)\}\}""".r

  val default_template: String =
"""<!DOCTYPE html>
<html lang="en">
<head>
{{head}}
<title>{{title}}</title>
</head>
<body{{body_attrs}}>
<a class="skip" href="#main">Skip to content</a>
<header id="top">
<a class="brand" href="{{index}}">{{brand}}</a>
{{nav}}
{{control}}
</header>
<div class="layout">
{{rail}}
<main id="main" tabindex="-1">
<h1>{{page_title}}</h1>
{{content}}
{{pager}}
</main>
</div>
{{badge}}
</body>
</html>
"""
}
