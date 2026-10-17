/*  Title:      site/src/stats.scala

Statistics of the Isabelle sessions a site publishes as its sources: what was
checked, how much of it there is, and whether any of it was skipped. The badge
in the corner of every page is the one-bit summary of this.
*/

package site

import isabelle._
import isabelle.graphview.Graphview


object Stats {
  /* a proof that was not given: the honest thing for a badge to be red about */
  val skipped: Set[String] = Set("sorry", "oops")

  sealed case class Theory(name: String, lines: Int, consts: Int, types: Int, thms: Int, skipped: Int)

  /* graph: the session's theories and the sessions they import directly, as [Session] */
  sealed case class Session(name: String, chapter: String, theories: List[Theory], timing: Timing,
      graph: Graph_Display.Graph) {
    def dir: String = Theories.dir + "/" + chapter + "/" + name
    def graph_id: String = site.Stats.graph_id(name)
    def lines: Int = theories.map(_.lines).sum
    def consts: Int = theories.map(_.consts).sum
    def types: Int = theories.map(_.types).sum
    def thms: Int = theories.map(_.thms).sum
    def skipped: Int = theories.map(_.skipped).sum
  }

  /* graph: the published sessions and the sessions they import, as [Session] */
  sealed case class Stats(sessions: List[Session], isabelle: String, graph: Graph_Display.Graph) {
    def is_empty: Boolean = sessions.isEmpty
    def published(name: String): Boolean = sessions.exists(_.name == name)

    /* a published session links to its graph, a theory of one to its presented source */
    def link(node: Graph_Display.Node): Option[String] =
      if (node.ident.startsWith("session.")) {
        val name = node.ident.stripPrefix("session.")
        if (published(name)) Some("#" + graph_id(name)) else None
      }
      else {
        val theory = node.ident.stripPrefix("theory.")
        sessions.find(_.theories.exists(_.name == theory)).map(session =>
          session.dir + "/" + Long_Name.base_name(theory) + ".html")
      }
    def lines: Int = sessions.map(_.lines).sum
    def theories: Int = sessions.map(_.theories.length).sum
    def consts: Int = sessions.map(_.consts).sum
    def types: Int = sessions.map(_.types).sum
    def thms: Int = sessions.map(_.thms).sum
    def skipped: Int = sessions.map(_.skipped).sum
    def elapsed: Time = Time.seconds(sessions.map(_.timing.elapsed.seconds).sum)
    def cpu: Time = Time.seconds(sessions.map(_.timing.cpu.seconds).sum)
    def ok: Boolean = skipped == 0
  }


  /* reading them out of the session store and the theory sources */

  def graph_id(session: String): String = "graph-" + session
  val sessions_graph_id = "graph-sessions"

  def session_node(name: String, published: Boolean): Graph_Display.Node =
    Graph_Display.Node(if (published) name else "[" + name + "]", "session." + name)

  def read(store: Site_Store, deps: Sessions.Deps, sessions: List[String]): Stats = {
    val structure = deps.sessions_structure

    def session_stats(session: String): Session = {
      val base = deps(session)
      val syntax = base.overall_syntax

      /* a proof command in the source, lexed with the session's own keywords, so the word
         in a comment, a string or a cartouche is not miscounted */
      def skipped_in(text: String): Int =
        Token.explode(syntax.keywords, text).count(tok => tok.is_command && skipped(tok.source))

      val theories =
        for {
          node <- base.proper_session_theories
          name = node.theory
        } yield {
          val (consts, types, thms) =
            Exn.capture(store.theory(name)) match {
              case Exn.Res(thy) => (thy.consts.length, thy.types.length, thy.thms.length)
              case Exn.Exn(_) => (0, 0, 0)
            }
          val (lines, skipped_proofs) =
            Exn.capture(File.read(node.path)) match {
              case Exn.Res(text) => (split_lines(text).length, skipped_in(text))
              case Exn.Exn(_) => (0, 0)
            }
          Theory(name, lines = lines, consts = consts, types = types, thms = thms,
            skipped = skipped_proofs)
        }

      /* the timing of the build that produced this session, as it recorded it */
      val timing = {
        val props =
          store.session_context.session_db(session) match {
            case Some(db) => store.store.read_session_timing(db, session)
            case None =>
              using_optional(store.store.try_open_database(session))({
                case Some(db) => store.store.read_session_timing(db, session)
                case None => Nil
              })
          }
        Markup.Timing_Properties.get(props)
      }

      /* as Sessions.Base builds session_graph_display, without the sessions the imported
         ones build on */
      val graph = {
        def node(theory: String): Graph_Display.Node = {
          val qualifier = structure.theory_qualifier(theory)
          if (qualifier == session)
            Graph_Display.Node(Long_Name.base_name(theory), "theory." + theory)
          else session_node(qualifier, false)
        }
        base.proper_session_theories.foldLeft(Graph_Display.empty_graph) {
          case (g, name) =>
            val a = node(name.theory)
            val bs = base.loaded_theories.imm_preds(name.theory).toList.map(node).filterNot(_ == a)
            bs.foldLeft((a :: bs).foldLeft(g)(_.default_node(_, Nil)))(_.add_edge(_, a))
        }
      }

      Session(session, structure(session).chapter, theories, timing, graph)
    }

    /* the published sessions and the sessions they import, ordered among themselves by the
       imports graph */
    val published = sessions.toSet
    val imports = structure.imports_graph
    val shown = (sessions ::: sessions.flatMap(imports.imm_preds)).distinct
    val graph = {
      val g = imports.restrict(imports.all_preds(shown).toSet).transitive_closure
        .restrict(shown.toSet).transitive_reduction_acyclic
      def node(name: String) = session_node(name, published(name))
      g.edges_iterator.foldLeft(
        g.keys.foldLeft(Graph_Display.empty_graph)((h, name) => h.default_node(node(name), Nil))
      ) { case (h, (a, b)) => h.add_edge(node(a), node(b)) }
    }

    Stats(sessions.map(session_stats),
      Isabelle_System.isabelle_identifier() getOrElse Isabelle_System.isabelle_id(), graph)
  }


  /* the badge: one bit, in the corner of every page. The colour says whether anything was
     skipped; the hover says what the badge is, that the site is an artifact of a build */

  def badge(stats: Stats, href: String): XML.Elem = {
    val skipped = stats.skipped.toString + " proof" + (if (stats.skipped == 1) "" else "s") + " skipped"
    val (cls, text) = if (stats.ok) ("ok", "Machine checked") else ("bad", skipped)
    val title =
      "This website was generated from checked Isabelle theories (click to see the statistics)."
    XML.Elem(Markup("a", List("class" -> ("checked " + cls), "href" -> href, "title" -> title)),
      HTML.text(text))
  }


  /* the statistics page */

  val page: Page.Page =
    Page.Page(Path.basic("stats"), "Stats", "", "Isabelle statistics", Nil,
      is_index = false, optional_limit = 1.0, part = None, order = 0, control = Some(Nil))

  /* durations for reading, not for measuring */
  def duration(t: Time): String = {
    val s = t.seconds
    if (s >= 3600) "%d h %d min".format((s / 3600).toInt, ((s % 3600) / 60).toInt)
    else if (s >= 60) "%d min %d s".format((s / 60).toInt, (s % 60).toInt)
    else "%.1f s".format(s)
  }

  /* a graph, laid out as Isabelle lays out a session's for session_graph.pdf, but as inline
     SVG: it sits in the page like an image, in the page's colours and font, and its nodes
     link as Stats.link says */

  def graph_svg(
    options: Options, stats: Stats, graph: Graph_Display.Graph, label: String
  ): XML.Elem = {
    val graphview_options = options
    val graphview =
      new Graphview(graph.transitive_reduction_acyclic) {
        def options: Options = graphview_options
      }
    graphview.update_layout()
    val layout = graphview.layout
    val metrics = layout.metrics
    val box = graphview.bounding_box()

    def num(d: Double): String = {
      val s = ((d * 10).round / 10.0).toString
      if (s.endsWith(".0")) s.dropRight(2) else s
    }
    def pt(x: Double, y: Double): String = num(x) + "," + num(y)

    /* the edge as Graphview draws it: a cardinal spline through the dummies of the layout,
       a straight line when there are none (Shapes.Cardinal_Spline_Edge) */
    def edge_path(edge: Graph_Display.Edge): String = {
      val p = layout.get_node(edge._1)
      val q = layout.get_node(edge._2)
      val (a, b) = (p.y min q.y, p.y max q.y)
      val ds = layout.dummies_iterator(edge).filter(d => a < d.y && d.y < b).toList
      val path = new StringBuilder("M" + pt(p.x, p.y))
      if (ds.isEmpty) path ++= " L" + pt(q.x, q.y)
      else {
        val slack = 0.1
        val coords = p :: ds ::: List(q)
        val dx = coords(2).x - coords(0).x
        val dy = coords(2).y - coords(0).y
        val (dx2, dy2) =
          coords.sliding(3).foldLeft((dx, dy)) {
            case ((dx, dy), Seq(l, m, r)) =>
              val dx2 = r.x - l.x
              val dy2 = r.y - l.y
              path ++= " C" + pt(l.x + slack * dx, l.y + slack * dy) + " " +
                pt(m.x - slack * dx2, m.y - slack * dy2) + " " + pt(m.x, m.y)
              (dx2, dy2)
            case (d, _) => d
          }
        val l = ds.last
        path ++= " C" + pt(l.x + slack * dx2, l.y + slack * dy2) + " " +
          pt(q.x - slack * dx2, q.y - slack * dy2) + " " + pt(q.x, q.y)
      }
      path.toString
    }

    val edges =
      graphview.visible_graph.edges_iterator.map(edge =>
        XML.Elem(Markup("path", List("d" -> edge_path(edge))), Nil)).toList

    /* the hover of a theory is its full name, which says the session */
    def node_elem(node: Graph_Display.Node): XML.Elem = {
      val info = layout.get_node(node)
      val line_height = metrics.height.ceil
      val y0 = info.y - (info.lines.length max 1) * line_height / 2 + metrics.ascent
      val rect =
        XML.Elem(Markup("rect", List(
          "x" -> num(info.left), "y" -> num(info.y - info.height2),
          "width" -> num(info.width), "height" -> num(info.height))), Nil)
      val texts =
        info.lines.zipWithIndex.map({ case (line, i) =>
          XML.Elem(Markup("text", List(
            "x" -> num(info.x), "y" -> num(y0 + i * line_height), "text-anchor" -> "middle")),
            HTML.text(line)) })
      val title =
        if (node.ident.startsWith("theory."))
          List(XML.elem("title", HTML.text(node.ident.stripPrefix("theory."))))
        else Nil
      val g = XML.elem("g", title ::: rect :: texts)
      stats.link(node) match {
        case Some(href) => XML.Elem(Markup("a", List("href" -> href)), List(g))
        case None => g
      }
    }
    val nodes = graphview.visible_graph.keys_iterator.map(node_elem).toList

    XML.Elem(Markup("svg", List(
      "class" -> "graph", "role" -> "img",
      "aria-label" -> label,
      "viewBox" -> (num(box.x) + " " + num(box.y) + " " + num(box.width) + " " + num(box.height)),
      "width" -> num(box.width), "height" -> num(box.height),
      "font-family" -> metrics.font.getFamily, "font-size" -> metrics.font.getSize.toString)),
      edges ::: nodes)
  }

  /* the session graph, and over it the box of a session holding the session's own graph,
     shown while the URL fragment names it. The frame is as tall as the tallest of them, so
     the page does not move when one replaces another; site.js zooms the box out of the
     session's node and back into it */
  val graph_label_height = 28   // px, as .graph-label in site.css

  def graph_figure(options: Options, stats: Stats): Option[XML.Elem] =
    if (stats.graph.is_empty) None
    else {
      def height(svg: XML.Elem): Double =
        Properties.get(svg.markup.properties, "height").map(_.toDouble).getOrElse(0.0)
      /* the label is in the graph's font, so the node's name stays as the node grows */
      def font(svg: XML.Elem): String =
        "font-family: " + Properties.get(svg.markup.properties, "font-family").getOrElse("") +
        "; font-size: " + Properties.get(svg.markup.properties, "font-size").getOrElse("") + "px"

      val overview_svg = graph_svg(options, stats, stats.graph, "Session dependencies")
      val overview =
        XML.Elem(Markup("div", List("class" -> "graph-overview", "id" -> sessions_graph_id)),
          List(overview_svg))

      val boxes =
        for (session <- stats.sessions if !session.graph.is_empty) yield {
          val svg = graph_svg(options, stats, session.graph, "Theory dependencies of " + session.name)
          val label =
            XML.Elem(Markup("div", List("class" -> "graph-label", "style" -> font(svg))),
              List(
                XML.Elem(Markup("a", List("class" -> "graph-back", "href" -> ("#" + sessions_graph_id))),
                  HTML.text("All sessions")),
                XML.Elem(Markup("span", List("class" -> "graph-name")), HTML.text(session.name))))
          (XML.Elem(Markup("div", List("class" -> "graph-session", "id" -> session.graph_id)),
            List(label, XML.Elem(Markup("div", List("class" -> "graph-body")), List(svg)))),
            height(svg) + graph_label_height + 2)
        }

      val frame_height = (height(overview_svg) :: boxes.map(_._2)).max.ceil.toInt
      Some(XML.Elem(Markup("figure", List("class" -> "graph")), List(
        XML.Elem(Markup("div", List("class" -> "graph-frame", "style" -> ("height: " + frame_height + "px"))),
          overview :: boxes.map(_._1)))))
    }

  def html(options: Options, stats: Stats): XML.Body = {
    def num(n: Int): String = {
      val s = n.toString
      s.reverse.grouped(3).mkString(" ").reverse      // thin spaces, so 12 345 reads
    }
    def cell(s: String): XML.Elem = XML.elem("td", HTML.text(s))
    def right(s: String): XML.Elem =
      XML.Elem(Markup("td", List("class" -> "num")), HTML.text(s))
    def head_cell(s: String): XML.Elem =
      XML.Elem(Markup("th", List("scope" -> "col")), HTML.text(s))

    /* the two commands as the sources render them */
    def command(s: String): XML.Body = List(HTML.code(List(HTML.span("keyword1 command", HTML.text(s)))))
    val lede =
      HTML.par(
        if (stats.ok)
          HTML.text("Every proof in the sessions published with this site is complete: no " +
            "occurrence of ") ::: command("sorry") ::: HTML.text(" or ") ::: command("oops") :::
          HTML.text(" anywhere in their theories. The numbers below come from the same build " +
            "the pages were rendered from.")
        else
          HTML.text("Some proofs in the sessions published with this site are skipped with ") :::
          command("sorry") ::: HTML.text(" or ") ::: command("oops") :::
          HTML.text(". The badge is red until they are discharged. The numbers below come " +
            "from the same build the pages were rendered from."))

    val summary =
      XML.elem("dl",
        List(
          "Sessions" -> num(stats.sessions.length),
          "Theories" -> num(stats.theories),
          "Lines of Isabelle" -> num(stats.lines),
          "Constants" -> num(stats.consts),
          "Types" -> num(stats.types),
          "Theorems" -> num(stats.thms),
          "Skipped proofs" -> num(stats.skipped),
          "Checked in" -> duration(stats.elapsed),
          "CPU time" -> duration(stats.cpu),
          "Isabelle" -> stats.isabelle
        ).flatMap({ case (k, v) => List(XML.elem("dt", HTML.text(k)), XML.elem("dd", HTML.text(v))) }))

    /* the list is narrow: the graph beside it, below it on a narrow display */
    val graphs = XML.Elem(Markup("div", List("class" -> "graphs")), graph_figure(options, stats).toList)
    val overview = XML.Elem(Markup("div", List("class" -> "overview")), List(summary, graphs))

    /* a session's tables; site.js shows the open box's alone, by its graph id */
    def session_table(session: Session): XML.Elem = {
      val rows =
        session.theories.sortBy(_.name).map(thy =>
          XML.elem("tr", List(
            XML.Elem(Markup("th", List("scope" -> "row")),
              List(HTML.link(
                session.dir + "/" + Long_Name.base_name(thy.name) + ".html",
                HTML.text(Long_Name.base_name(thy.name))))),
            right(num(thy.lines)), right(num(thy.consts)), right(num(thy.types)),
            right(num(thy.thms)), right(if (thy.skipped == 0) "–" else num(thy.skipped)))))
      val total =
        XML.Elem(Markup("tr", List("class" -> "total")), List(
          XML.Elem(Markup("th", List("scope" -> "row")), HTML.text("total")),
          right(num(session.lines)), right(num(session.consts)), right(num(session.types)),
          right(num(session.thms)), right(if (session.skipped == 0) "–" else num(session.skipped))))
      XML.Elem(Markup("section", List("class" -> "session-stat", "data-graph" -> session.graph_id)), List(
        XML.elem("h3", HTML.text(session.name)),
        HTML.par(HTML.text("Checked in " + duration(session.timing.elapsed) +
          ", using " + duration(session.timing.cpu) + " of CPU time.")),
        XML.Elem(Markup("div", List("class" -> "table")), List(
          XML.elem("table", List(
            XML.elem("thead", List(XML.elem("tr",
              head_cell("Theory") :: List("Lines", "Constants", "Types", "Theorems", "Skipped").map(s =>
                XML.Elem(Markup("th", List("scope" -> "col", "class" -> "num")), HTML.text(s)))))),
            XML.elem("tbody", rows ::: List(total))))))))
    }

    List(lede, overview,
      XML.Elem(Markup("div", List("class" -> "session-stats")), stats.sessions.map(session_table)))
  }
}
