/*  Title:      site/src/site.scala

Isabelle command-line tool "site": generate a website from the page theories
of an Isabelle session, checking every formal reference against the session
store. The site configures itself in its own theory (see thy/site.ML).
*/

package site

import isabelle._


object Site {
  /* the tool's own resources */

  def home: Path = Path.explode("$SITE_HOME")


  /* the errors of all pages are reported together */

  private def check[A](progress: Progress, results: List[Exn.Result[A]]): List[A] = {
    val errors = results.collect({ case Exn.Exn(ERROR(msg)) => msg })
    if (errors.nonEmpty) {
      errors.foreach(msg => progress.echo_error_message(msg))
      error("Errors in " + errors.length + " page(s)")
    }
    results.collect({ case Exn.Res(r) => r })
  }


  /* the site configuration: every theory of the session exports it, and they agree */

  private def read_config(store: Site_Store): Config.Config = {
    val configs = store.session_theories.map(thy => thy.theory -> Config.read(store, thy.theory))
    configs.map(_._2).distinct match {
      case List(config) =>
        for ((what, path) <- List("logo" -> config.logo, "icon" -> config.icon)
          if path.nonEmpty && !Render.is_rooted(path))
          error("Site." + what + " " + quote(path) + " is not under " + commas(Render.root_dirs) +
            ": the path is relative to the site root, where only these directories are written")
        config
      case Nil => error("No theories in session " + store.session)
      case _ =>
        error("Site configuration differs between the theories of session " + store.session +
          " (" + commas(configs.map(_._1)) + "): configure the site in one theory only")
    }
  }


  /* the pages: session theories with a page command; the others configure the site */

  private def read_pages(store: Site_Store, config: Config.Config, progress: Progress): List[Page.Page] = {
    val pages =
      check(progress, store.session_theories.flatMap { thy =>
        if (progress.verbose) progress.echo("Theory " + thy.theory_base_name + " (" + thy.path + ")")
        Exn.capture(Page.parse(store, config, thy.path, thy.theory_base_name, thy.theory)) match {
          case Exn.Res(None) => None
          case Exn.Res(Some(page)) => Some(Exn.Res(page))
          case Exn.Exn(exn) => Some(Exn.Exn(exn))
        }
      })
    if (pages.isEmpty) error("No pages in session " + store.session + " (a page needs a page command)")
    if (!pages.exists(_.is_index))
      error("No landing page: session " + store.session + " has no theory " + config.index)
    for (page <- pages if page.name == Stats.page.name)
      error(page.where(1) + "the name " + quote(page.name) + " is reserved for the statistics page")
    for (page <- pages if page.name == References.page.name)
      error(page.where(1) + "the name " + quote(page.name) + " is reserved for the references page")
    pages
  }

  private def warn_optional(pages: List[Page.Page], progress: Progress): Unit =
    for (page <- pages; ratio = page.optional_ratio if ratio > page.optional_limit)
      progress.echo_warning(page.where(1) + "optional content is " + (ratio * 100).round +
        "% of the prose, over this page's limit of " + (page.optional_limit * 100).round +
        "%: move material into the core, or raise the limit on the page command")


  /* the sessions the site publishes as its sources: their dependencies are resolved once,
     for these sessions only, and shared by the statistics and the presentation */

  private def read_sources(store: Site_Store, config: Config.Config): (Sessions.Deps, Stats.Stats) = {
    for (session <- config.sources if !store.background.sessions_structure.defined(session))
      error("Unknown source session " + quote(session) + " (Site.sources in the site theory)")
    val deps = store.deps(config.sources)
    (deps, Stats.read(store, deps, config.sources))
  }


  /* the output directory: the fonts, the presented sources, and the assets every page
     links, in order: the generator's own (the base stylesheet and the script) and then the
     site's, which may not shadow them */

  private def prepare_output(store: Site_Store, config: Config.Config, deps: Sessions.Deps,
      out: Path, clean: Boolean, progress: Progress): List[String] = {
    if (clean) Isabelle_System.rm_tree(out)
    Isabelle_System.make_directory(out)
    if (config.sources.isEmpty) HTML.init_fonts(out)
    Theories.present(store, deps, config.sources, out, progress = progress)

    val target = Isabelle_System.make_directory(out + Path.basic("assets"))
    def files(dir: Path): List[String] = if (dir.is_dir) File.read_dir(dir) else Nil
    val own_dir = home + Path.basic("assets")
    val site_dir = store.session_dir + Path.basic("assets")
    val shadowed = files(own_dir).intersect(files(site_dir))
    if (shadowed.nonEmpty)
      error("Site assets shadow the generator's: " + commas_quote(shadowed) + " (in " + site_dir + ")")
    for (dir <- List(own_dir, site_dir) if dir.is_dir) Isabelle_System.copy_dir(dir, target, direct = true)
    List(own_dir, site_dir).flatMap(dir => files(dir).filter(a => a.endsWith(".css") || a.endsWith(".js")).sorted)
  }

  /* the machine-readable index of the site */
  private def write_json(out: Path, config: Config.Config, pages: List[Page.Page],
      states: List[Profile.State], stats: Stats.Stats): Unit =
    File.write(out + Path.basic("site.json"),
      JSON.Format(Map(
        "title" -> config.title,
        "axes" -> config.axes.map(a => Map("name" -> a.name, "title" -> a.title, "levels" -> a.levels)),
        "parts" -> config.parts.map(s => Map("id" -> s.id, "label" -> s.label)),
        "pages" -> pages.map(page => Map("name" -> page.name, "title" -> page.title,
          "part" -> page.part.getOrElse(""), "order" -> page.order,
          "items" -> Page.flatten(page.items).length,
          "optional_ratio" -> page.optional_ratio)),
        "profiles" -> states.map(_.name),
        "isabelle" -> Map(
          "ok" -> stats.ok, "sessions" -> stats.sessions.map(_.name), "theories" -> stats.theories,
          "lines" -> stats.lines, "constants" -> stats.consts, "types" -> stats.types,
          "theorems" -> stats.thms, "skipped" -> stats.skipped))))


  /* the commit of the site's session directory, short, and whether tracked files have
     local changes (untracked ones, like downloads and the output, do not count). Where git
     cannot read the repository, as in a CI container that does not own the checkout, the
     commit is GitHub Actions' GITHUB_SHA, a clean checkout; None without either */
  private def read_commit(dir: Path): Option[(String, Boolean)] = {
    val head = Isabelle_System.bash("git rev-parse --short HEAD", cwd = dir)
    if (head.ok) {
      val status = Isabelle_System.bash("git status --porcelain --untracked-files=no", cwd = dir)
      Some((head.out.trim, status.ok && status.out.trim.nonEmpty))
    }
    else proper_string(Isabelle_System.getenv("GITHUB_SHA")).map(sha => (sha.take(7), false))
  }


  /* main entry point */

  def site(
    options: Options,
    session: String,
    session_dirs: List[Path] = Nil,
    out_dir: Option[Path] = None,
    profiles: List[String] = Nil,
    no_build: Boolean = false,
    clean: Boolean = false,
    progress: Progress = new Progress
  ): Unit = {
    val dirs = session_dirs ::: List(home + Path.basic("thy"))

    if (!no_build) {
      val results =
        Build.build(options, selection = Sessions.Selection(sessions = List(session)),
          dirs = dirs, progress = progress)
      if (!results.ok) error("Build of session " + session + " failed")
    }

    using(new Site_Store(options, session, dirs, progress = progress)) { store =>
      val out = out_dir getOrElse (store.session_dir + Path.basic("out"))

      val config = read_config(store)
      val pages = read_pages(store, config, progress)
      val (source_deps, stats) = read_sources(store, config)

      /* the badge on every page, and the statistics page it links to, which has no theory */
      val stats_page = if (config.badge && !stats.is_empty) Some(Stats.page) else None
      val badge = stats_page.map(p => Stats.badge(stats, p.file_name))

      val footer =
        if (config.repository.isEmpty) None
        else Some(Render.footer(config.repository, read_commit(store.session_dir)))

      /* the references page, generated from the site's bib and the citations on the pages */
      val bib = if (config.bib.isEmpty) Map.empty[String, Bib.Entry] else Bib.read(store.session_dir + Path.explode(config.bib))
      val refs_page = if (config.bib.isEmpty) None else Some(References.page)
      val index = Bib.index(bib, Bib.cited(pages), config.cite_style)
      val site = (stats_page.toList ::: refs_page.toList ::: pages).map(p => p.name -> p).toMap

      /* render items once, then write one document per profile state */
      val links = new Theories.Links(store, config.sources)
      val render = new Render(store, config, site, out, links, bib, index)
      val rendered = check(progress, pages.map(page => Exn.capture((page, render.items(page)))))
      warn_optional(pages, progress)
      val states = render.profiles.matrix(profiles.map(render.profiles.parse_profile))

      val assets = prepare_output(store, config, source_deps, out, clean, progress)
      val template_file = store.session_dir + Path.basic("template.html")
      val template = if (template_file.is_file) File.read(template_file) else Render.default_template
      val stats_html = if (stats_page.isEmpty) Nil else Stats.html(store.options, stats)
      val refs_html = if (refs_page.isEmpty) Nil else References.html(index, render.citations.value, site)
      for (state <- states) {
        render.prepare_state(state)
        for ((page, items) <- rendered)
          render.write(page, items, states, state, assets, template, badge, footer)
        for (page <- stats_page)
          render.write(page, stats_html, states, state, assets, template, badge = None, footer)
        for (page <- refs_page)
          render.write(page, refs_html, states, state, assets, template, badge, footer)
      }
      write_json(out, config, pages, states, stats)

      progress.echo("Wrote " + pages.length + " page(s) in " + states.length +
        " profile state(s) to " + out.expand)
    }
  }


  /* Isabelle tool wrapper */

  val isabelle_tool =
    Isabelle_Tool("site", "generate a website from the page theories of a session",
      Scala_Project.here, { args =>
        var options = Options.init()
        var session_dirs: List[Path] = Nil
        var out_dir: Option[Path] = None
        var profiles: List[String] = Nil
        var no_build = false
        var clean = false
        var verbose = false

        val getopts = Getopts("""
Usage: isabelle site [OPTIONS] SESSION

  Options are:
    -O DIR       output directory (default: the session directory's out/)
    -c           clean output directory before generating
    -d DIR       include session directory
    -n           no build: trust the existing session store
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
    -p PROFILE   render an additional reader profile (e.g. fp=1,proof=0)
    -v           verbose mode

  Generate the website of SESSION: every theory with a page command becomes a
  page, and every formal reference is checked against the Isabelle session
  store. The site's axes, profiles and commands are declared in its theory
  (see thy/site.ML).
""",
          "O:" -> (arg => out_dir = Some(Path.explode(arg))),
          "c" -> (_ => clean = true),
          "d:" -> (arg => session_dirs = session_dirs ::: List(Path.explode(arg))),
          "n" -> (_ => no_build = true),
          "o:" -> (arg => options = options + arg),
          "p:" -> (arg => profiles = profiles ::: List(arg)),
          "v" -> (_ => verbose = true))

        val more_args = getopts(args)
        val session =
          more_args match {
            case List(session) => session
            case _ => getopts.usage()
          }

        val progress = new Console_Progress(verbose = verbose)

        site(options, session, session_dirs = session_dirs, out_dir = out_dir, profiles = profiles,
          no_build = no_build, clean = clean, progress = progress)
      })
}

class Tools extends Isabelle_Scala_Tools(Site.isabelle_tool)
