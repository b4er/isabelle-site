/*  Title:      site/src/theories.scala

The Isabelle sources of the presented sessions, as HTML under src/, and the
links from a reference on a page into them. This is Isabelle's own session
presentation (browser_info), rooted inside the site rather than beside it, so
a name in the prose and the theory that defines it share one URL space.
*/

package site

import isabelle._


object Theories {
  val dir: String = "src"     // site-root relative, cf. Render.root_dirs

  /* the Isabelle fonts: Browser_Info puts a copy under src/ and the pages share it; a site
     without sources gets its own at the root (Site.prepare_output) */
  def fonts_root(out_dir: Path, sessions: List[String]): Path =
    if (sessions.nonEmpty) out_dir + Path.basic(dir) else out_dir

  /* presentation of the sessions the site declares as its sources */

  def present(store: Site_Store, deps: Sessions.Deps, sessions: List[String], out_dir: Path,
      progress: Progress = new Progress): Unit = {
    if (sessions.nonEmpty) {
      val root_dir = out_dir + Path.basic(dir)
      using(Export.open_database_context(store.store)) { database_context =>
        /* a presented session carries the uuid of the build it came from: when that is the
           current build, it is not presented again, as in Isabelle's Browser_Info.build */
        val context0 = Browser_Info.context(deps.sessions_structure, root_dir = root_dir)
        val outdated = sessions.filter { session =>
          using(database_context.open_database(session)) { session_database =>
            store.store.read_build(session_database.db, session) match {
              case Some(build) =>
                !Browser_Info.Meta_Info.check_build_uuid(context0.session_dir(session), build.uuid)
              case None => true
            }
          }
        }
        for (session <- sessions if !outdated.contains(session))
          progress.echo("Presentation of " + session + " is current", verbose = true)
        if (outdated.nonEmpty) {
          val document_info = Document_Info.read(database_context, deps, outdated)
          val context =
            Browser_Info.context(deps.sessions_structure, root_dir = root_dir,
              document_info = document_info)
          context.update_root()
          for (session <- outdated) {
            using(database_context.open_session(deps.background(session))) { session_context =>
              Browser_Info.build_session(context, session_context, progress = progress)
            }
          }
        }
      }
    }
  }


  /* links into the presented sources: the file is Isabelle's, the anchor is the entity's
     export name, which is how browser_info itself addresses a definition */

  class Links(store: Site_Store, sessions: List[String]) extends Source.Links {
    private val structure = store.background.sessions_structure

    private def session_of(theory: String): String = structure.theory_qualifier(theory)

    private lazy val index: Map[(String, String), String] = {
      val entries =
        for {
          session <- sessions
          chapter = structure(session).chapter
          theory <- store.theory_names if session_of(theory) == session
          thy <- (Exn.capture(store.theory(theory)) match {
            case Exn.Res(thy) => List(thy)
            case Exn.Exn(_) => Nil      // a theory without entity tables has nothing to link to
          })
          (kind, entity) <-
            thy.consts.map(e => Facts.CONST -> e.no_content) :::
            thy.types.map(e => Facts.TYPE -> e.no_content) :::
            thy.thms.map(e => Facts.THM -> e.no_content)
        } yield {
          val href =
            dir + "/" + chapter + "/" + session + "/" + Long_Name.base_name(theory) + ".html#" +
              entity.kname
          (kind, entity.name) -> href
        }
      entries.toMap
    }

    def entity(kind: String, name: String): Option[String] = index.get((kind, name))
  }
}
