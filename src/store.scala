/*  Title:      site/src/store.scala

Access to the Isabelle session store: page theories and syntax of the site
session, entity tables and PIDE snapshots of everything it imports, memoised
per theory. All reads go through Export.
*/

package site

import isabelle._


class Site_Store(
  val options: Options,
  val session: String,
  dirs: List[Path],
  progress: Progress = new Progress
) extends AutoCloseable {
  val store: Store = Store(options)

  /* the site session: its own theories and their syntax (from theory headers, no build needed) */

  val background: Sessions.Background =
    Sessions.background(options, session, dirs = dirs, progress = progress)

  def syntax: Outer_Syntax = background.base.overall_syntax

  val session_dir: Path = background.sessions_structure(session).dir

  /* theories defined by this session (not by its parents) */
  val session_theories: List[Document.Node.Name] = background.base.proper_session_theories


  /* exports of the site session: the pages are in there, so it has to be built */

  val session_context: Export.Session_Context = Export.open_session_context(store, background)

  def close(): Unit = session_context.close()


  /* page items: the checked trees of a theory's page commands, by the symbol offset of the
     command (thy/site.ML, export_item) */

  private val item_prefix = "site/item/"

  private lazy val export_names: List[Export.Entry_Name] = session_context.entry_names()

  def page_items(theory: String): Map[Int, XML.Body] = {
    val context = session_context.theory(theory)
    (for {
      entry <- export_names.iterator
      if entry.theory == theory && entry.name.startsWith(item_prefix)
      offset <- Value.Int.unapply(entry.name.substring(item_prefix.length).stripSuffix(".xml"))
    } yield offset -> context(entry.name).yxml()).toMap
  }

  private def base: Sessions.Base = session_context.session_base


  /* theory names: long names of everything loaded in the session hierarchy */

  val theory_names: List[String] = base.loaded_theories.keys.sorted


  /* memo tables: the value is computed outside the lock, so parallel readers do not
     serialise on each other's reads; a duplicate computation is harmless and discarded */

  private def memo[A](table: Synchronized[Map[String, A]], name: String)(compute: => A): A =
    table.value.get(name) getOrElse {
      val a = compute
      table.change_result(map =>
        map.get(name) match {
          case Some(a1) => (a1, map)
          case None => (a, map + (name -> a))
        })
    }


  /* entity tables (names and positions; contents only with export_theory) */

  private val theories = Synchronized(Map.empty[String, Export_Theory.Theory])

  def theory(name: String): Export_Theory.Theory =
    memo(theories, name) {
      val thy = Export_Theory.read_theory(session_context.theory(name))
      if (thy.consts.isEmpty && thy.types.isEmpty && thy.thms.isEmpty)
        error("No entity tables for theory " + quote(name))
      thy
    }


  /* short-name lookup: every theory of the hierarchy that has entity tables. Built once,
     in parallel, and only when a page actually writes an unqualified name. */

  lazy val entity_theories: List[(String, Export_Theory.Theory)] = {
    progress.echo("Indexing " + theory_names.length + " theories for short names", verbose = true)
    Par_List.map((name: String) => Exn.capture(name -> theory(name)), theory_names)
      .collect({ case Exn.Res(res) => res })
  }


  /* PIDE snapshots (available for every normal build) */

  private val snapshots = Synchronized(Map.empty[String, Document.Snapshot])

  def snapshot(name: String): Document.Snapshot =
    memo(snapshots, name) {
      Build.read_theory(session_context.theory(name)) getOrElse
        error("No PIDE markup for theory " + quote(name))
    }

  /* the commands of a theory grouped into elements (statement + proof): parsed once, since
     every reference into the theory selects one */
  private val elements = Synchronized(Map.empty[String, Source.Elements])

  def elements(name: String): Source.Elements =
    memo(elements, name) { Source.elements(syntax, snapshot(name)) }


  /* session dependencies, resolved for a selection of sessions and their ancestors only:
     Sessions.deps over the whole structure would resolve every session that ISABELLE_HOME
     and the -d directories make visible */

  def deps(sessions: List[String]): Sessions.Deps =
    Sessions.deps(background.sessions_structure.selection(Sessions.Selection(sessions = sessions)),
      progress = progress)
}
