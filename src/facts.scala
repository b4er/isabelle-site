/*  Title:      site/src/facts.scala

Resolution of @{thm}, @{const}, @{type} against the entity tables.
*/

package site

import isabelle._


object Facts {
  /* kinds */

  val THM = Markup.FACT
  val CONST = Markup.CONSTANT
  val TYPE = Markup.TYPE_NAME

  val kinds: Map[String, String] = Map("fact" -> THM, "thm" -> THM, "const" -> CONST, "type" -> TYPE)

  sealed case class Ref(kind: String, theory: String, entity: Export_Theory.Entity0) {
    def name: String = entity.name
    override def toString: String = kind + " " + quote(name)

    /* defining command span in the theory source */
    def def_range(snapshot: Document.Snapshot): Option[Text.Range] =
      Position.Range.unapply(entity.pos).map(Symbol.Text_Chunk(snapshot.node.source).decode)
  }


  /* name resolution: "name", "Theory.name" or "Session.Theory.name", as Isabelle accepts
     them, where the name is the entity's full one or the one its theory's name space
     shows (constructors as Theory.Con, class operations as Theory.op); an unqualified name
     that several theories define is an error, not a precedence question, because a page
     is read without the theory's name space in mind */

  /* None for a name no theory defines; an ambiguous name or an unknown theory is an error */
  def find(store: Site_Store, kind: String, name: String): Option[Ref] = {
    val parts = Long_Name.explode(name)
    if (parts.length < 2) find_short(store, kind, name)
    else find_long(store, kind, name, parts)
  }

  def resolve(store: Site_Store, kind: String, name: String): Ref =
    find(store, kind, name) getOrElse error("Unknown " + kind + " " + quote(name))

  private def find_short(store: Site_Store, kind: String, name: String): Option[Ref] = {
    val candidates =
      for {
        (thy_name, thy) <- store.entity_theories
        entity <- entities(thy, kind) if Long_Name.base_name(entity.name) == name
      } yield Ref(kind, thy_name, entity)
    candidates match {
      case List(ref) => Some(ref)
      case Nil => None
      case refs => error("Ambiguous " + kind + " " + quote(name) + ", qualify it: " +
        commas_quote(refs.map(r => Long_Name.base_name(r.theory) + "." + Long_Name.base_name(r.name))))
    }
  }

  private def find_long(store: Site_Store, kind: String, name: String, parts: List[String])
      : Option[Ref] = {
    val theories =
      for {
        i <- List(1, 2) if parts.length > i
        thy <-
          (if (i == 1) store.theory_names.filter(n => Long_Name.base_name(n) == parts.head)
           else store.theory_names.filter(_ == Long_Name.implode(parts.take(2))))
      } yield (i, thy)
    if (theories.isEmpty) error("Unknown theory in " + quote(name))

    val candidates =
      for {
        (i, thy) <- theories
        rest = Long_Name.implode(parts.drop(i))
        entity_name = Long_Name.qualify(Long_Name.base_name(thy), rest)
        entity <- entities(store.theory(thy), kind).filter(e => e.name == entity_name || e.xname == rest)
      } yield Ref(kind, thy, entity)

    candidates match {
      case List(ref) => Some(ref)
      case Nil => None
      case refs => error("Ambiguous " + kind + " " + quote(name) + ": " +
        commas_quote(refs.map(r => r.theory + "." + r.name)))
    }
  }

  /* any kind, unless one is given */
  def resolve_any(store: Site_Store, name: String, kind: Option[String]): Ref =
    kind match {
      case Some(k) => resolve(store, kinds.getOrElse(k, error("Unknown entity kind " + quote(k))), name)
      case None =>
        List(THM, CONST, TYPE).flatMap(k => find(store, k, name)) match {
          case List(ref) => ref
          case Nil => error("Unknown entity " + quote(name))
          case refs => error("Ambiguous entity " + quote(name) + ": " + commas(refs.map(_.toString)) +
            " (add a kind, e.g. (const))")
        }
    }

  private def entities(thy: Export_Theory.Theory, kind: String): List[Export_Theory.Entity0] =
    kind match {
      case THM => thy.thms.map(_.no_content)
      case CONST => thy.consts.map(_.no_content)
      case TYPE => thy.types.map(_.no_content)
      case _ => error("Bad entity kind " + quote(kind))
    }


  /* hover text: "name :: type" from the PIDE typing markup of the defining command,
     else "kind name" (entity contents such as types are exported only with export_theory) */

  def hover(store: Site_Store, ref: Ref): String = {
    val elems = store.elements(ref.theory)
    val base = Long_Name.base_name(ref.name)
    def typed(t: XML.Tree): Option[String] =
      t match {
        case XML.Wrapped_Elem(m, tip, body) if Source.hover_markup(m.name) && XML.content(body) == base =>
          Some(XML.content(tip))
        case XML.Elem(_, body) => body.iterator.flatMap(typed).nextOption()
        case _ => None
      }
    val tip =
      for {
        range <- ref.def_range(elems.snapshot)
        elem <- elems.containing(range.start)
        xml = elems.snapshot.xml_markup(range = elems.range(elem), elements = Markup.Elements.full)
        tip <- xml.iterator.flatMap(typed).nextOption()
      } yield ref.name + " :: " + tip
    tip getOrElse (ref.kind + " " + ref.name)
  }
}
