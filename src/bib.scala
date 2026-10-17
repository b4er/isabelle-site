/*  Title:      site/src/bib.scala

Citations: the site's .bib file read into entries, the \<^cite> markers, and the
generated References page listing the cited entries with where each is cited.
*/

package site

import isabelle._


object Bib {
  /* one entry of the bib file: its key, kind and fields, with the readings a page needs */
  sealed case class Entry(key: String, kind: String, fields: Map[String, String]) {
    def field(name: String): String = fields.getOrElse(name, "")

    def people: List[String] = {
      val a = if (field("author").nonEmpty) field("author") else field("editor")
      if (a.isEmpty) Nil else a.split("""\s+and\s+""").toList.map(_.trim).filter(_.nonEmpty)
    }
    /* "Milner, Robin" or "Robin Milner": the surname either way, a particle kept with it */
    def surnames: List[String] =
      people.map(p =>
        if (p.contains(",")) p.takeWhile(_ != ',').trim
        else {
          val words = p.split("""\s+""").toList
          val (rest, last) = (words.init, words.last)
          val particles = rest.reverse.takeWhile(w => Bib.particles(w.toLowerCase)).reverse
          (particles :+ last).mkString(" ")
        })

    def year: String = field("year")

    /* the marker's text: first author, "and" a second, "et al." for more, then the year */
    def label: String = {
      val who =
        surnames match {
          case Nil => key
          case List(a) => a
          case List(a, b) => a + " and " + b
          case a :: _ => a + " et al."
        }
      if (year.isEmpty) who else who + " " + year
    }

    def venue: String =
      List("journal", "booktitle", "school", "institution", "publisher", "howpublished")
        .map(field).find(_.nonEmpty).getOrElse("")

    /* the entry as one line of plain text, for a hover */
    def text: String =
      List(people.mkString(", "), field("title"), List(venue, year).filter(_.nonEmpty).mkString(" "))
        .filter(_.nonEmpty).mkString(". ") + "."

    /* the entry as HTML: authors, title, where and when, a link if the entry has one */
    def html: XML.Body = {
      val url = List("url", "doi", "ee").map(field).find(_.nonEmpty)
        .map(u => if (u.startsWith("10.")) "https://doi.org/" + u else u)
      val title = HTML.text(field("title"))
      val where = List(venue, year).filter(_.nonEmpty).mkString(", ")
      HTML.text(people.mkString(", ") + (if (people.isEmpty) "" else ". ")) :::
        List(XML.elem("em", url match { case Some(u) => List(HTML.link(u, title)) case None => title })) :::
        HTML.text(if (where.isEmpty) "." else ". " + where + ".")
    }
  }

  /* the part of a surname written before it: "De Giacomo", "van Rossum" */
  val particles: Set[String] =
    Set("de", "del", "della", "der", "den", "di", "du", "da", "la", "le", "van", "von", "ten", "ter", "zu")

  /* where a citation sits: a page, and the section on it if any */
  sealed case class Site_Ref(page: String, page_title: String, heading: Option[(String, String)])

  /* the keys the pages cite, in document order, read off the prose before rendering: the
     markers need their numbers before the references page is written */
  private val cite_marker = """\\<\^cite>(?:\\<open>|‹)(.*?)(?:\\<close>|›)""".r
  def keys(text: String): List[String] =
    text.trim.split("""\s+and\s+""").toList.map(_.trim).filter(_.nonEmpty)
  def cited(pages: List[Page.Page]): List[String] =
    (for {
      page <- pages
      case Page.Text(text, _) <- Page.flatten(page.items)
      m <- cite_marker.findAllMatchIn(text)
      key <- keys(m.group(1))
    } yield key).distinct

  /* the cited entries in the order of the references page, by first author and year, and
     the number each gets; the label of a marker follows the site's style */
  sealed case class Index(entries: List[Entry], style: String) {
    val numbers: Map[String, Int] = entries.map(_.key).zipWithIndex.map({ case (k, i) => k -> (i + 1) }).toMap
    def label(e: Entry): String =
      style match {
        case "author-year" => e.label
        case _ => numbers.get(e.key).map(_.toString).getOrElse(e.label)
      }
  }
  def index(bib: Map[String, Entry], cited: List[String], style: String): Index =
    Index(cited.flatMap(bib.get).sortBy(e => (e.surnames.headOption.getOrElse(e.key).toLowerCase, e.year, e.key)), style)


  /* reading the file: Isabelle's tokenizer gives the chunks, the fields are read off their
     tokens; a value keeps its text with braces and quotes removed */

  /* TeX accents and ligatures as Unicode: \'e, \"o, \`a, \^i, \~n, \=o, \.z, \c{c}, \ss, \o, \ae */
  private val accents: Map[Char, Char] =
    Map('\'' -> '\u0301', '"' -> '\u0308', '`' -> '\u0300', '^' -> '\u0302', '~' -> '\u0303',
      '=' -> '\u0304', '.' -> '\u0307', 'c' -> '\u0327', 'v' -> '\u030C', 'u' -> '\u0306', 'H' -> '\u030B')
  private val accent = """\\(['"`^~=.]|[cvuH](?=[\s{]))\s*\{?([A-Za-z])\}?""".r
  private val ligatures = List("\\ss" -> "ß", "\\ae" -> "æ", "\\AE" -> "Æ", "\\oe" -> "œ", "\\OE" -> "Œ",
    "\\o" -> "ø", "\\O" -> "Ø", "\\aa" -> "å", "\\AA" -> "Å", "\\l" -> "ł", "\\L" -> "Ł", "---" -> "\u2014", "--" -> "\u2013")
  def detex(s: String): String = {
    val s1 = accent.replaceAllIn(s, m => java.util.regex.Matcher.quoteReplacement(
      java.text.Normalizer.normalize(m.group(2) + accents(m.group(1).head), java.text.Normalizer.Form.NFC)))
    ligatures.foldLeft(s1)({ case (t, (a, b)) => t.replace(a, b) })
  }

  private def clean(s: String): String = {
    val t = s.trim
    val inner =
      if (t.startsWith("{") && t.endsWith("}") && t.length >= 2) t.substring(1, t.length - 1)
      else if (t.startsWith("\"") && t.endsWith("\"") && t.length >= 2) t.substring(1, t.length - 1)
      else t
    detex(inner).replaceAll("[{}]", "").replaceAll("""\s+""", " ").trim
  }

  private def fields(toks: List[Bibtex.Token]): Map[String, String] = {
    val result = Map.newBuilder[String, String]
    var rest = toks.filterNot(_.is_ignored)
    def kw(s: String)(t: Bibtex.Token): Boolean = t.kind == Bibtex.Token.Kind.KEYWORD && t.source == s
    while (rest.nonEmpty) {
      rest = rest.dropWhile(kw(","))
      rest match {
        case name :: eq :: more if name.is_name && kw("=")(eq) =>
          /* the value runs to the next comma at depth 0, or the end */
          var depth = 0
          val (value, tail) =
            more.span(t => {
              val stop = depth == 0 && kw(",")(t)
              if (kw("{")(t) || kw("(")(t)) depth += 1
              if (kw("}")(t) || kw(")")(t)) depth -= 1
              !stop
            })
          result += (name.source.toLowerCase -> clean(value.map(_.source).mkString))
          rest = tail
        case _ => rest = Nil
      }
    }
    result.result()
  }

  def read(path: Path): Map[String, Entry] = {
    if (!path.is_file) error("No bib file " + path + " (Site.bib in the site theory)")
    val chunks = Bibtex.parse(File.read(path))
    val bad = chunks.filter(_.is_malformed)
    if (bad.nonEmpty) error("Malformed bib file " + path + ": " + bad.head.source.take(60))
    val entries =
      for (chunk <- chunks if chunk.is_entry) yield {
        /* @kind{name, fields}: the tokens after the name are the fields */
        val toks = chunk.tokens.filterNot(_.is_ignored)
        val body = toks.dropWhile(t => !t.is_open).drop(1)
        val after_name = body.dropWhile(_.is_name)
        val inner = if (after_name.nonEmpty && (after_name.last.source == "}" || after_name.last.source == ")")) after_name.init else after_name
        Entry(chunk.name, chunk.kind.toLowerCase, fields(inner))
      }
    val dup = Library.duplicates(entries.map(_.key))
    if (dup.nonEmpty) error("Duplicate bib keys in " + path + ": " + commas_quote(dup))
    entries.map(e => e.key -> e).toMap
  }
}


object References {
  /* the references page: no theory, written in every profile state like the pages */
  val page: Page.Page =
    Page.Page(Path.basic("references"), "References", "", "References", Nil,
      is_index = false, optional_limit = 1.0, part = None, order = 0, control = Some(Nil))

  /* the cited entries in the index's order, each with where it is cited */
  def html(index: Bib.Index, citations: Map[String, List[Bib.Site_Ref]],
      site: Map[String, Page.Page]): XML.Body = {
    val cited = index.entries
    val lede =
      if (cited.isEmpty) List(HTML.par(HTML.text("There are no citations on this site."))) else Nil
    def place(r: Bib.Site_Ref): XML.Body = {
      val file = site.get(r.page).map(_.file_name).getOrElse(r.page + ".html")
      r.heading match {
        case Some((id, title)) => List(HTML.link(file + "#" + id, HTML.text(r.page_title + " › " + title)))
        case None => List(HTML.link(file, HTML.text(r.page_title)))
      }
    }
    def entry(e: Bib.Entry): List[XML.Elem] = {
      val where = citations.getOrElse(e.key, Nil)
      List(
        XML.Elem(Markup("dt", List("id" -> e.key)), HTML.text("[" + index.label(e) + "]")),
        XML.elem("dd",
          HTML.par(e.html) ::
            (if (where.isEmpty) Nil
             else List(XML.Elem(Markup("p", List("class" -> "cited-on")),
               HTML.text("Cited on ") ::: Library.separate(HTML.text(", "), where.map(place)).flatten :::
               HTML.text("."))))))
    }
    lede ::: List(XML.Elem(Markup("dl", List("class" -> "references")), cited.flatMap(entry)))
  }
}
