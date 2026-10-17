# isabelle-site

A website generator for Isabelle sessions as an Isabelle system component:
`isabelle site` generates plain HTML from Isabelle theories by reading types,
constants and theorems straight from the session store.

## Setup

Tested with Isabelle2025-2 and nothing else, register the component and build:

```
isabelle components -u "$PWD"
isabelle scala_build

isabelle getenv SITE_HOME     # should print this directory
isabelle site -d example Site_Example
```

## Usage

```
isabelle site [OPTIONS] SESSION
  -O DIR       output directory (default: the session directory's out/)
  -c           clean output directory before generating
  -d DIR       include session directory
  -n           no build: trust the existing session store
  -o OPTION    override Isabelle system OPTION
  -p PROFILE   render an additional reader profile
  -v           verbose mode
```

Without `-n` the tool first builds SESSION, which is also Isabelle's own check
of every page, with `-n` it trusts the session store.

## Layout

```
etc/settings      SITE_HOME="$COMPONENT"
etc/build.props   Isabelle/Scala module description (sources, services)
ROOTS             the sessions of the component: thy (Site)
thy/site.ML       the page commands: grammar, checking, exports
thy/Site.thy      base theory, session Site
src/              the generator, one file per concern
assets/           the base stylesheet and the script every generated site gets
example/          the example site
NOTES.md          decisions, in order
```

## Writing a site

A site is an ordinary Isabelle session whose theories import `Site.Site`:

```isabelle
theory My_Doc
  imports Site.Site
  keywords "why" :: document_body
begin

setup ‹
  Site.title "My Project" #>
  Site.logo "assets/logo.svg" #>
  Site.icon "assets/icon.svg" #>
  Site.parts [("start", "Get started"), ("docs", "Documentation")] #>
  Site.links [("GitHub", "https://example.com")] #>
  Site.sources ["my_development"] #>
  Site.badge false #>
  Site.bib "document/root.bib" #>
  Site.cite_audience "proof>=result" #>
  Site.cite_style "author-year" #>
  Site.local_links false #>
  Site.axes
   [("fp", "Functional programming", ["none", "ml", "haskell"]),
    ("goal", "Goal", ["evaluate", "learn", "contribute"])] #>
  Site.profiles [("hello", ["none", "learn"]), ("dev", ["haskell", "contribute"])] #>
  Site.axis_tags
   [("fp", [("", ""), ("OCaml", "Asides on OCaml."), ("Haskell", "Asides on Haskell.")])] #>
  Site.control_label "Asides"             (* before the tags in the header; default "Reader profile" *)
›

ML ‹Site.block_command \<^command_keyword>‹why› "fp=none"›
ML ‹Site.checked_command \<^command_keyword>‹hol› "a HOL term"
  (fn ctxt => fn src => ignore (Syntax.read_term ctxt (Syntax.implode_input src)))›

end
```

Pages then import that theory and the development they cite. `example/site/` is
a complete site to copy from.

The page shell is `template.html` in the session directory when the site has
one, with the holes `{{head}}` `{{title}}` `{{body_attrs}}` `{{index}}`
`{{site}}` `{{brand}}` `{{nav}}` `{{control}}` `{{rail}}` `{{page_title}}`
`{{content}}` `{{pager}}` `{{badge}}`; an unknown hole is an error. Without one
the generator uses its default.

### Items

Every command has the same shape, and containers nest:

```
name (key: value, class, ...) ‹body›
```

```isabelle
theory Datatypes
  imports Example_Site Site_Example_Dev.Tree
begin

page (part: guide, order: 10) ‹Datatypes› (* exactly one per page *)

text ‹Core prose, always shown.›

section ‹A heading›  (* <h2> (subsection gives <h3>) *)

(* any container; attrs become HTML attributes *)
div (columns) ‹
  ‹A bare cartouche inside a container is prose.›

  pre (code) ‹mirror Leaf = Leaf›
›

why ‹Prose that only some readers see.›
block (audience: "proofs=shown") ‹…›

(* defining command of a fact, const or type *)
source ‹Tree.mirror›

(* kind, if the name is ambiguous *)
source (kind: type) ‹Tree.tree›

(* facts as printed *)
doc_thm (rule, display) ‹Tree.mirror_mirror[of t]›

(* raw HTML *)
html ‹<figure><img src="assets/x.svg" alt="…"></figure>›

(* a checked block: rendered from its markup *)
hol ‹inorder (mirror Leaf)›

end
```
