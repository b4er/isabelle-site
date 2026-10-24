signature SITE =
sig
  type attrs = (string * (string * Position.T)) list

  datatype item =
      Text of Input.source
    | Item of (string * Position.T) * attrs * Input.source

  datatype kind =
      Container
    | Prose of {markdown: bool}
    | Verbatim
    | Thms
    | Block of {audience: string option}
    | Checked of {check: Proof.context -> Input.source -> unit}

  val title: string -> theory -> theory
  val logo: string -> theory -> theory
  val icon: string -> theory -> theory
  val badge: bool -> theory -> theory
  val bib: string -> theory -> theory
  val cite_audience: string -> theory -> theory
  val cite_style: string -> theory -> theory
  val local_links: bool -> theory -> theory
  val index: string -> theory -> theory
  val parts: (string * string) list -> theory -> theory
  val links: (string * string) list -> theory -> theory
  val repository: string -> theory -> theory
  val sources: string list -> theory -> theory
  val axes: (string * string * string list) list -> theory -> theory
  val profiles: (string * string list) list -> theory -> theory
  val axis_tags: (string * (string * string) list) list -> theory -> theory
  val control_label: string -> theory -> theory
  val declare: string -> kind -> theory -> theory
  val check_audience: Proof.context -> string * Position.T -> unit
  val check_page_attrs: Proof.context -> attrs -> unit
  val attrs: attrs parser
  val read_items: Proof.context -> Input.source -> item list
  val read_container: Proof.context -> Input.source -> item list option
  val check_item: Proof.context -> item -> XML.tree
  val check_items: Proof.context -> item list -> XML.tree list
  val check_container: Proof.context -> Input.source -> XML.tree list
  val config: theory -> XML.tree
  val container_command: Outer_Syntax.command_keyword -> string -> unit
  val verbatim_command: Outer_Syntax.command_keyword -> string -> unit
  val checked_command: Outer_Syntax.command_keyword -> string ->
    (Proof.context -> Input.source -> unit) -> unit
  val block_command: Outer_Syntax.command_keyword -> string -> unit
  val thms_command: Outer_Syntax.command_keyword -> string -> unit
  val page_command: Outer_Syntax.command_keyword -> string -> unit
end;

(*a setting of the site, with the merge every setting shares*)
functor Site_Setting(eqtype T val empty: T) =
  Theory_Data(type T = T val empty = empty fun merge (x, y) = if x = empty then y else x);

structure Site: SITE =
struct

(** site configuration: reader axes, named profiles, item kinds **)

type attrs = (string * (string * Position.T)) list;

datatype item =
    Text of Input.source
  | Item of (string * Position.T) * attrs * Input.source;

datatype kind =
    Container
  | Prose of {markdown: bool}
  | Verbatim
  | Thms
  | Block of {audience: string option}
  | Checked of {check: Proof.context -> Input.source -> unit};

structure Title = Site_Setting(type T = string val empty = "");          (* in <title> and the header *)
structure Logo = Site_Setting(type T = string val empty = "");           (* site-root relative image for the header *)
structure Icon = Site_Setting(type T = string val empty = "");           (* site-root relative image for the favicon *)
structure Index = Site_Setting(type T = string val empty = "Index");     (* theory of the landing page *)
structure Parts = Site_Setting(type T = (string * string) list val empty = []);   (* id, label: the top-level navigation *)
structure Links = Site_Setting(type T = (string * string) list val empty = []);   (* label, URL: nav items that leave the site *)
structure Repository = Site_Setting(type T = string val empty = "");   (* URL: the footer links it and the commit the site was built from *)
structure Sources = Site_Setting(type T = string list val empty = []);   (* sessions presented as HTML under src/ *)
structure Axes = Site_Setting(type T = (string * string * string list) list val empty = []);   (* name, title, level names *)
structure Profiles = Site_Setting(type T = (string * int list) list val empty = []);   (* name, one level per axis *)
structure Axis_Tags = Site_Setting(type T = (string * (string * string) list) list val empty = []);   (* axis, per level: tag, hint *)
structure Control_Label = Site_Setting(type T = string val empty = "");   (* before the tags in the header; "" for "Reader profile" *)
structure Bib = Site_Setting(type T = string val empty = "");   (* session-dir relative .bib the citations resolve in *)
structure Cite_Audience = Site_Setting(type T = string val empty = "");   (* who sees a citation marker; "" for everyone *)
structure Cite_Style = Site_Setting(type T = string val empty = "");   (* "number" (default) or "author-year" *)

structure Badge = Theory_Data(type T = bool val empty = true fun merge (x, y) = x andalso y);

structure Local_Links = Theory_Data(type T = bool val empty = true fun merge (x, y) = x andalso y);

structure Commands = Theory_Data
(
  type T = (kind * bool) Symtab.table;
  val empty = Symtab.empty;
  val merge = Symtab.merge (K true);
);

val title = Title.put;
val logo = Logo.put;
val icon = Icon.put;
val index = Index.put;
val links = Links.put;
val repository = Repository.put;
val sources = Sources.put;
val badge = Badge.put;
val bib = Bib.put;
val cite_audience = Cite_Audience.put;
fun cite_style s =
  if member (op =) ["number", "author-year"] s then Cite_Style.put s
  else error ("Bad citation style " ^ quote s ^ ": expected number or author-year");
val local_links = Local_Links.put;

fun parts x =
  if has_duplicates (op =) (map #1 x) then error "Duplicate part id" else Parts.put x;

fun axes x =
  if has_duplicates (op =) (map #1 x) then error "Duplicate axis name" else Axes.put x;

(* a profile names one level per axis, in axis order *)
fun profiles profiles' thy =
  let
    val axes = Axes.get thy;
    fun level (name, _, levels) l =
      (case find_index (fn l' => l' = l) levels of
        ~1 => error ("Bad level " ^ quote l ^ " for axis " ^ name ^ ": expected " ^ commas levels)
      | i => i);
    fun profile (name, levels) =
      if length levels <> length axes
      then error ("Profile " ^ quote name ^ " needs one level per axis: " ^ commas (map #1 axes))
      else (name, map2 level axes levels);
  in Profiles.put (map profile profiles') thy end;

(* per axis, one (tag, hint) per level *)
fun axis_tags tags thy =
  let
    val axes = Axes.get thy;
    fun check (a, ts) =
      (case find_first (fn (name, _, _) => name = a) axes of
        NONE => error ("Unknown axis " ^ quote a ^ ": expected " ^ commas (map #1 axes))
      | SOME (_, _, levels) =>
          if length ts <> length levels
          then error ("Axis " ^ a ^ " needs one tag and hint per level: " ^ commas levels)
          else ());
  in
    if has_duplicates (op =) (map #1 tags) then error "Duplicate axis in tags"
    else (List.app check tags; Axis_Tags.put tags thy)
  end;

val control_label = Control_Label.put;

fun add_command external name kind = Commands.map (Symtab.update (name, (kind, external)));

(* an existing document command (text, section, ...) that a page uses as an item *)
val declare = add_command true;

fun kind_of ctxt (name, pos) =
  (case Symtab.lookup (Commands.get (Proof_Context.theory_of ctxt)) name of
    SOME (kind, _) => kind
  | NONE => error ("Unknown page item " ^ quote name ^ Position.here pos));


(** audience predicates: "axis op level" atoms, space = and, "|" = or **)

val ops = [">=", "<=", "!=", "="];

fun check_atom axes atom =
  (case get_first (fn op' =>
      (case first_field op' atom of SOME (a, l) => SOME (a, op', l) | NONE => NONE)) ops of
    NONE => error ("Bad audience atom " ^ quote atom ^ ": expected axis op level with op one of " ^
      space_implode " " ops)
  | SOME (a, _, l) =>
      (case find_first (fn (name, _, _) => name = a) axes of
        NONE => error ("Unknown axis " ^ quote a ^ " in audience atom " ^ quote atom ^
          ": expected " ^ commas (map #1 axes))
      | SOME (_, _, levels) =>
          if member (op =) levels l orelse
            (case Int.fromString l of SOME i => 0 <= i andalso i < length levels | NONE => false)
          then ()
          else error ("Bad level " ^ quote l ^ " in audience atom " ^ quote atom ^ ": expected " ^
            commas levels ^ " or 0.." ^ string_of_int (length levels - 1))));

fun check_audience ctxt (pred, pos) =
  let
    val axes = Axes.get (Proof_Context.theory_of ctxt);
    val conjs = map (filter (fn s => s <> "") o space_explode " ") (space_explode "|" pred);
    val _ = if exists null conjs then error ("Empty audience predicate " ^ quote pred) else ();
  in List.app (List.app (check_atom axes)) conjs end
  handle ERROR msg => error (msg ^ Position.here pos);

fun check_page_attrs ctxt attrs =
  let
    val thy = Proof_Context.theory_of ctxt;
    val parts = Parts.get thy;
    val axes = Axes.get thy;
    fun check_part (id, pos) =
      if exists (fn (i, _) => i = id) parts then ()
      else error ("Unknown part " ^ quote id ^ ": expected " ^ commas (map #1 parts) ^
        Position.here pos);
    fun check_control (spec, pos) =
      if spec = "none" then ()
      else
        space_explode " " spec |> filter (fn s => s <> "") |> List.app (fn a =>
          if exists (fn (name, _, _) => name = a) axes then ()
          else error ("Unknown axis " ^ quote a ^ " in control: expected " ^
            commas (map #1 axes) ^ " or \"none\"" ^ Position.here pos));
    fun check_rail (v, pos) =
      if v = "none" then ()
      else error ("Bad rail " ^ quote v ^ ": expected \"none\"" ^ Position.here pos);
    fun check_order (v, pos) =
      (case Int.fromString v of
        SOME _ => ()
      | NONE => error ("Bad order " ^ quote v ^ ": expected an integer" ^ Position.here pos));
    fun check_optional (v, pos) =
      (case Int.fromString v of
        SOME i => if 0 <= i andalso i <= 100 then () else raise Fail ""
      | NONE => raise Fail "")
      handle Fail _ =>
        error ("Bad optional limit " ^ quote v ^ ": expected a percentage 0..100" ^ Position.here pos);
  in
    List.app (fn (key, value) =>
      (case key of
        "part" => check_part value
      | "control" => check_control value
      | "rail" => check_rail value
      | "order" => check_order value
      | "optional" => check_optional value
      | "class" => ()
      | _ => error ("Unknown page attribute " ^ quote key ^
          ": expected part, order, control, rail or optional" ^ Position.here (#2 value)))) attrs
  end;


(** syntax **)

val attr_name = Parse.command || Parse.name;

val attr =
  attr_name -- Scan.option (\<^keyword>\<open>:\<close> |-- Parse.!!! (Parse.position Parse.name))
    >> (fn (k, SOME v) => (k, v) | (k, NONE) => ("class", (k, Position.none)));

fun merge_attrs attrs =
  distinct (op =) (map #1 attrs) |> map (fn key =>
    let val vals = map_filter (fn (k, v) => if k = key then SOME v else NONE) attrs
    in (key, (space_implode " " (map #1 vals), #2 (hd vals))) end);

val attrs =
  Scan.optional (\<^keyword>\<open>(\<close> |-- Parse.!!! (Parse.list1 attr --| \<^keyword>\<open>)\<close>)) []
    >> merge_attrs;

val item =
  Parse.input Parse.cartouche >> Text ||
  Parse.position (Parse.command || Parse.name) -- attrs -- Parse.input Parse.cartouche
    >> (fn ((name, attrs), body) => Item (name, attrs, body));

fun read_items ctxt input =
  let
    val keywords = Thy_Header.get_keywords' ctxt;
    val toks =
      Token.tokenize keywords {strict = true} (Input.source_explode input)
      |> filter Token.is_proper;
    val _ = Context_Position.reports_text ctxt (maps (Token.reports keywords) toks);
    val (items, rest) = Scan.finite Token.stopper (Scan.repeat item) toks;
  in
    (case rest of
      [] => items
    | tok :: _ =>
        error ("Unexpected " ^ Token.print tok ^ " in page items" ^ Position.here (Token.pos_of tok)))
  end;

fun attr_value attrs key = Option.map #1 (AList.lookup (op =) attrs key);

fun item_shaped toks =
  let
    fun line tok = Position.line_of (Token.pos_of tok);
    fun opens next =
      Token.kind_of next = Token.Cartouche orelse Token.keyword_with (fn s => s = "(") next;
    fun go _ [] = NONE
      | go prev (tok :: rest) =
          if Token.is_command tok andalso (case prev of NONE => true | SOME p => line p <> line tok)
            andalso (case rest of next :: _ => opens next | [] => false)
          then SOME tok else go (SOME tok) rest;
  in go NONE toks end;

fun read_container ctxt input =
  let
    val keywords = Thy_Header.get_keywords' ctxt;
    val toks =
      the_default [] (try (filter Token.is_proper o
        Token.tokenize keywords {strict = false} o Input.source_explode) input);
    fun items_of items = if exists (fn Item _ => true | Text _ => false) items then SOME items else NONE;
  in
    (case item_shaped toks of
      SOME tok =>
        items_of (read_items ctxt input handle ERROR msg =>
          error ("Container body is read as page items, since it has the item " ^
            quote (Token.content_of tok) ^ Position.here (Token.pos_of tok) ^
            ": prose between items is a text item or a bare cartouche\n" ^ msg))
    | NONE => Option.mapPartial items_of (try (read_items ctxt) input))
  end;


(** checking: an item is checked and returned as the XML the generator renders **)

val markup_with_body = ["cartouche", "emph", "bold", "verbatim", "url", "cite"];
val markup_alone = ["item", "enum"];
val antiquotations = ["thm", "const", "type"];

fun check_markup markdown src =
  let
    fun plain what pos =
      if markdown then ()
      else error ("A heading is plain text on this site: no " ^ what ^ Position.here pos);
    val control = Symbol.encode o Symbol.Control;
    fun check (Antiquote.Text _) = ()
      | check (Antiquote.Control {name = (name, pos), body, ...}) =
          let val _ = plain "markup" pos in
            if null body then
              if member (op =) markup_alone name then ()
              else error ("Unsupported markup " ^ quote (control name) ^ " on this site" ^
                Position.here pos)
            else if member (op =) markup_with_body name then
              (if name = "emph" orelse name = "bold"
               then List.app check (Antiquote.parse_comments pos (Symbol_Pos.cartouche_content body))
               else ())
            else error ("Unsupported markup " ^ quote (control name ^ Symbol.open_ ^ "..." ^ Symbol.close) ^
              " on this site (expected " ^ commas (map control (tl markup_with_body)) ^
              ", or a bare cartouche for code)" ^ Position.here pos)
          end
      | check (Antiquote.Antiq {body, range = (pos, _), ...}) =
          let
            val _ = plain "antiquotation" pos;
            val toks = Token.tokenize Keyword.empty_keywords {strict = false} body |> filter Token.is_proper;
            val is_name = Token.is_kind Token.Ident orf Token.is_kind Token.Long_Ident orf
              Token.is_kind Token.Sym_Ident orf Token.is_kind Token.String;
            val ok =
              (case toks of
                [kind, name] =>
                  Token.is_kind Token.Ident kind andalso
                  member (op =) antiquotations (Token.content_of kind) andalso is_name name
              | _ => false);
          in
            if ok then ()
            else error ("Unsupported antiquotation on this site: expected @{" ^
              space_implode "|" antiquotations ^ " name}, with no attributes" ^ Position.here pos)
          end;
  in List.app check (Antiquote.read_comments src) end;

fun check_prose ctxt markdown src =
  let
    val _ = Context_Position.reports ctxt (Document_Output.document_reports src);
    val _ = Document_Output.output_document ctxt {markdown = markdown} src;
  in check_markup markdown src end;

fun line_props pos = [("line", string_of_int (the_default 0 (Position.line_of pos)))];

fun text_xml src =
  XML.Elem (("text", line_props (Input.pos_of src)), [XML.Text (Input.string_of src)]);

fun body_xml src = XML.Elem (("body", []), [XML.Text (Input.string_of src)]);

fun range_props src =
  Position.properties_of (Position.range_position (Symbol_Pos.range (Input.source_explode src)))
  |> filter (fn (k, _) => k = Markup.offsetN orelse k = Markup.end_offsetN);

fun attr_xml (key, (value, _)) = XML.Elem (("attr", [("key", key), ("value", value)]), []);


(* doc_thm: facts as printed, as semantic XML *)

local

fun class_of name =
  if name = Markup.freeN then SOME "free"
  else if name = Markup.boundN then SOME "bound"
  else if name = Markup.tfreeN then SOME "tfree"
  else if name = Markup.tvarN then SOME "tvar"
  else if name = Markup.varN then SOME "var"
  else NONE;

fun is_layout name = name = Markup.blockN orelse name = Markup.breakN orelse name = Markup.fbreakN;

fun const_name (XML.Text _) = NONE
  | const_name (XML.Elem ((name, props), body)) =
      if name = Markup.entityN andalso Properties.get props Markup.kindN = SOME "constant"
      then Properties.get props Markup.nameN else get_first const_name body;

fun span attrs body = XML.Elem (("span", attrs), body);

fun spans (t as XML.Text _) = [t]
  | spans (elem as XML.Elem ((name, props), body)) =
      (case XML.unwrap_elem elem of
        SOME (((wrapped, _), hover), content) =>
          let val inner = maps spans content in
            if wrapped = Markup.typingN then
              let
                val prefix = (case get_first const_name content of SOME c => c ^ " :: " | NONE => "");
              in [span [("class", "typed"), ("tip", prefix ^ XML.content_of hover)] inner] end
            else inner
          end
      | NONE =>
          let val inner = maps spans body in
            if is_layout name then [XML.Elem ((name, props), inner)]
            else if name = Markup.entityN then
              (case (Properties.get props Markup.kindN, Properties.get props Markup.nameN) of
                (SOME kind, SOME n) => [span [("class", "entity"), ("kind", kind), ("name", n)] inner]
              | _ => inner)
            else (case class_of name of SOME c => [span [("class", c)] inner] | NONE => inner)
          end);

val pretty_to_xml = Pretty.symbolic_output #> Bytes.content #> YXML.parse_body #> maps spans;

fun render_normal ctxt th =
  XML.Elem (("normal", []), pretty_to_xml (Thm.pretty_thm ctxt th));

fun render_rule ctxt th =
  let
    val (prems, concl) = Logic.strip_horn (Thm.prop_of th);
    fun t tm = XML.Elem (("t", []), pretty_to_xml (Syntax.pretty_term ctxt tm));
  in
    XML.Elem (("rule", []),
      [XML.Elem (("prems", []), map t prems), XML.Elem (("concl", []), [t concl])])
  end;

fun fact_name ctxt (Facts.Named ((name, pos), _)) _ =
      Facts.check (Context.Proof ctxt) (Global_Theory.facts_of (Proof_Context.theory_of ctxt)) (name, pos)
  | fact_name ctxt _ th =
      (case Thm_Name.short (Thm.derivation_name th) of
        "" => error ("Unnamed theorem: " ^ Thm.string_of_thm ctxt th)
      | name => name);

fun freeze ctxt th = Variable.import true [th] ctxt |> #1 |> #2 |> the_single;

val thm_options = ["rule", "display"];

in

fun check_thms ctxt (name, pos) attrs body =
  let
    val thy = Proof_Context.theory_of ctxt;
    val ctxt' = ctxt |> Config.put Printer.show_markup true;
    val opts = space_explode " " (the_default "" (attr_value attrs "class")) |> filter (fn s => s <> "");
    val _ =
      opts |> List.app (fn opt =>
        if member (op =) thm_options opt then ()
        else error ("Unknown " ^ name ^ " option " ^ quote opt ^ " (expected " ^ commas thm_options ^ ")" ^
          Position.here pos));
    val has = member (op =) opts;
    val prepare = if has "display" then freeze ctxt' else I;
    val render = if has "rule" then render_rule else render_normal;
    val refs = Parse.read_embedded ctxt (Thy_Header.get_keywords thy) Parse.thms1 body;
    val thms =
      refs |> maps (fn (fact_ref, srcs) =>
        Attrib.eval_thms ctxt' [(fact_ref, srcs)] |> map (fn th => (fact_name ctxt' fact_ref th, th)));
  in thms |> map (fn (n, th) => XML.Elem (("thm", [("name", n)]), [render ctxt' (prepare th)])) end;

end;

fun check_item ctxt (Text src) = (check_prose ctxt true src; text_xml src)
  | check_item ctxt (Item (name as (cmd, pos), attrs, body)) =
      let
        fun item kind props content =
          XML.Elem (("item", ("name", cmd) :: ("kind", kind) :: line_props pos @ props),
            map attr_xml attrs @ content);
      in
        (case kind_of ctxt name of
          Container => item "container" [] (check_container ctxt body)
        | Block {audience} =>
            let
              val pred =
                (case (AList.lookup (op =) attrs "audience", audience) of
                  (SOME pred, _) => (check_audience ctxt pred; #1 pred)
                | (NONE, SOME pred) => pred
                | (NONE, NONE) => error ("Block without audience" ^ Position.here pos));
            in item "block" [("audience", pred)] (check_container ctxt body) end
        | Prose {markdown} => (check_prose ctxt markdown body; item "prose" [] [body_xml body])
        | Verbatim => item "verbatim" [] [body_xml body]
        | Checked {check} => (check ctxt body; item "checked" (range_props body) [body_xml body])
        | Thms => item "thms" [] (check_thms ctxt name attrs body))
      end
and check_items ctxt = map (check_item ctxt)
and check_container ctxt body =
      (case read_container ctxt body of
        SOME items => check_items ctxt items
      | NONE => (check_prose ctxt true body; [text_xml body]));


(** the export read by the generator **)

fun config thy =
  let
    val axes = Axes.get thy;
    fun part (id, label) = XML.Elem (("part", [("id", id), ("label", label)]), []);
    fun link (label, url) = XML.Elem (("link", [("label", label), ("url", url)]), []);
    fun source session = XML.Elem (("source", [("session", session)]), []);
    fun axis (name, title, levels) =
      let
        val tags =
          (case AList.lookup (op =) (Axis_Tags.get thy) name of
            SOME ts => ts
          | NONE => map (K ("", "")) levels);
      in
        XML.Elem (("axis", [("name", name), ("title", title)]),
          map2 (fn l => fn (tag, hint) =>
            XML.Elem (("level", [("name", l), ("tag", tag), ("hint", hint)]), [])) levels tags)
      end;
    fun profile (name, levels) =
      XML.Elem (("profile", [("name", name)]),
        map2 (fn (a, _, _) => fn l => XML.Elem (("value", [("axis", a), ("level", string_of_int l)]), []))
          axes levels);
    fun command (name, (_, external)) =
      XML.Elem (("command", [("name", name), ("external", if external then "true" else "false")]), []);
  in
    XML.Elem (("config",
        [("title", Title.get thy), ("logo", Logo.get thy), ("icon", Icon.get thy),
         ("index", Index.get thy), ("repository", Repository.get thy), ("badge", if Badge.get thy then "true" else "false"),
         ("bib", Bib.get thy), ("cite_audience", Cite_Audience.get thy),
         ("cite_style", (case Cite_Style.get thy of "" => "number" | s => s)),
         ("local_links", if Local_Links.get thy then "true" else "false"),
         ("control_label", (case Control_Label.get thy of "" => "Reader profile" | s => s))]),
      map part (Parts.get thy) @ map link (Links.get thy) @ map source (Sources.get thy) @
      map axis axes @ map profile (Profiles.get thy) @
      map command (Symtab.dest (Commands.get thy)))
  end;

val _ =
  (Theory.setup o Thy_Info.add_presentation) (fn _ => fn thy =>
    Export.export thy (Path.binding0 (Path.explode "site/config.xml")) [config thy]);


(** top-level commands **)

fun keep_ctxt f = Toplevel.keep (fn st => f (Toplevel.presentation_context st));

fun export_item ctxt tree =
  let
    val pos = Position.thread_data ();
    val offset =
      (case Position.offset_of pos of
        SOME i => i
      | NONE => error ("Page item without a position" ^ Position.here pos));
  in
    Export.export (Proof_Context.theory_of ctxt)
      (Path.binding0 (Path.explode ("site/item/" ^ string_of_int offset ^ ".xml"))) [tree]
  end;

fun item_command keyword description kind parse_body hook =
 (Theory.setup (add_command false (#1 keyword) kind);
  Outer_Syntax.command keyword description
    (Parse.position (Scan.succeed (#1 keyword)) -- attrs -- parse_body
      >> (fn ((name, attrs), body) =>
        keep_ctxt (fn ctxt =>
         (hook ctxt attrs;
          export_item ctxt (check_item ctxt (Item (name, attrs, body))))))));

fun container_command keyword description =
  item_command keyword description Container Parse.document_source (K (K ()));

fun verbatim_command keyword description =
  item_command keyword description Verbatim (Parse.input Parse.cartouche) (K (K ()));

fun checked_command keyword description check =
  item_command keyword description (Checked {check = check}) (Parse.input Parse.cartouche) (K (K ()));

fun thms_command keyword description =
  item_command keyword description Thms (Parse.input Parse.cartouche) (K (K ()));

fun block_command keyword audience =
  item_command keyword
    ("prose for readers with " ^ (if audience = "" then "the given audience" else quote audience))
    (Block {audience = if audience = "" then NONE else SOME audience})
    Parse.document_source (K (K ()));

fun page_command keyword description =
  item_command keyword description (Prose {markdown = false}) Parse.document_source
    check_page_attrs;

end;
