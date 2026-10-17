theory Example_Site
  imports Site.Site
  keywords "why" "formal" "hol" :: document_body
begin

text \<open>
  The example site: a minimal Isabelle documentation, three pages about the three theories
  of session Site_Example_Dev. Two reader axes, two named profiles, two block commands.
\<close>

setup \<open>
  Site.title "html-site" #>
  Site.parts [("guide", "Guide")] #>
  Site.links [("Isabelle", "https://isabelle.in.tum.de")] #>
  Site.sources ["Site_Example_Dev"] #>
  Site.axes
   [("isabelle", "Isabelle", ["new", "used"]),
    ("proofs", "Proofs", ["hidden", "shown"])] #>
  Site.profiles
   [("newcomer", ["new", "hidden"]),
    ("expert", ["used", "shown"])]
\<close>

ML \<open>
  Site.block_command \<^command_keyword>\<open>why\<close> "isabelle=new";
  Site.block_command \<^command_keyword>\<open>formal\<close> "proofs=shown";
  Site.checked_command \<^command_keyword>\<open>hol\<close> "a HOL term, read in the page's context"
    (fn ctxt => fn src => ignore (Syntax.read_term ctxt (Syntax.implode_input src)));
\<close>

end
