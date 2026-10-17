theory Site
  imports Pure
  keywords
    "page" :: document_heading
    and "div" "block" "pre" "html" :: document_body
    and "doc_thm" "source" :: thy_decl
begin

text \<open>
  Base theory for websites generated from Isabelle theories. A site imports it
  and declares its reader axes, profiles and extra commands. Pages import the
  site's theory.
\<close>

ML_file \<open>site.ML\<close>

ML \<open>
  Site.page_command \<^command_keyword>\<open>page\<close> "page title (exactly one per page)";
  Site.container_command \<^command_keyword>\<open>div\<close> "HTML element with nested items";
  Site.block_command \<^command_keyword>\<open>block\<close> "";
  Site.verbatim_command \<^command_keyword>\<open>pre\<close> "preformatted text";
  Site.verbatim_command \<^command_keyword>\<open>html\<close> "raw HTML";
  Site.verbatim_command \<^command_keyword>\<open>source\<close> "the defining command of a fact, constant or type";
  Site.thms_command \<^command_keyword>\<open>doc_thm\<close> "facts as printed";
\<close>

setup \<open>
  Site.declare "text" (Site.Prose {markdown = true}) #>
  Site.declare "section" (Site.Prose {markdown = false}) #>
  Site.declare "subsection" (Site.Prose {markdown = false})
\<close>

end
