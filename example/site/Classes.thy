theory Classes
  imports Example_Site Site_Example_Dev.Pointed
begin

page (part: guide, order: 30, optional: 50) \<open>Type classes\<close>

text \<open>
  A type class names an operation that every type in the class provides. @{const Pointed.point}
  is such an operation: a distinguished value of the type.
\<close>

source (kind: const) \<open>Pointed.point\<close>

text \<open>
  A type joins the class by an instantiation that defines the operation for it. The natural
  numbers pick zero; lists pick the empty list, whatever their elements:
\<close>

doc_thm \<open>Pointed.point_nat_def Pointed.point_list_def\<close>

section \<open>Generic functions\<close>

text \<open>
  A function whose type mentions the class works for every instance. @{const Pointed.defaults}
  builds a list of default values of any pointed type:
\<close>

source \<open>Pointed.defaults\<close>

doc_thm \<open>Pointed.length_defaults\<close>

why \<open>
  The class constraint \<^verbatim>\<open>'a::pointed\<close> in the type of @{const Pointed.defaults} is what
  makes @{const Pointed.point} available inside it: the function is defined once and works
  at every instance, as @{thm Pointed.defaults_nat} shows for the natural numbers.
\<close>

formal \<open>
  text \<open>
    Instantiating the type variable picks the instance's definition of the operation, as
    @{thm Pointed.defaults_nat} shows:
  \<close>

  doc_thm \<open>Pointed.defaults_nat\<close>
\<close>

html \<open><p><small>Every fact on this page is cited from <a href="src/Examples/Site_Example_Dev/Pointed.html">the theory</a> it is proved in.</small></p>\<close>

end
