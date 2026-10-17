theory Locales
  imports Example_Site Site_Example_Dev.Monoids
begin

page (part: guide, order: 20, optional: 50) \<open>Locales\<close>

text \<open>
  A locale is a signature with assumptions: parameters that are fixed, and facts that are
  assumed about them. @{const Monoids.semigroup} fixes a binary operation and assumes it
  associative.
\<close>

source (kind: const) \<open>Monoids.semigroup\<close>

text \<open>
  Locales extend one another. @{const Monoids.monoid} adds a neutral element with its two
  laws, and inside it anything proved holds for every monoid:
\<close>

source (kind: const) \<open>Monoids.monoid\<close>

section \<open>Working inside a locale\<close>

text \<open>
  Within the locale's context, @{thm Monoids.monoid.neutral_unique} shows that the neutral
  element is the only one, and @{const Monoids.monoid.pow} iterates the operation:
\<close>

doc_thm (rule) \<open>Monoids.monoid.neutral_unique\<close>
doc_thm \<open>Monoids.monoid.pow_add\<close>

why \<open>
  The facts carry the locale's name: @{thm Monoids.monoid.pow_add} is stated for an
  arbitrary monoid, and its statement mentions the locale's parameters as variables.
\<close>

formal \<open>
  text \<open>
    The uniqueness proof is a calculation: \<open>e' = e' \<cdot> e\<close> by the right neutral law, and
    \<open>e' \<cdot> e = e\<close> by the assumption about \<open>e'\<close>.
  \<close>

  source \<open>Monoids.monoid.neutral_unique\<close>
\<close>

section \<open>Interpretation\<close>

text \<open>
  An interpretation shows that a concrete structure is a monoid, and every fact of the
  locale becomes a fact about it: @{thm Monoids.nat_add.pow_add} is
  @{thm Monoids.monoid.pow_add} for addition on the natural numbers.
\<close>

doc_thm \<open>Monoids.nat_add.pow_add\<close>

end
