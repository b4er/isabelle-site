theory Monoids
  imports Main
begin

text \<open>Locales: a signature with assumptions, extended, used, and interpreted.\<close>

locale semigroup =
  fixes f :: "'a \<Rightarrow> 'a \<Rightarrow> 'a"  (infixl "\<cdot>" 70)
  assumes assoc: "(x \<cdot> y) \<cdot> z = x \<cdot> (y \<cdot> z)"

locale monoid = semigroup +
  fixes e :: 'a
  assumes left_neutral: "e \<cdot> x = x"
    and right_neutral: "x \<cdot> e = x"
begin

lemma neutral_unique:
  assumes "\<And>x. e' \<cdot> x = x"
  shows "e' = e"
proof -
  have "e' = e' \<cdot> e" by (simp add: right_neutral)
  also have "\<dots> = e" by (rule assms)
  finally show ?thesis .
qed

fun pow :: "'a \<Rightarrow> nat \<Rightarrow> 'a" where
  "pow x 0 = e"
| "pow x (Suc n) = x \<cdot> pow x n"

lemma pow_add: "pow x (m + n) = pow x m \<cdot> pow x n"
  by (induction m) (simp_all add: left_neutral assoc)

end

interpretation nat_add: monoid "(+)" "0 :: nat"
  by unfold_locales simp_all

end
