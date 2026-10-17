theory Pointed
  imports Main
begin

text \<open>Type classes: an operation every instance provides, and a function generic in it.\<close>

class pointed =
  fixes point :: 'a

instantiation nat :: pointed
begin
definition "point = (0 :: nat)"
instance ..
end

instantiation list :: (type) pointed
begin
definition "point = ([] :: 'a list)"
instance ..
end

definition defaults :: "nat \<Rightarrow> 'a::pointed list" where
  "defaults n = replicate n point"

lemma length_defaults [simp]: "length (defaults n) = n"
  by (simp add: defaults_def)

lemma defaults_nat: "defaults 2 = [0 :: nat, 0]"
  by (simp add: defaults_def point_nat_def numeral_2_eq_2)

end
