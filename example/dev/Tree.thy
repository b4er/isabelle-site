theory Tree
  imports Main
begin

text \<open>A binary tree with a value at every node, and two functions over it.\<close>

datatype 'a tree = Leaf | Node "'a tree" 'a "'a tree"

fun mirror :: "'a tree \<Rightarrow> 'a tree" where
  "mirror Leaf = Leaf"
| "mirror (Node l x r) = Node (mirror r) x (mirror l)"

fun inorder :: "'a tree \<Rightarrow> 'a list" where
  "inorder Leaf = []"
| "inorder (Node l x r) = inorder l @ [x] @ inorder r"

lemma mirror_mirror: "mirror (mirror t) = t"
  by (induction t) auto

lemma inorder_mirror: "inorder (mirror t) = rev (inorder t)"
  by (induction t) auto

end
