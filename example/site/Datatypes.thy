theory Datatypes
  imports Example_Site Site_Example_Dev.Tree
begin

page (part: guide, order: 10, optional: 60) \<open>Datatypes\<close>

text \<open>
  A datatype is defined by its constructors. @{type Tree.tree} has two: @{const Tree.Leaf},
  the empty tree, and @{const Tree.Node}, which joins two trees with a value between them.
\<close>

source (kind: type) \<open>Tree.tree\<close>

why \<open>
  The declaration says everything there is to know about trees: every tree is built from
  these two constructors, and different constructions are different trees. Isabelle derives
  the induction principle, the case analysis and the injectivity of the constructors from
  the declaration alone.
\<close>

section \<open>Functions by recursion\<close>

text \<open>
  @{const Tree.mirror} swaps the subtrees at every node, and @{const Tree.inorder} lists the
  values from left to right. Both are defined by one equation per constructor.
\<close>

source \<open>Tree.mirror\<close>
source \<open>Tree.inorder\<close>

text \<open>A term the page checks itself: the constants link to their definitions, and hovering a
  subterm shows its type.\<close>

hol \<open>inorder (mirror (Node Leaf (1::nat) Leaf))\<close>

why \<open>
  Recursion that follows the constructors is called \<^emph>\<open>primitive recursion\<close>: each call is on
  a subtree of the argument, so the function terminates, and Isabelle proves that itself.
\<close>

section \<open>Facts\<close>

text \<open>Mirroring twice gives the tree back, and mirroring reverses the inorder listing:\<close>

doc_thm \<open>Tree.mirror_mirror Tree.inorder_mirror\<close>

formal \<open>
  text \<open>
    Both proofs are by induction on the tree, with @{thm Tree.mirror_mirror} as the shape of
    every such proof: the induction rule of the datatype gives one case per constructor, and
    \<^verbatim>\<open>auto\<close> closes each with the defining equations.
  \<close>

  doc_thm (rule) \<open>Tree.tree.induct\<close>
\<close>

div (columns) \<open>
  pre (code) \<open>mirror (Node (Node Leaf 1 Leaf) 2 Leaf)
  = Node Leaf 2 (Node Leaf 1 Leaf)\<close>
  \<open>Evaluating by the equations: the outer node's subtrees are swapped, then the inner one's,
  which are both empty.\<close>
\<close>

end
