/*  Title:      site/src/profile.scala

Reader profiles over the site's axes: audience predicates, rendering states and
the profile matrix. The axes and the named profiles come from the site
configuration, so this is generic over sites.
*/

package site

import isabelle._


object Profile {
  /* a profile assigns a level to every axis */

  sealed case class Profile(axes: List[Config.Axis], values: Map[String, Int]) {
    def apply(a: Config.Axis): Int = values(a.name)
    def query: String = axes.map(a => a.name + "=" + apply(a)).mkString("&")    // URL form
    def slug: String = axes.map(a => a.name + apply(a)).mkString("-")           // path form
    def tags: List[String] = axes.map(a => a.tags(apply(a))).filter(_.nonEmpty)
  }


  /* predicates: atoms "axis op level", space = and, "|" = or */

  sealed abstract class Op(val name: String) {
    def apply(a: Int, b: Int): Boolean
    override def toString: String = name
  }
  case object Ge extends Op(">=") { def apply(a: Int, b: Int): Boolean = a >= b }
  case object Le extends Op("<=") { def apply(a: Int, b: Int): Boolean = a <= b }
  case object Ne extends Op("!=") { def apply(a: Int, b: Int): Boolean = a != b }
  case object Eq extends Op("=") { def apply(a: Int, b: Int): Boolean = a == b }
  val ops: List[Op] = List(Ge, Le, Ne, Eq)     // longest first, cf. thy/site.ML

  sealed abstract class Pred { def apply(p: Profile): Boolean }
  case class Atom(axis: Config.Axis, op: Op, level: Int) extends Pred {
    def apply(p: Profile): Boolean = op(p(axis), level)
    override def toString: String = axis.name + op + level
  }
  case class And(ps: List[Pred]) extends Pred {
    def apply(p: Profile): Boolean = ps.forall(_(p))
  }
  case class Or(ps: List[Pred]) extends Pred {
    def apply(p: Profile): Boolean = ps.exists(_(p))
  }

  /* disjunctive normal form with numeric levels: what pages carry and the generator's site.js evaluates */
  def dnf(p: Pred): List[List[Atom]] =
    p match {
      case a: Atom => List(List(a))
      case And(ps) =>
        ps.map(dnf).foldLeft(List(List.empty[Atom]))((acc, d) => for (x <- acc; y <- d) yield x ::: y)
      case Or(ps) => ps.flatMap(dnf)
    }

  def print(p: Pred): String = dnf(p).map(_.mkString(" ")).mkString(" | ")


  /* rendering states: no profile (nothing open), a profile, or everything open (review) */

  sealed abstract class State { def name: String; def open(pred: Pred): Boolean }
  case object Closed extends State { val name = "none"; def open(pred: Pred): Boolean = false }
  case class Of(name: String, profile: Profile) extends State {
    def open(pred: Pred): Boolean = pred(profile)
  }
  case object Open extends State { val name = "all"; def open(pred: Pred): Boolean = true }
}


/* parsing and the matrix, against the axes of one site */

class Profiles(config: Config.Config) {
  def axes: List[Config.Axis] = config.axes

  def profile(levels: List[Int]): Profile.Profile =
    Profile.Profile(axes, axes.map(_.name).zip(levels).toMap)

  /* a pre-set name, or "fp=1,proof=none,systems=2,goal=learn" (all axes, any order) */
  def parse_profile(s: String): Profile.Profile =
    named.find(_._1 == s.trim).map(_._2) getOrElse parse_values(s)

  private def parse_values(s: String): Profile.Profile = {
    val values =
      for (item <- s.split("[,&]").toList.map(_.trim).filter(_.nonEmpty)) yield
        item.split("=", 2) match {
          case Array(a, v) => val ax = config.axis(a.trim); ax.name -> ax.level(v.trim)
          case _ => error("Bad profile item " + quote(item) + ": expected axis=level")
        }
    val missing = axes.map(_.name).filterNot(values.map(_._1).contains)
    if (missing.nonEmpty) error("Profile " + quote(s) + " lacks " + commas(missing))
    Profile.Profile(axes, values.toMap)
  }

  val named: List[(String, Profile.Profile)] =
    config.profiles.map({ case (name, levels) => name -> profile(levels) })

  private val atom =
    ("^([A-Za-z0-9_]+)(" + Profile.ops.map(op => java.util.regex.Pattern.quote(op.name)).mkString("|") +
      ")([A-Za-z0-9_]+)$").r

  def parse_pred(s: String): Profile.Pred = {
    def conj(t: String): Profile.Pred =
      t.trim.split("\\s+").toList.filter(_.nonEmpty) match {
        case Nil => error("Empty audience predicate in " + quote(s))
        case atoms =>
          Profile.And(atoms.map({
            case atom(a, op, v) =>
              val ax = config.axis(a)
              Profile.Atom(ax, Profile.ops.find(_.name == op).get, ax.level(v))   // the regex is built from ops
            case bad => error("Bad audience atom " + quote(bad) + ": expected axis op level" +
              " with op one of " + Profile.ops.mkString(" "))
          }))
      }
    s.split("\\|", -1).toList match {
      case List(t) => conj(t)
      case ts => Profile.Or(ts.map(conj))
    }
  }

  /* the pre-rendered readings: the core, and everything. Named profiles are a query string
     for readers with JavaScript, and a "-p" argument when someone wants one on disk. */
  def matrix(extra: List[Profile.Profile]): List[Profile.State] =
    Profile.Closed :: Profile.Open ::
      extra.distinct.map(p => Profile.Of(named.find(_._2 == p).map(_._1).getOrElse(p.slug), p))
}
