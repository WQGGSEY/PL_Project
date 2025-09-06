package cs320

object Implementation extends Template {

  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = a*b*c;

  def concat(x: String, y: String): String = s"$x$y";

  def addN(n: Int): Int => Int = (x: Int) => n + x;

  def twice(f: Int => Int): Int => Int = (x: Int) => f(f(x));

  def compose(f: Int => Int, g: Int => Int): Int => Int = (x: Int) => f(g(x));

  def double(l: List[Int]): List[Int] = l.map(x => x * 2)

  def sum(l: List[Int]): Int = l.sum

  def getKey(m: Map[String, Int], s: String): Int = m.getOrElse(s, error(s));

  def countLeaves(t: Tree): Int = t match{
    case Leaf(v) => 1
    case Branch(left, root, right) => countLeaves(left) + countLeaves(right)
  }

  def flatten(t: Tree): List[Int] = t match{
    case Leaf(v) => List(v)
    case Branch(left, root, right) => flatten(left) ++ List(root) ++ flatten(right)
  }
}
