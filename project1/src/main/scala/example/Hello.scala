package example

object ShapeExample extends App {

  trait Shape 
  case class Circle(radius: Double) extends Shape
  case class Rectangle(width: Double, height: Double) extends Shape
  case class Square(side: Double) extends Shape
  case class Triangle(a: Double, b: Double, c: Double) extends Shape

  // Shape => Double
  def perimeter(shape: Shape): Double = shape match {
    case Circle(r) => 2 * Math.PI * r
    case Rectangle(w, h) => 2 * (w + h)
    case Square(s) => 4 * s
    // case _ => throw new IllegalArgumentException("Unknown shape")
    // case Triangle(a, b, c) => a + b + c
    case _ => 0.0 // Fallback case to handle unknown shapes
  }

}

