package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map {
      case (k, v) => (k, Signal(eval(v(), namedExpressions)))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    evalSafe(expr, references, references.keySet)
  }

  def evalSafe(expr: Expr, references: Map[String, Signal[Expr]], vars: Set[String]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) => name match {
        case none if !vars.contains(none) => Double.NaN
        case _ => evalSafe(
          references.getOrElse(name, Signal(Literal(Double.NaN))).apply(),
          references,
          vars - name)
      }
      case Plus(a, b) => evalSafe(a, references, vars) + evalSafe(b, references, vars)
      case Minus(a, b) => evalSafe(a, references, vars) - evalSafe(b, references, vars)
      case Times(a, b) => evalSafe(a, references, vars) * evalSafe(b, references, vars)
      case Divide(a, b) => evalSafe(a, references, vars) / evalSafe(b, references, vars)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
