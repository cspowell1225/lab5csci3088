package jsy.student

import jsy.lab2.Lab2Like
import jsy.student.Lab2.toBoolean

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * Anna Malawista
   * 
   * Partner: Christopher Powell
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */



  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(true) => 1
      case B(false) => 0
      case Undefined => Double.NaN
      case _ => ???
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case _ => ???
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case _ => ???
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */

      case Var(x) => lookup(env,x)

      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined

      case If(bool, e1, e2) => bool match{
        case B(true) => eval(e1)
        case B(false) => eval(e2)
      }

      case ConstDecl(x, e1, e2) => {
        val env2 = extend(env, x, eval(env, e1))
        eval(env2, e2)
      }

      case Unary(uop, e1) => uop match{
        case Neg => N(-1 * toNumber(e1))
        case Not => e1 match{
          case B(true) => B(false)
          case B(false) => B(true)
        }
      }

      case Binary(bop, e1, e2) => bop match {
        case And => {
          e1 match {
            case B(b) => {
              e2 match {
                case B(b) => {
                  if (toBoolean(eval(env, e1))) eval(env, e2)
                  else eval(env, e1)
                }
                case _ => eval(env, e2)
              }
            }
            case _ => eval(env, e1)
          }
        }
        case Or => {
          e1 match {
            case B(b) => {
              e2 match {
                case B(b) => {
                  if (toBoolean(eval(env, e1))) eval(env, e1)
                  else eval(env, e2)
                }
                case _ => eval(env, e2)
              }
            }
            case _ => eval(env, e1)
          }
        }
        case Plus => N(toNumber(eval(env, e1)) + toNumber(eval(env, e2)))
        case Minus => N(toNumber(eval(env, e1)) - toNumber(eval(env, e2)))
        case Times => N(toNumber(eval(env, e1)) * toNumber(eval(env, e2)))
        case Div => N(toNumber(eval(env, e1)) / toNumber(eval(env, e2)))
        case Eq => {
          if (toNumber(eval(env, e1)) == toNumber(eval(env, e2))) B(true)
          else B(false)
        }
        case Ne => {
          if (toNumber(eval(env, e1)) != toNumber(eval(env, e2))) B(true)
          else B(false)
        }
        case Lt => {
          if (toNumber(eval(env, e1)) < toNumber(eval(env, e2))) B(true)
          else B(false)
        }
        case Le => {
          if (toNumber(eval(env, e1)) <= toNumber(eval(env, e2))) B(true)
          else B(false)
        }
        case Gt => {
          if (toNumber(eval(env, e1)) > toNumber(eval(env, e2))) B(true)
          else B(false)
        }
        case Ge => {
          if (toNumber(eval(env, e1)) >= toNumber(eval(env, e2))) B(true)
          else B(false)
        }
        case Seq => {
          eval(env, e1)
          eval(env, e2)
        }
      }

      case _ => e
    }
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

     println(pretty(v))
  }

}
