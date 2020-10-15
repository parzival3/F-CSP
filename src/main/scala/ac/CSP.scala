package ac

import scala.annotation.tailrec

case class Domain(values: List[Int])
case class Variable(name: String)

class CSP(val variables: List[Variable], val domainMap: Map[Variable, Domain]) {

  var queue: List[(Variable, Variable)] = combinator(variables)

  def constraint(x: Int, y: Int): Boolean = y > x

  /*
   * Helper function to get all the variables in a list before the current variable.
   * TODO: This function has to be removed when we switch to constraint as way to specify connection
   *       between two edges
   */
  private def slicing(list: List[Variable], cVar: Variable): List[Variable] = {
    list.slice(0, list.indexOf(cVar))
  }


  /*
   * Combinator just give us a list of all the combination between a list of variables
   * TODO: this is just to test the ARC consistency, this has to be removed
   */
  private def combinator(list: List[Variable]): List[(Variable, Variable)]  = {
    for {
      v1 <- list
      v2 <- list
      if v1 != v2
    } yield (v1, v2)
  }

  def isArcConsistent: Boolean = AC_3(queue, domainMap)

  /*
   * function REVISE(csp,Xi, Xj) returns true iff we revise the domain of Xi
   *  revised ← false
   *  for each x in Di do
   *   if no value y in Dj allows (x,y) to satisfy the constraint between Xi and Xj then
   *     delete x from Di
   *     revised ← true
   *  return revised
   *
   * @return (Boolean, Domain) return a tuple containing true if there was a revision of the domain and the new domain
   * @param vars a (Variable, Variable) tuple take from the head of the queue
   * @param domList  a Map (Variable -> Domain)
   * TODO:  currently the function take as parameters a tuple of variable and the DomainMap.
   *        we are missing the map of neighbors or node that link a Variable Xi to another one Xj.
   *        This can be a structure Neighbors which is a map between Variable -> (Constraint, Variable)
   *        Or Xi -> (Cn, Xj)
   */
  def revise(vars: (Variable, Variable), domList: Map[Variable, Domain]): (Boolean, Domain) = {
    val newDom = Domain(domList(vars._1).values.filter(x => domList(vars._2).values.foldLeft(false)(_ || constraint(x, _))))
    (newDom != domList(vars._1), newDom)
  }

  /*
    * function AC-3(csp) returns false if an inconsistency is found and true otherwise
    *   queue ← a queue of arcs, initially all the arcs in csp
    *   while queue is not empty do
    *     (Xi, Xj) ← POP(queue)
    *     if REVISE(csp,Xi, Xj) then
    *       if size of Di == 0 then
    *         return false
    *       for each Xk in Xi.NEIGHBORS - {Xj} do
    *         add (Xk, Xi) to queue
    *   return true
    *
    * This is an algorithm to enforce arc consistency between the node of a CSP, it differs from the code use in the
    * book since it uses recursion to find if the current CSP is arc consistent or not
    * @return Boolean if the current CSP problem is arc consistent
    * @param queue List of tuples (Variable, Variable) this is basically a list of neighbors node or Variables that
    *              are connected by a constraint
    * @param domList This is a map between a Variable and its domain
   */

  @tailrec
  private def AC_3(queue: List[(Variable, Variable)], domList: Map[Variable, Domain]): Boolean = {
    queue match {
      case x :: xs => revise(x, domList) match {
        case (false, _ ) => AC_3(xs, domList)
        case (true, dom) => dom.values match {
          case Nil => false
          case _ =>
            val newDom = domList - x._1 + (x._1 -> dom)
            AC_3(xs:::combinator(slicing(variables, x._1)), newDom)
        }
      }
      case Nil => true
    }
  }
}


object ConsistencyApp extends App {
  var listOfDomains = Map(Variable("a") -> Domain(0 to 10 toList), Variable("b") -> Domain(0 to 10 toList), Variable("c") -> Domain(0 to 10 toList))
  val listOfVariables = List(Variable("a"), Variable("b"), Variable("c"))
  val cons = new CSP(listOfVariables, listOfDomains)
  print(cons.isArcConsistent)
}
