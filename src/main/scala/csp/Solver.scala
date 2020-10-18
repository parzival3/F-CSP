package csp

import scala.annotation.tailrec

/*
 * function BACKTRACKING-SEARCH(csp) returns a solution or failure
 *  return BACKTRACK(csp, { })
 *
 * function BACKTRACK(csp, assignment) returns a solution or failure
 *  if assignment is complete then return assignment
 *  var ← SELECT-UNASSIGNED-VARIABLE(csp, assignment)
 *  for each value in ORDER-DOMAIN-VALUES(csp, var , assignment) do
 *    if value is consistent with assignment then
 *    add {var = value} to assignment
 *    inferences ← INFERENCE(csp, var , assignment)
 *    if inferences 6= failure then
 *      add inferences to csp
 *      result ← BACKTRACK(csp, assignment)
 *      if result = failure then return result
 *        remove inferences from csp
 *    remove {var = value} from assignment
 *  return failure
 */

/*
 * Case class that represent a "State" of the Problem.
 * In our case a "State" of the problem is a node on the searching graph containing the current CSP to solve and
 * the list of already assigned variables
 */
case class Solution(csp: CSP, assignments: Assignments) extends Node {

  /**
    * A new solution is consistent if a new assignment is consistent.
    * @param newAssignment tuple of (Variable,Value)
    * @return Boolean
    */
  def isConsistent(newAssignment: (Variable, Int)): Boolean = {
    require(assignments.notAssigned(newAssignment._1))
    isComplete || csp.constraints.filter(c => c.relatesToVar(newAssignment._1)).forall { c =>
      c.isConsistent(c.neighbor, Assignments(assignments.mapVarValue ++ Map(newAssignment._1 -> newAssignment._2)))
    }
  }

  def isComplete: Boolean = assignments.isComplete(csp.variables)

  override def selectUnassignedVar(solution: Solution): Variable = {
    solution.assignments.getUnassignedVariable(solution.csp.variables)
  }

  override def orderDomainValues(solution: Solution, variable: Variable): LazyList[Int] = {
    require(solution.csp.varDomMap(variable).values.nonEmpty)
    solution.csp.varDomMap(variable).values.to(LazyList)
  }

  override def inference(solution: Solution, unassignedVar: Variable): Option[CSP] = {
    val listOfNeighborNotAssigned = (solution.csp.neighbors(unassignedVar).zip(Set(unassignedVar)))
    solution.MAC(csp, listOfNeighborNotAssigned)
  }

  override def MAC(csp: CSP, queue: List[(Variable, Variable)] = csp.combinationOfArcs): Option[CSP] = AC_3(csp, queue)

  override def children(solution: Solution): LazyList[Solution with Node] = {
    val newVar = selectUnassignedVar(solution)
    val values = orderDomainValues(solution, newVar).filter(value => solution.isConsistent(newVar -> value))
    values.flatMap { value =>
      val newAssignment = solution.assignments.addValue(newVar, value)
      inference(Solution(solution.csp, newAssignment), newVar) match {
        case None => LazyList.empty
        case Some(newCSP) => {
          LazyList(Solution(newCSP, newAssignment))
        }
      }
    }
  }

  override def backtrackingSearch(csp: CSP): LazyList[Solution with Node] = {
    val noUnaryCSP = csp.removeUnary()
    Solution(csp, Assignments()).MAC(noUnaryCSP) match {
      case Some(newCSP) => backtrack(Solution((newCSP), Assignments()))
      case None         => LazyList.empty
    }
  }

  override def backtrack(solution: Solution = this): LazyList[Solution with Node] = children(solution).flatMap {
    child => if (child.isComplete) LazyList(child) else child.backtrack()
  }
}

/*
 * Node that represent one node of the search tree in the current problem.
 * This node is not to confuse with the "node" used to define the constraint graph in the CSP itself
 *
 */
trait Node {

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
   */
  def revise(csp: CSP, Xi: Variable, Xj: Variable): Option[Domain] = {
    csp.getConstraints(List(Xi, Xj)) match {
      case List() => None
      case constraints =>
        val XiValues = csp.varDomMap(Xi).values
        val XjValues = csp.varDomMap(Xj).values
        val revised = Domain(XiValues.filter { x =>
          XjValues.foldLeft(false) { (p, n) =>
            p || constraints.forall(c => c.isSatisfied(Map(Xi -> x, Xj -> n)))
          }
        })
        Option.when(revised != csp.varDomMap(Xi))(revised)
    }
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

  def AC_3(csp: CSP, queue: List[(Variable, Variable)]): Option[CSP] = AC_3_imp(csp, queue)

  @tailrec
  private def AC_3_imp(csp: CSP, queue: List[(Variable, Variable)]): Option[CSP] = {
    queue match {
      case x :: xs =>
        val Xi = x._1
        val Xj = x._2
        revise(csp, Xi, Xj) match {
          case None => AC_3_imp(csp, xs)
          case Some(dom) =>
            dom.values match {
              case Nil => None
              case _ =>
                AC_3_imp(csp.restrictDomain(x._1 -> dom), xs ::: csp.reviseArcs(Xi, Xj))
            }
        }
      case _ => Some(csp)
    }
  }
  /*
   * The children of the current node are all the possible node with one less free variable
   *  var ← SELECT-UNASSIGNED-VARIABLE(csp, assignment)
   *  for each value in ORDER-DOMAIN-VALUES(csp, var , assignment) do
   *    if value is consistent with assignment then
   *    add {var = value} to assignment
   *    inferences ← INFERENCE(csp, var , assignment)
   *    if inferences 6= failure then
   *      add inferences to csp
   *      result ← BACKTRACK(csp, assignment)
   *
   */

  def isArcConsistent(csp: CSP): Boolean = AC_3(csp, csp.combinationOfArcs).isDefined

  def MAC(csp: CSP, queue: List[(Variable, Variable)]): Option[CSP] = AC_3(csp, queue)

  def children(solution: Solution): LazyList[Solution with Node]

  /**
    * function BACKTRACKING-SEARCH(csp) returns a solution or failure
    *  return BACKTRACK(csp, { })
    *  @param
    *  @return
    */
  def backtrackingSearch(csp: CSP): LazyList[Solution with Node]

  /**
    * @param solution
    * function BACKTRACK(csp, assignment) returns a solution or failure
    *  if assignment is complete then return assignment
    */
  def backtrack(solution: Solution): LazyList[Solution with Node]

  /**
    * Select the next variable to try for our graph
    * Currently it only returns the first available variable, probalby a better solution is to return a LazyList of
    * variables
    * @param solution the current solution in which there is a free variable
    * @return Variable
    */
  def selectUnassignedVar(solution: Solution): Variable

  /**
    * Select the order in which the values in the domain are selected
    * For us is not very important, the problem here is that we need a Lazy List in order to use
    * the functional DFS
    * @param solution the current solution
    * @param variable the current unassigned variable
    * @return LazyList of integer values congruent with the current variable domain
    */
  def orderDomainValues(solution: Solution, variable: Variable): LazyList[Int]

  def inference(solution: Solution, unassignedVar: Variable): Option[CSP]
}
