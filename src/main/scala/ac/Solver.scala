package ac

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


case class Assignments(mapVarValue: Map[Variable, Int] = Map[Variable, Int]()) {

  def addValue(unassignedVar: Variable, value: Int): Assignments = {
    Assignments(mapVarValue + (unassignedVar -> value))
  }

  def getUnassignedVariable(variables: List[Variable]): Variable = {
    require(notComplete(variables))
    variables.filter(notAssigned).head
  }

  def assigned(variable: Variable): Boolean = {
    mapVarValue.contains(variable)
  }
  def notAssigned(variable: Variable): Boolean = !assigned(variable)

  def isComplete(variableList: List[Variable]): Boolean = mapVarValue.size == variableList.size
  def notComplete(variableList: List[Variable]): Boolean = !isComplete(variableList)
}

/*
 * Case class that represent a "State" of the Problem.
 * In our case a "State" of the problem is a node on the searching graph containing the current CSP to solve and
 * the list of already assigned variables
 */
case class Solution(csp: CSP, assignments: Assignments) extends Node {
  /*
   * TODO: This doesn't really make sens but that's the best I come up with for now
   *       definitely to refactor inside assignment for now the information is
   *       distributed across multiple classes I should rethink on where to store all the
   *       data, List[Variable], mapOfConstraint, Domains, Assignments etc...
   *
   * A new solution is consistent if a new assignment is consistent.
   * @param newAssignment tuple of (Variable,Value)
   * @return Boolean
   */
  def isConsistent(newAssignment: (Variable, Int)):Boolean = {
    require(assignments.notAssigned(newAssignment._1))
    assignments.mapVarValue.forall(csp.isAssignmentConsistent(newAssignment, _))
  }

  def isComplete: Boolean = assignments.isComplete(csp.variables)

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
  private def combinator(list: List[Variable]): List[(Variable, Variable)] = {
    for {
      v1 <- list
      v2 <- list
      if v1 != v2
    } yield (v1, v2)
  }

  def isArcConsistent: Boolean = AC_3(csp, combinator(csp.variables))

  def constraint(x: Int, y: Int): Boolean = y > x
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
  private def AC_3(csp: CSP, queue: List[(Variable, Variable)]): Boolean = {
    queue match {
      case x :: xs => revise(x, csp.domainMap) match {
        case (false, _) => AC_3(csp, xs)
        case (true, dom) => dom.values match {
          case Nil => false
          case _ =>
            val newDomain = csp.domainMap - x._1 + (x._1 -> dom)
            AC_3(csp.restrictDomain(newDomain), xs ::: combinator(slicing(csp.variables, x._1)))
        }
      }
      case Nil => true
    }
  }
}

/*
 * Node that represent one node of the search tree in the current problem.
 * This node is not to confuse with the "node" used to define the constraint graph in the CSP itself
 *
 */
trait Node {

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
  def children(solution: Solution): LazyList[Option[Solution with Node]] = {
      // TODO: Should we try all the unassigned variables? maybe with a lazy list? and do a flatMap?
      val unassignedVar = selectUnassignedVar(solution)
      orderDomainValues(solution, unassignedVar).map { value =>
        solution.isConsistent(newAssignment = unassignedVar -> value)
        val newAssignment = solution.assignments.addValue(unassignedVar, value)
        inference(solution.csp, unassignedVar, newAssignment) match {
          case None => None
          case Some(x) =>Some(Solution(solution.csp.restrictDomain(x), newAssignment))
        }
      }
  }

  /*
  * function BACKTRACKING-SEARCH(csp) returns a solution or failure
    *  return BACKTRACK(csp, { })
  */
  def backtrackingSearch(csp: CSP):LazyList[Option[Solution with Node]] = backtrack(Solution(csp, Assignments()))
  /*
  * function BACKTRACK(csp, assignment) returns a solution or failure
    *  if assignment is complete then return assignment
   */
  private def backtrack(solution: Solution): LazyList[Option[Solution with Node]] = children(solution).flatMap {
    child => child match {
      case None => LazyList(None)
      case Some(x) =>  if (x.isComplete) LazyList(child) else x.backtrack(solution)
    }
  }

  /*
   * Select the next variable to try for our graph
   * Currently it only returns the first available variable, probalby a better solution is to return a LazyList of
   * variables
   * @param solution the current solution in which there is a free variable
   */
  def selectUnassignedVar(solution: Solution): Variable = {
    solution.assignments.getUnassignedVariable(solution.csp.variables)
  }

  /*
   * Select the order in which the values in the domain are selected
   * For us is not very important, the problem here is that we need a Lazy List in order to use
   * the functional DFS
   * @param solution the current solution
   * @param variable the current unassigned variable
   * @return LazyList of integer values congruent with the current variable domain
   */
  def orderDomainValues(solution: Solution, variable: Variable): LazyList[Int] = {
    require(solution.csp.domainMap(variable).values.nonEmpty)
    solution.csp.domainMap(variable).values.to(LazyList)
  }

  /*
   * TODO: complete the method
   */
  def inference(csp: CSP, unassignedVar: Variable, newAssignment: Assignments): Option[Map[Variable, Domain]] = {
    require(newAssignment.notAssigned(unassignedVar))
    Some(csp.domainMap)
  }
}
