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
  def areAssigned(variables: List[Variable]): Boolean = {
    variables.forall(mapVarValue.contains)
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
    val res = assignments.mapVarValue.forall(csp.isAssignmentConsistent(newAssignment, _))
    res
  }

  def isComplete: Boolean = assignments.isComplete(csp.variables)

  private def combinator2 =  csp.neighbors.flatMap(x => (x._2.keySet.zip(Set(x._1))).map(_.swap)).toList

  def isArcConsistent: Boolean = AC_3(csp, combinator2).isDefined
  def MAC(queue: List[(Variable, Variable)] = combinator2): Option[CSP] = AC_3(csp, queue)
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

    csp.neighbors(Xi).get(Xj) match {
      case None => None
      case Some(neighbor) =>  {
        val constraints = neighbor.toList
        def applyToAllConstraint(valuei: Int, valuej: Int): Boolean = {
          constraints.forall(_.fun(valuei, valuej))
        }
        val newDom = Domain(csp.domainMap(Xi).values.filter(x => csp.domainMap(Xj).values.foldLeft(false)(_ || applyToAllConstraint(x, _))))
        Option.when(newDom != csp.domainMap(Xi))(newDom)
      }
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

  @tailrec
  private def AC_3(csp: CSP, queue: List[(Variable, Variable)]): Option[CSP] = {
    queue match {
      case x :: xs =>
        val Xi = x._1
        val Xj = x._2
        revise(csp, Xi, Xj) match {
        case None => AC_3(csp, xs)
        case Some(dom) => dom.values match {
          case Nil => None
          case _ =>
            val arcsToReview = csp.neighbors(Xi).keySet.-(Xj).zip(Set(Xi)).toList
            AC_3(csp.restrictDomain(x._1 -> dom), xs ::: arcsToReview)
        }
      }
      case Nil => Some(csp)
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
        (solution.isConsistent(newAssignment = unassignedVar -> value)) match {
          case true => {
            val newAssignment = solution.assignments.addValue(unassignedVar, value)
            inference(Solution(solution.csp, newAssignment), unassignedVar) match {
              case None => None
              case Some(newCSP) => {
                println(newCSP.domainMap)
                Some(Solution(newCSP, newAssignment))
              }
            }
          }
          case false => None
        }
      }.to(LazyList)
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
  def backtrack(solution: Solution): LazyList[Option[Solution with Node]] = children(solution).flatMap {
    child => child match {
      case None => LazyList(None)
      case Some(x) =>  if (x.isComplete) LazyList(child) else x.backtrack(x)
    }
  }

  /*
   * Select the next variable to try for our graph
   * Currently it only returns the first available variable, probalby a better solution is to return a LazyList of
   * variables
   * @param solution the current solution in which there is a free variable
   * @return Variable
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
  def orderDomainValues(solution: Solution, variable: Variable): List[Int] = {
    require(solution.csp.domainMap(variable).values.nonEmpty)
    solution.csp.domainMap(variable).values
  }

  def inference(solution: Solution, unassignedVar: Variable): Option[CSP] = {
    // require(solution.assignments.notAssigned(unassignedVar))
    val listOfNeighborNotAssigned = (solution.csp.neighbors(unassignedVar) -- solution.assignments.mapVarValue.keys).keys.zip(Set(unassignedVar)).toList
    solution.MAC(listOfNeighborNotAssigned)
  }
}
