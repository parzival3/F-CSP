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
    csp.constraints.filter(c => c.relatesToVar(newAssignment._1)).forall(c => c.isConsistent(c.neighbor, Assignments(assignments.mapVarValue ++ Map(newAssignment._1 -> newAssignment._2))))
  }

  def isComplete: Boolean = assignments.isComplete(csp.variables)

  private def combinator2 =  csp.neighbors.flatMap(x => (x._2.keySet.zip(Set(x._1))).map(_.swap)).toList
  private def combinator3: List[(Variable, Variable)] = csp.constraints.flatMap(c => List((c.neighbor.head, c.neighbor(1)), (c.neighbor(1), c.neighbor.head))).distinct

  def isArcConsistent: Boolean = AC_3(csp, combinator2).isDefined
  def MAC(queue: List[(Variable, Variable)] = combinator3): Option[CSP] = {
    val solution = AC_3(csp, queue)
    solution
  }
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
        val XiValues = csp.domainMap(Xi).values
        val XjValues = csp.domainMap(Xj).values
        val revised = Domain(XiValues.filter{ x => XjValues.foldLeft(false) { (p, n) =>
          p || constraints.forall(c => c.isSatisfied(Map(Xi -> x, Xj -> n)))}}
      )
     Option.when(revised != csp.domainMap(Xi))(revised)
    }
  }
//    csp.neighbors(Xi).get(Xj) match {
//      case None => None
//      case Some(neighbor) =>  {
//        val constraints = neighbor.toList
//        def applyToAllConstraint(valuei: Int, valuej: Int): Boolean = {
//          constraints.forall(_.fun(valuei, valuej))
//        }
//        val newDom = Domain(csp.domainMap(Xi).values.filter(x => csp.domainMap(Xj).values.foldLeft(false)(_ || applyToAllConstraint(x, _))))
//        Option.when(newDom != csp.domainMap(Xi))(newDom)
//      }
//    }
//  }

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
            AC_3(csp.restrictDomain(x._1 -> dom), xs ::: csp.reviseArcs(Xi, Xj))
        }
      }
      case _ => Some(csp)
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
  def children(solution: Solution): LazyList[Solution with Node] = {
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

  /*
  * function BACKTRACKING-SEARCH(csp) returns a solution or failure
    *  return BACKTRACK(csp, { })
  */
  def backtrackingSearch(csp: CSP):LazyList[Solution with Node] = {
    Solution(csp, Assignments()).MAC() match {
      case Some(newCSP) => backtrack(Solution((newCSP), Assignments()))
      case None => LazyList.empty
    }
  }
  /*
  * function BACKTRACK(csp, assignment) returns a solution or failure
    *  if assignment is complete then return assignment
   */
  def backtrack(solution: Solution): LazyList[Solution with Node] = children(solution).flatMap {
    child => if (child.isComplete) LazyList(child) else child.backtrack(child)
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
  def orderDomainValues(solution: Solution, variable: Variable): LazyList[Int] = {
    require(solution.csp.domainMap(variable).values.nonEmpty)
    solution.csp.domainMap(variable).values.to(LazyList)
  }

  def inference(solution: Solution, unassignedVar: Variable): Option[CSP] = {
    val listOfNeighborNotAssigned = (solution.csp.neighbors(unassignedVar) -- solution.assignments.mapVarValue.keys).keys.zip(Set(unassignedVar)).toList
    solution.MAC(listOfNeighborNotAssigned)
  }
}
