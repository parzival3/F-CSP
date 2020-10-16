package ac

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

  def inference(csp: CSP, unassignedVar: Variable, newAssignment: Assignments): Option[Domain] = {
    None
  }
}
