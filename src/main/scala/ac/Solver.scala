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



case class Assignment(mapVarValue: Map[Variable, Int] = Map[Variable, Int]()) {

  def addValue(unassignedVar: Variable, value: Int): Assignment = {
    Assignment(mapVarValue + (unassignedVar -> value))
  }

  def assigned(variable: Variable): Boolean = {
    mapVarValue.contains(variable)
  }
  def notAssigned(variable: Variable): Boolean = !assigned(variable)

  def isConsistent(value: Int, curVar: Variable): Boolean =  {
    require(notAssigned(curVar))

  }

  def isComplete: Boolean = true
}

/*
 * Case class that represent a "State" of the Problem.
 */
case class Solution(csp: CSP, assignment: Assignment) extends Node

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
      val unassignedVar = selectUnassignedVar(solution.csp, solution.assignment)
      orderDomainValues(solution.csp, unassignedVar, solution.assignment).map { value =>
        solution.assignment.isConsistent(value)
        val newAssignment = solution.assignment.addValue(unassignedVar, value)
        inference(solution.csp, unassignedVar, newAssignment) match {
          case None => None
          case Some(x) =>Some(Solution(solution.csp.restrictDomain(x), newAssignment))
        }
      }
  }

  def isComplete: Boolean = true

  /*
  * function BACKTRACKING-SEARCH(csp) returns a solution or failure
    *  return BACKTRACK(csp, { })
  */
  def backtrackingSearch(csp: CSP):LazyList[Option[Solution with Node]] = backtrack(Solution(csp, Assignment()))
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


  def selectUnassignedVar(csp: CSP, assignment: Assignment): Variable = {
    Variable("a")
  }

  def orderDomainValues(csp: CSP, unassignedVar: Variable, assignment: Assignment): LazyList[Int] = {
    LazyList[Int]()
  }

  def inference(csp: CSP, unassignedVar: Variable, newAssignment: Assignment): Option[Domain] = {
    None
  }
}
