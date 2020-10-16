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
 *      if result 6= failure then return result
 *        remove inferences from csp
 *    remove {var = value} from assignment
 *  return failure
 */
case class Assignment() {
  def addValue(unassignedVar: Variable, value: Int): Assignment = {
    Assignment()
  }

  def isConsistent(value: Int): Boolean =  {
    true
  }

  def isComplete: Boolean = true
}

case class Solution(csp: CSP, assignment: Assignment) extends Node

trait Node {
  def children(solution: Solution): LazyList[Solution with Node] = {
    val unassignedVar = selectUnassignedVar(solution.csp, solution.assignment)
    orderDomainValues(solution.csp, unassignedVar, solution.assignment).map { value =>
      solution.assignment.isConsistent(value)
      val newAssignment = solution.assignment.addValue(unassignedVar, value)
      val resInference = inference(solution.csp, unassignedVar, newAssignment)
      Solution(solution.csp.restrictDomain(resInference._2), newAssignment)
    }
  }
  def backtrackingSearch:(Boolean, Assignment) = backtrack(csp, Assignment())

  def selectUnassignedVar(csp: CSP, assignment: Assignment): Variable = {
    Variable("a")
  }

  def orderDomainValues(csp: CSP, unassignedVar: Variable, assignment: Assignment): LazyList[Int] = {
    LazyList[Int]()
  }

  def inference(csp: CSP, unassignedVar: Variable, newAssignment: Assignment): (Boolean, Domain) = {
    (true, Domain(List[Int]()))
  }

  @tailrec
  protected def backtrack(csp: CSP, assignment: Assignment): (Boolean, Assignment) = {
    if (assignment.isComplete) {
      (true, Assignment())
    } else {
      val unassignedVar = selectUnassignedVar(csp, assignment)
      orderDomainValues(csp, unassignedVar, assignment).foreach { value =>
        assignment.isConsistent(value)
        val newAssignment = assignment.addValue(unassignedVar, value)
        val resInference = inference(csp, unassignedVar, newAssignment)
        if (resInference._1) {
            backtrack(csp.restrictDomain(resInference._2), newAssignment)
        }
      }
    }
  }
}
