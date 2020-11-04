package csp

case class Domain(values: List[Int])
case class Variable(name: String)

/**
  * Class representing the current Constraint Satisfaction Problem
  * @param variables List of variables used in the problem
  * @param varDomMap Map describing the domain assigned to each variable
  * @param constraints List of constraints in the problem
  */
class CSP(val variables: List[Variable], val varDomMap: Map[Variable, Domain], val constraints: List[Constraint]) {

  /**
    * Remove all the unary constraint before proceeding to solve the problem
    * @return CSP
    */
  def removeUnary(): CSP = {
    val newDomainMap = varDomMap.map { x =>
      val newDom = x._2.values.filter { value =>
      constraints
        .filter(c => c.isUnary && c.relatesToVar(x._1))
        .forall(cc => cc.isSatisfied(Map(x._1 -> value)))
      }
      (x._1, Domain(newDom))
    }
    new CSP(variables, newDomainMap, constraints)
  }

  /**
    * Create a new CSP with a restricted domain for the current variable
    * @param mapVarDomain
    * @return CSP
    */
  def restrictDomain(mapVarDomain: (Variable, Domain)): CSP = {
    val newDomain = (varDomMap - mapVarDomain._1) + mapVarDomain
    new CSP(variables, newDomain, constraints)
  }

  /**
    * Get all the constraint between two variables
    * @param variables List[Variable]
    * @return List(Constraint)
    */
  def getConstraints(variables: List[Variable]): List[Constraint] = {
    constraints.filter(c => c.relatesTo(variables))
  }

  /**
    * This function used in the [[revise]] function returns a list of topple (Xk, Xi) where Xk != Xj
    * We need to find all the neighbor of Xi because the changes in the domain Di might enable further reduction in the
    * domain of Dk that's why the list of tuple are (Xk, Xi) and not (Xi, Xk)
    * @param Xi variable with the new restricted domain
    * @param Xj variable which caused the domain restriction
    * @return List(Variable, Variable)
    */
  def reviseArcs(Xi: Variable, Xj: Variable): List[(Variable, Variable)] = {
    neighbors(Xi).filterNot(_ == Xj).map(variable => (variable, Xi))
  }

  /**
    * Get all the neighbor of the current variable by looking at its constraints
    * @param variable current variable
    * @return List[Variables] containing all the neighbors
    */
  def neighbors(variable: Variable): List[Variable] = {
    constraints.filter(c => c.relatesToVar(variable)).flatMap(c => c.neighbor.filterNot(_ == variable)).distinct
  }

  /**
    * Helper method to retrieve all the possible neighbors for all the variables in the problem
    * The name combination of arcs instead of combination of edges is to underline that this function is only used
    * int the AC_3 and MAC method as described in the book
    * @return List[(Variable, Variable)] a list of all the possible neighbors for each values.
    */
  val combinationOfArcs: List[(Variable, Variable)] = {
    val direct: List[(Variable, Variable)] =
      constraints.filterNot(_.isUnary).flatMap(c => List((c.neighbor.head, c.neighbor(1))))
    val inverse: List[(Variable, Variable)] = direct.map(_.swap)
    (direct ::: inverse).distinct
  }
}
