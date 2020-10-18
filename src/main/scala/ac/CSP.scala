package ac

case class Domain(values: List[Int])
case class Variable(name: String)


class CSP(val variables: List[Variable], val domainMap: Map[Variable, Domain], val constraints: List[Constraint]) {
  /**
   * Remove all the unary constraint before proceeding to solve the problem
   * @return CSP
   */
  def removeUnary(): CSP = {
    val newDomainMap = domainMap.map {x =>
      val newDom = x._2.values.filter { value =>
         constraints.filter(c => c.isUnary && c.relatesToVar(x._1))
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
    val newDomain = (domainMap - mapVarDomain._1)  + mapVarDomain
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
    constraints.filter(c => !c.isUnary)
    .filter(c => c.relatesToVar(Xi) && !c.relatesToVar(Xj)).map(c => (c.getOther(Xi).get, Xi)).distinct
  }

  def neighbors(variable: Variable): List[Variable] = {
    constraints.filter(c => c.relatesToVar(variable)).flatMap(c => c.neighbor.filterNot(_ == variable)).distinct
  }

  val combinationOfNeighbors: List[(Variable, Variable)] =
    constraints.filterNot(_.isUnary)
    .flatMap(c => List((c.neighbor.head, c.neighbor(1)), (c.neighbor(1), c.neighbor.head))).distinct
}
