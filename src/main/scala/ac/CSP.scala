package ac

case class Domain(values: List[Int])
case class Variable(name: String)


class CSP(val variables: List[Variable], val domainMap: Map[Variable, Domain], val constraints: List[Constraint]) {

  def restrictDomain(mapVarDomain: (Variable, Domain)): CSP = {
    val newDomain = (domainMap - mapVarDomain._1)  + mapVarDomain
    new CSP(variables, newDomain, constraints)
  }

  /*
     * Get all the constraint between two variables
     * @param variables List[Variable]
     * @return List(Constraint)
    */
  def getConstraints(variables: List[Variable]): List[Constraint] = {
    constraints.filter(c => c.relatesTo(variables))
  }

  def reviseArcs(Xi: Variable, Xj: Variable): List[(Variable, Variable)] = {
    constraints.filter(c => c.relatesToVar(Xi) && !c.relatesToVar(Xj)).map(c => (c.getOthers(Xi).head, Xi)).distinct
  }

  def neighbors(variable: Variable): List[Variable] = {
    constraints.filter(c => c.relatesToVar(variable)).flatMap(c => c.neighbor.filterNot(_ == variable)).distinct
  }

  val combinationOfNeighbors: List[(Variable, Variable)] =
    constraints.flatMap(c => List((c.neighbor.head, c.neighbor(1)), (c.neighbor(1), c.neighbor.head))).distinct
}
