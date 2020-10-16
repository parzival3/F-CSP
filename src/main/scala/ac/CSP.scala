package ac

case class Domain(values: List[Int])
case class Variable(name: String)
case class Constraint(fun: (Int, Int) => Boolean)

class CSP(val variables: List[Variable], val domainMap: Map[Variable, Domain], val mapOfConstraint: Map[Constraint, (Variable, Variable)]) {

  // TODO:
  def restrictDomain(newDomain: Map[Variable, Domain]): CSP = {
    new CSP(variables, newDomain, mapOfConstraint)
  }

  def restrictDomain(mapVarDomain: (Variable, Domain)): CSP = {
    val newDomain = (domainMap - mapVarDomain._1)  + mapVarDomain
    new CSP(variables, newDomain, mapOfConstraint)
  }

  /*
   * Auxiliary method used to check if the current pair of variables and their values are consistent
   * TODO: this method doesn't give the expected result, we need to add also the permutation (Xi, Xj) and (Xj, Xi)
   */
  def variablesConstraintMap: Map[(Variable, Variable), Iterable[Constraint]] = {
    mapOfConstraint.groupBy(_._2).view.mapValues(_.keys).toMap
  }

  /*
   * Method to check if the two variables are consistent
   * @param Xi first variable
   * @param Xj second variable
   * @param valuei value of the frist variable
   * @param valuej value of the second variable
   * @param mapOfConstraint a map of Constraint -> Variable, Variable
   */
  def isAssignmentConsistent(ass1: (Variable, Int), ass2: (Variable, Int)): Boolean = {
    reverseConstraintGraph2(ass1._1)(ass2._1).fun(ass1._2, ass2._2)
  }

  /*
   * Reverse the representation of a graph composed by a list of edge to neighbors (connected nodes) to a
   * Here the graph is a constraint graph and should not be confuse with a graph of the search tree. In here
   * the node are only represented by variables and the edges are the constraint.
   * node -> list of edges and neighbor node
   * From List(edge -> (node_1, node_2), edge -> (node_2, node_3)) ===> (node_1 -> Map(edge -> node_2, ...), node_2 -> ...)
   * @param directGraph a graph in the form of List(edge -> (node_1, node_2), edge -> (node_2, node_3))
   * @return a graph in the form of Map(node_1 -> Map(edge -> node_2, ...), node_2 -> ...)
   */
  def reverseConstraintGraph: Map[Variable, Map[Constraint, Variable]] = {
    /* Auxiliary function
     * Reverse the representation of a connection in a graph
     * From edge -> (node_1, node_2) ===> node_1 -> Map(edge -> node_2, ...)
     * @param edge the edge connecting two nodes
     * @param node the current node that we want to convert
     */
    def reverseEdgeDes(edge: (Constraint, (Variable, Variable)), node: Variable): (Constraint, Variable) = {
      if (edge._2._2 == node) (edge._1, edge._2._1) else (edge._1, edge._2._2)
    }

    def implementation: Map[Variable, Map[Constraint, Variable]] = {
      val setOfNode = mapOfConstraint.flatMap(x => List(x._2._1, x._2._2)).toSet
      setOfNode.map { cv =>
        val edgeToNode = mapOfConstraint withFilter (con => Set(con._2._1, con._2._2).contains(cv)) map (node => reverseEdgeDes(node, cv))
        (cv -> edgeToNode)
      }.toMap
    }

    implementation
  }

  def reverseConstraintGraph2: Map[Variable, Map[Variable, Constraint]] = {

    /* Auxiliary function
     * Reverse the representation of a connection in a graph
     * From edge -> (node_1, node_2) ===> node_1 -> Map(edge -> node_2, ...)
     * @param edge the edge connecting two nodes
     * @param node the current node that we want to convert
     */
    def reverseEdgeDes(edge: (Constraint, (Variable, Variable)), node: Variable): (Variable, Constraint) = {
      if (edge._2._2 == node) (edge._2._1, edge._1) else (edge._2._2, edge._1)
    }

    def implementation: Map[Variable, Map[Variable, Constraint]] = {
      // TODO: there must be a better way to do this
      val setOfNode = mapOfConstraint.flatMap(x => List(x._2._1, x._2._2)).toSet
      setOfNode.map { cv =>
        val edgeToNode = mapOfConstraint withFilter (con => Set(con._2._1, con._2._2).contains(cv)) map (node => reverseEdgeDes(node, cv))
        (cv -> edgeToNode)
      }.toMap
    }

    implementation
  }
}
