package ac

case class Domain(values: List[Int])
case class Variable(name: String)


class CSP(val variables: List[Variable], val domainMap: Map[Variable, Domain], val mapOfConstraint: Map[FunctionConstraint2V, (Variable, Variable)]) {
  // TODO: Transform the Map of constraint into a list
  val constraints: List[FunctionConstraint2V] = mapOfConstraint.keySet.toList
  // TODO:
  def restrictDomain(newDomain: Map[Variable, Domain]): CSP = {
    new CSP(variables, newDomain, mapOfConstraint)
  }

  def restrictDomain(mapVarDomain: (Variable, Domain)): CSP = {
    val newDomain = (domainMap - mapVarDomain._1)  + mapVarDomain
    new CSP(variables, newDomain, mapOfConstraint)
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

  /*
   * Auxiliary method used to check if the current pair of variables and their values are consistent
   * TODO: this method doesn't give the expected result, we need to add also the permutation (Xi, Xj) and (Xj, Xi)
   */
  def variablesConstraintMap: Map[(Variable, Variable), Iterable[FunctionConstraint2V]] = {
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
    neighbors(ass1._1).get(ass2._1) match {
      case Some(x) => x.forall(_.fun(ass1._2, ass2._2))
      case None => true
    }
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
  def reverseConstraintGraph: Map[Variable, Map[FunctionConstraint2V, Variable]] = {
    /* Auxiliary function
     * Reverse the representation of a connection in a graph
     * From edge -> (node_1, node_2) ===> node_1 -> Map(edge -> node_2, ...)
     * @param edge the edge connecting two nodes
     * @param node the current node that we want to convert
     */
    def reverseEdgeDes(edge: (FunctionConstraint2V, (Variable, Variable)), node: Variable): (FunctionConstraint2V, Variable) = {
      if (edge._2._2 == node) (edge._1, edge._2._1) else (edge._1, edge._2._2)
    }

    def implementation: Map[Variable, Map[FunctionConstraint2V, Variable]] = {
      variables.map { cv =>
        val edgeToNode = mapOfConstraint withFilter (con => Set(con._2._1, con._2._2).contains(cv)) map (node => reverseEdgeDes(node, cv))
        (cv -> edgeToNode)
      }.toMap
    }

    implementation
  }

  def neighbors: Map[Variable, Map[Variable, Iterable[FunctionConstraint2V]]] = {
    // ATTENTION: This implies that the constraint are unique in the initial graph map
    reverseConstraintGraph.map(x => x._1 -> x._2.groupBy(_._2).view.mapValues(_.keys).toMap)
  }
}
