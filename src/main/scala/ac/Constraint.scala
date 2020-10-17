package ac

trait Constraint {
  /*
   * A constraint is consistent if there is at least a neighbor that is not assigned or if all its variable are
   * congruent with the constraint
   * @param neighbor the list of variables for which this constraint is applied
   */
  def isConsistent(neighbor: List[Variable], assignments: Assignments): Boolean


  /*
   * An assignment is complete only if all the neighbor have are assigned and their values are congruent with the
   * current constraint
   * @param neighbor list of variables that relies on this constraint
   */
  def isComplete(neighbor: List[Variable], assignments: Assignments): Boolean

  def relatesTo(variables: List[Variable]): Boolean
}

case class FunctionConstraint2V(neighbor: List[Variable], fun: (Int, Int) => Boolean) extends Constraint {

  override def isConsistent(neighbor: List[Variable], assignments: Assignments): Boolean = {
    require(neighbor.size == 2)
    !assignments.areAssigned(neighbor) || fun(assignments.mapVarValue(neighbor.head), assignments.mapVarValue(neighbor(1)))
  }

  override def isComplete(neighbor: List[Variable], assignments: Assignments): Boolean = {
    require(neighbor.size == 2)
    assignments.areAssigned(neighbor) && fun(assignments.mapVarValue(neighbor.head), assignments.mapVarValue(neighbor(1)))
  }

  override def relatesTo(variables: List[Variable]): Boolean = {
    require(variables.size == 2)
    require(neighbor.size == variables.size)
    variables.forall(neighbor.contains(_))
  }
}
