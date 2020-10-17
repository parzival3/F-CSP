package ac

trait Constraint {
  /*
   * A constraint is consistent if there is at least a neighbor that is not assigned or if all its variable are
   * congruent with the constraint
   * @param neighbor the list of variables for which this constraint is applied
   */
  def isConsistent(variables: List[Variable], assignments: Assignments): Boolean

  def relatesTo(variables: List[Variable]): Boolean

  def isSatisfied(mapVariableValue: Map[Variable, Int]): Boolean

  def relatesToVar(variable: Variable): Boolean

  def getOthers(variable: Variable): List[Variable]

  def neighbor: List[Variable]
}

case class FunctionConstraint2V(values: List[Variable], fun: (Int, Int) => Boolean) extends Constraint {

  override def isConsistent(variables: List[Variable], assignments: Assignments): Boolean = {
    require(variables.size == 2)
    !assignments.areAssigned(variables) || fun(assignments.mapVarValue(variables.head), assignments.mapVarValue(variables(1)))
  }

  override def relatesTo(variables: List[Variable]): Boolean = {
    require(variables.size == 2)
    require(values.size == variables.size)
    variables.forall(values.contains(_))
  }

  override def isSatisfied(mapVariableValue: Map[Variable, Int]): Boolean =  {
    require(mapVariableValue.size == 2)
    fun(mapVariableValue(values.head), mapVariableValue(values(1)))
  }

  override def relatesToVar(variable: Variable): Boolean = {
    values.contains(variable)
  }

  override def getOthers(variable: Variable): List[Variable] = {
    values.filterNot(_  == variable)
  }

  override def neighbor: List[Variable] = values
}
