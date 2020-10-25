package sv
//import constraint.SVMacros.RandInt

import csp.{Assignments, CSP, Constraint, Domain, Node, Solution, Variable}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox.Context
object Random {
  type RandInt = Int
  type RandArray = Array[Int]
}

class Random(val seed: Int = 42) {


  var csp: Option[CSP] = None
  var randVars: List[(Variable, Domain)] = List[(Variable, Domain)]()
  var randVarsM: mutable.HashMap[Variable, Domain] = mutable.HashMap[Variable, Domain]()
  val mapOfConstraint: ListBuffer[Constraint] = ListBuffer[Constraint]()
  var randCVars: List[(Variable, Iterator[Int])] = List[(Variable, Iterator[Int])]()
  var iterator: Option[Iterator[Solution with Node]] = None

  /**
   * Constraint block class. This class encapsulate a list of constraints.
   * @param constraints
   * @param r
   */
  class ConstraintBlock(val constraints: List[Constraint], val r: Random) {
    def disable(): Unit = {
      r.removeConstraints(constraints)
      r.restartIterator()
    }

    def enable(): Unit = {
      r.addConstraints(constraints)
      r.restartIterator()
    }
  }

  def rand_impl(myMap: (String, List[Int])): Variable = {
    val addVar = Variable(myMap._1) -> Domain(myMap._2)
    randVarsM += (Variable(myMap._1) -> Domain(myMap._2))
    randVars = randVars ::: List(addVar)
    Variable(myMap._1)
  }

  def addRandVar(myMap: (String, List[Int])): Variable = {
    val addVar = Variable(myMap._1) -> Domain(myMap._2)
    randVarsM += (Variable(myMap._1) -> Domain(myMap._2))
    randVars = randVars ::: List(addVar)
    Variable(myMap._1)
  }

  def rand(param: Array[Int]): Array[Int] = macro sv.RandomMacros.randArray
  def rand(param: Int): Int = macro sv.RandomMacros.randInt
  def randd(param: Int, dom: Range): Int = macro sv.RandomMacros.randDec
  def svrandc(param: Any): Variable = macro sv.RandomMacros.svrandcImpl
  def randomMacro[T]: Unit = macro sv.RandomMacros.svrandomMacroImpl[T]

  def randc_impl(myMap: (String, List[Int])): Variable = {
    val iter = LazyList.continually(myMap._2).flatten.iterator
    val addVar = Variable(myMap._1) -> iter
    randCVars = randCVars ::: List(addVar)
    Variable(myMap._1)
  }

  def restartIterator(): Unit = {
    csp = Some(new CSP(randVars.toMap.keySet.toList, randVars.toMap, mapOfConstraint.toList))
    iterator = Some(Solution(csp.get, Assignments(), seed).backtrackingSearch(csp.get).iterator)
  }

  def randomize(): Option[Assignments] = {
    val cspAss = iterator match {
      case None => {
        restartIterator()
        if (iterator.isDefined) None else Some(iterator.get.next().assignments)
      }
      case Some(x) =>  if (x.isEmpty) None else Some(x.next().assignments)
    }

    cspAss match {
      case None => None
      case Some(x) => Some(Assignments(x.mapVarValue ++ randCVars.flatMap(x => Map(x._1 -> x._2.next())).toMap))
    }
  }

  def constraintBlock(constraints: Constraint*): ConstraintBlock = {
    addConstraints(constraints.toList)
    new ConstraintBlock(constraints.toList, this)
  }

  def removeConstraints(constraints: List[Constraint]): Unit = {
    mapOfConstraint --= constraints
  }

  def addConstraints(constraints: List[Constraint]): Unit = {
    mapOfConstraint ++= constraints
  }
}

object RandomMacros {

  def svrandcImpl(c: Context)(param: c.Expr[Any]): c.Tree = {
    import c.universe._
    if (param.actualType.toString == "Array[Int]") {
      val paramRep = show(param.tree)
      val paramRepTree = Literal(Constant(paramRep))
      println(paramRepTree.toString())
    }
    reify { println(param.splice) }
    val self = c.prefix
    val dom = List(1, 2, 3)
    val paramRep = show(param.tree)
    val newName = paramRep.substring(paramRep.lastIndexOf(".") + 1)
    q"""
        println($paramRep)
        $self.rand_impl(($newName, $dom))
     """
  }

  def randInt(c: Context)(param: c.Expr[sv.Random.RandInt]): c.Tree = {
    import c.universe._
    val typeString = param.actualType.toString
    val paramRep = show(param.tree)
    val newName = paramRep.substring(paramRep.lastIndexOf(".") + 1)
    // TODO: How can we check for a specific type?
    typeString match {
      case "sv.Random.RandInt" => q"10"
      case _ => q"""
              throw new Exception($newName + " is not declared as RandInt")
              """
    }
  }

  def randArray(c: Context)(param: c.Expr[sv.Random.RandArray]): c.Tree = {
    import c.universe._
    val typeString = param.actualType.toString
    val paramRep = show(param.tree)
    val newName = paramRep.substring(paramRep.lastIndexOf(".") + 1)
    // TODO: How can we check for a specific type?
    typeString match {
      case "sv.Random.RandArray" => q"Array(10)"
      case _ => q"""
              throw new Exception($newName + " is not declared as RandArray")
              """
    }
  }

  def randDec(c: Context)(param: c.Expr[sv.Random.RandInt], dom: c.Tree): c.Tree = {
    import c.universe._
    // TODO: using Tree is not very safe
    val typeString = param.actualType.toString
    val paramRep = show(param.tree)
    val newName = paramRep.substring(paramRep.lastIndexOf(".") + 1)
    // val splice = dom.splice
    // TODO: How can we check for a specific type?
    typeString match {
      case "sv.Random.RandInt" =>
        q"""
          println($dom.toList)
          50
         """
      case _ => q"""
              throw new Exception($newName + " is not declared as RandInt")
              """
    }
  }

  def svrandomMacroImpl[T: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    val self = c.prefix
    val lol = for {
      t <- weakTypeOf[T].members
      s <- weakTypeOf[T].members
      g <- weakTypeOf[T].members
      tname = t.name.decodedName.toString.filter(_ != ' ')
      sname = s.name.decodedName.toString
      gname = g.name.decodedName.toString
      if t.isTerm && !t.isMethod && sname == tname + "_=" && g.isMethod && gname == tname
    } yield (t, s, g)
    lol.foreach(x => println((x._1.name.decodedName.toString.filter(_ != ' '), x._2.name.decodedName.toString, x._3.name.decodedName.toString)))
    val operations = lol.map{x =>
      val first = x._1.asTerm.name
      val second = x._2.asTerm.name
      val third = x._3.asTerm.name
      val fistname = first.decodedName.toString.filter(_ != ' ')
      q"""
          val variable = csp.Variable($fistname)
          println(variable)
          if ($self.randVarsM.contains(variable)) $self.$second = $self.$third + 1
       """

    }
    q"""
      ..$operations
     """
  }
}
