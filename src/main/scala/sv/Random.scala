package sv
//import constraint.SVMacros.RandInt

import csp.{Assignments, CSP, Constraint, Domain, Node, Solution, Variable}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox.Context

object Random{
  type RandInt = Int
  type RandArray = Array[Int]
}

class Random(val seed: Int = 42) {

  var cspO: Option[CSP] = None
  var randVars: List[(Variable, Domain)] = List[(Variable, Domain)]()
  var randVarsM: mutable.HashMap[Variable, Domain] = mutable.HashMap[Variable, Domain]()
  val mapOfConstraint: ListBuffer[Constraint] = ListBuffer[Constraint]()
  var randCVars: List[(Variable, Iterator[Int])] = List[(Variable, Iterator[Int])]()
  var randCVarsM: mutable.HashMap[Variable, Iterator[Int]] = mutable.HashMap[Variable, Iterator[Int]]()
  var iterator: Option[Iterator[Solution with Node]] = None
  var cAssignments: Option[Assignments] = Some(Assignments())
  var flag = false
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

  def addRandVar(myMap: (String, List[Int])): Unit = {
    randVarsM = randVarsM += (Variable(myMap._1) -> Domain(myMap._2))
  }

  def addRandCVar(myMap: (String, List[Int])): Unit = {
    val iter = LazyList.continually(myMap._2).flatten.iterator
    randCVarsM = randCVarsM += Variable(myMap._1) -> iter
  }
  def rand(param: Array[Int]): Array[Int] = macro sv.RandomMacros.randArray
  def rand(param: Int, dom: Range): Int = macro sv.RandomMacros.randVarDec
  def svrandc(param: Any): Variable = macro sv.RandomMacros.svrandcImpl
  def consMacro(param: (Int) => Boolean): ConstraintBlock = macro sv.RandomMacros.addConstraintBlock
  def unary(param: (Int) => Boolean): Constraint = macro sv.RandomMacros.createUnary
  def binary(param: (Int, Int) => Boolean): Constraint = macro sv.RandomMacros.createBinary
  def randomize: Unit = macro sv.RandomMacros.svrandomMacroImpl

  def randc_impl(myMap: (String, List[Int])): Variable = {
    val iter = LazyList.continually(myMap._2).flatten.iterator
    val addVar = Variable(myMap._1) -> iter
    randCVars = randCVars ::: List(addVar)
    Variable(myMap._1)
  }

  def restartIterator(): Unit = {
    cspO = Some(new CSP(randVarsM.keys.toList, randVarsM.toMap, mapOfConstraint.toList))
    iterator = Some(Solution(cspO.get, Assignments(), seed).backtrackingSearch(cspO.get).iterator)
  }

  def randomizeImp(): Option[Assignments] = {
    val cspAss = iterator match {
      case None => {
        restartIterator()
        if (iterator.isEmpty) None else Some(iterator.get.next().assignments)
      }
      case Some(x) =>  if (x.isEmpty) None else Some(x.next().assignments)
    }

    val toreturn = cspAss match {
      case None => None
      case Some(x) => Some(Assignments(x.mapVarValue ++ randCVars.flatMap(x => Map(x._1 -> x._2.next())).toMap))
    }
    cAssignments = toreturn
    toreturn
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
    val self = c.prefix
    val dom = List(1, 2, 3)
    val paramRep = show(param.tree)
    val newName = paramRep.substring(paramRep.lastIndexOf(".") + 1)
    q"""
        $self.rand_impl(($newName, $dom))
     """
  }

  def addConstraintBlock(c: Context)(param: c.Expr[Int => Boolean]): c.Tree = {
    import c.universe._
    val Function(args, _) = param.tree
    val ValDef(_, name, _, _) = args(0)
    val self = c.prefix
    val func = reify {
      param.splice
    }
    q"""
       $self.constraintBlock {
        _root_.csp.Unary(_root_.csp.Variable($name.toString), $func)
      }"""
  }

  def createBinary(c: Context)(param: c.Expr[(Int, Int) => Boolean]): c.Tree = {
    import c.universe._
    val Function(args, _) = param.tree
    val ValDef(_, x_name, _, _) = args(0)
    val ValDef(_, y_name, _, _) = args(1)
    val varx = x_name.toString
    val vary = y_name.toString
    val func = reify {
      param.splice
    }
    c.untypecheck(param.tree)
    q"_root_.csp.Binary(_root_.csp.Variable($varx), _root_.csp.Variable($vary), $func)"
  }

  def createUnary(c: Context)(param: c.Expr[Int => Boolean]): c.Tree = {
    import c.universe._
    val Function(args, _) = param.tree
    val ValDef(_, name, _, _) = args(0)
    val varName = name.toString
    val func = reify {
      param.splice
    }
    c.untypecheck(param.tree)
    q"_root_.csp.Unary(_root_.csp.Variable($varName), $func)"
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

  /**
    * Random Variable Declaration, this functions declares a random variable by setting its value to 0 and
    * adds it to the randVarM variables database.
    *
    * @param c
    * @param param
    * @param dom
    * @return
    */
  def randVarDec(c: Context)(param: c.Expr[sv.Random.RandInt], dom: c.Tree): c.Tree = {
    import c.universe._
    // TODO: using Tree is not very safe
    val self = c.prefix
    val typeString = param.actualType.toString
    val paramRep = show(param.tree)
    val newName = paramRep.substring(paramRep.lastIndexOf(".") + 1)
    // TODO: How can we check for a specific type?
    typeString match {
      case "sv.Random.RandInt" =>
        q"""
          $self.addRandVar(($newName, $dom.toList))
          0
        """
      case _ => q"""
              throw new Exception($newName + " is not declared as RandInt")
              """
    }
  }

  def svrandomMacroImpl(c: Context): c.Tree = {
    import c.universe._
    val self = c.prefix
    val currentType = self.actualType.baseType(self.actualType.baseClasses(0))
    val lol = for {
      t <- currentType.members
      s <- currentType.members
      g <- currentType.members
      tname = t.name.decodedName.toString.filter(_ != ' ')
      sname = s.name.decodedName.toString
      gname = g.name.decodedName.toString
      if t.isTerm && !t.isMethod && sname == tname + "_=" && g.isMethod && gname == tname
    } yield (t.asTerm.name.toString.filter(_ != ' '), s.asTerm.name, g.asTerm.name)
    // TODO: Filter non rand vars
    // val mapOfVars = lol.groupBy(z => Variable(z._1))
    val operations = lol.map { x =>
       val variable = x._1
       val setter = x._2
      println(x)
      q"""
          val myVar = Variable($variable)
          if ($self.cAssignments.isDefined) {
            if ($self.randVarsM.contains(myVar)) {
              val assignments = $self.cAssignments.get
              $self.$setter = assignments(myVar)
          }
         }
       """
    }
    q"""
       ..$operations
     """
  }
}
