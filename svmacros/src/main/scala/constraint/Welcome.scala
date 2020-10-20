package constraint


import scala.reflect.macros.blackbox.Context
// import scala.language.experimental.macros

object Welcome {
  def isEvenLog(number: Int): Unit = macro isEvenLogImplementation

  def isEvenLogImplementation(c: Context)(number: c.Tree): c.Tree = {
    import c.universe._

    q"""
       if ($number%2==0){
         println($number.toString + " is even")
       }else {
         println($number.toString + " is odd")
       }
    """
  }
}