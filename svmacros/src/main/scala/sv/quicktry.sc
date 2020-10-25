class Random {
  def printHashId(x: Int): Unit = {
    println(System.identityHashCode(x))
  }
}

class Packet extends Random {
  var src = 0
  var size = 0
  printHashId(src)
  printHashId(size)
}

val o = new Packet

val z = (0 to 10).toList
val iter = LazyList.continually(z).flatten.iterator.drop(4).take(30).toList
println(iter)

object myEnum extends Enumeration {
  val lol = Value(1)
  val ab = Value(2)
}

println(myEnum.values.map(x => x.id))