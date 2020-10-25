package sv

class SVEnumeration extends Enumeration {
  def domainValues(): List[Int] = {
    values.map(x => x.id).toList
  }
}
