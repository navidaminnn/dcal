package com.github.distcompiler.dcal

class DCalErrors(val errors: List[DCalError]) extends Exception {
  override def getMessage: String = errors.map{ err =>
    err.description
  }.mkString("\n")

  def ||(that: DCalErrors) =
    DCalErrors(this.errors ::: that.errors)

  def toOption: Option[DCalErrors] =
    errors match
      case Nil => None
      case _ => Some(this)
}

object DCalErrors extends Exception {
  def union(lstOpt: List[Option[DCalErrors]]): Option[DCalErrors] =
    lstOpt.flatten.reduce(_ || _).toOption

  def union(thisOpt: Option[DCalErrors], thatOpt: Option[DCalErrors]): Option[DCalErrors] =
    union(List(thisOpt, thatOpt))
}
