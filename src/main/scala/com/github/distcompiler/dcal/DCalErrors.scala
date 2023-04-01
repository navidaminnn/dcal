package com.github.distcompiler.dcal

case class DCalErrors(errors: List[DCalError]) extends Exception {
  def this(error: DCalError) =
    this(errors = List(error))

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
  def apply(error: DCalError) = new DCalErrors(error)
  def union(lstOpt: List[Option[DCalErrors]]): Option[DCalErrors] =
    lstOpt.flatten.reduceOption(_ || _)

  def union(thisOpt: Option[DCalErrors], thatOpt: Option[DCalErrors]): Option[DCalErrors] =
    union(List(thisOpt, thatOpt))
}
