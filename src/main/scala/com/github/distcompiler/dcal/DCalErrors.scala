package com.github.distcompiler.dcal

case class DCalErrors(errors: List[DCalError]) extends Exception {
  override def getMessage: String = errors.map{ err =>
    err.description
  }.mkString("\n")

  def ||(that: DCalErrors) =
    DCalErrors(this.errors ::: that.errors)
}

object DCalErrors extends Exception {
  def apply(error: DCalError) = new DCalErrors(List(error))

  def union(errsLst: List[DCalErrors]): DCalErrors =
    errsLst.reduce(_ || _)

  def union(thisErrs: DCalErrors, thatErrs: DCalErrors): DCalErrors =
    union(List(thisErrs, thatErrs))
}
