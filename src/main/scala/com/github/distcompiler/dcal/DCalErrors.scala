package com.github.distcompiler.dcal

import cats.data.Chain

case class DCalErrors(errors: Chain[DCalError]) extends Exception {
  override def getMessage: String = errors.toList.map{ err =>
    err.description
  }.mkString("\n")

  def ||(that: DCalErrors) =
    DCalErrors(this.errors ++ that.errors)

  def isEmpty = errors.isEmpty
}

object DCalErrors extends Exception {
  def apply(error: DCalError) = new DCalErrors(Chain(error))

  def apply(errors: List[DCalError]) = new DCalErrors(Chain.fromSeq(errors))

  def union(errsLst: List[DCalErrors]): DCalErrors =
    errsLst match
      case Nil => DCalErrors(Chain())
      case _ => errsLst.reduce(_ || _)

  def union(thisErrs: DCalErrors, thatErrs: DCalErrors): DCalErrors =
    union(List(thisErrs, thatErrs))
}
