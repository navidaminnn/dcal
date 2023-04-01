package com.github.distcompiler.dcal

abstract class DCalError {
  def description: String
}

// Use final case class here because
// 1- final: prevents extends, 2- case: enables pattern matching, 3- class: creates instances
final case class ModuleNotFound(dcalModule: DCalAST.Module) extends DCalError {
  val description = s"Referenced module ${dcalModule.name} not found: <line number>"
}
final case class DefinitionNotFound(dcalDef: DCalAST.Definition) extends DCalError {
  val description = s"Referenced definition ${dcalDef.name} not found: <line number>"
}
final case class NameNotFound(name: String) extends DCalError {
  val description = s"Referenced name $name not found: <line number>"
}
final case class MemberNotFound(name: String, memberName: String) extends DCalError {
  val description = s"$memberName not a member of $name: <line number>"
}

final case class RedefinedName(name: String) extends DCalError {
  val description = s"$name already defined in this scope: <line number>"
}

final case class ReassignmentToImmutable(name: String) extends DCalError {
  val description = s"immutable $name reassigned: <line number>"
}
