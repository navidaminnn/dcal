package com.github.distcompiler.dcal

abstract class DCalError {
  def description: String
}

/**
 * Thrown when the DCal file of an imported module is not found in the file system.
 */
final case class ModuleNotFound(dcalModuleName: String) extends DCalError {
  val description = s"Referenced module $dcalModuleName not found: <line number>"
}

/**
 * Thrown when a definition name in a procedure call is not found.
 */
final case class DefinitionNotFound(dcalDefName: String) extends DCalError {
  val description = s"Referenced definition $dcalDefName not found: <line number>"
}

/**
 * Thrown when a name, in a position other than an import statement or a procedure call, is not found.
 */
final case class NameNotFound(name: String) extends DCalError {
  val description = s"Referenced name $name not found: <line number>"
}

/**
 * Thrown when memberName in <\name>.<\memberName> is not found.
 */
final case class MemberNotFound(name: String, memberName: String) extends DCalError {
  val description = s"$memberName not a member of $name: <line number>"
}

/**
 * Thrown when an existing name is redeclared. Any one of an import, a definition, a let statement, or a var statement
 * can cause a name redeclaration error, regardless of what the initial declaration is.
 */
final case class RedeclaredName(name: String) extends DCalError {
  val description = s"$name already defined in this scope: <line number>"
}

/**
 * Thrown when a name other than var name appears in an assign pair's left-hand side. This includes module, definition,
 * and value names.
 */
final case class ReassignmentToImmutable(name: String) extends DCalError {
  val description = s"Immutable $name reassigned: <line number>"
}

/**
 * Thrown when two modules import each other, directly or indirectly.
 */
final case class CircularDependency(moduleName: String, importName: String) extends DCalError {
  val description = s"Circular dependency detected in $moduleName, involving the module $importName: <line number>"
}
