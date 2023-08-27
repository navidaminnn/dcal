package distcompiler.db

import cats.*
import cats.data.*
import cats.syntax.all.given

final class Db[T](procFns: Set[T => Db[T]], data: Set[T]) {

}

object Db {

}
