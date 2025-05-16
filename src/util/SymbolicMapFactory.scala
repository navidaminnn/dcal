// Copyright 2024-2025 Forja Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package forja.util

import scala.reflect.ClassTag
import java.lang.ref.{ReferenceQueue, WeakReference}
import java.util.concurrent.atomic.AtomicInteger

abstract class SymbolicMapFactory:
  trait Mapped:
    val mapIdx: Int =
      val idx = Mapped.refQueue.poll() match
        case ref: Mapped.MappedRef =>
          Mapped.refKeepAlive.remove(ref)
          ref.mapIdx
        case null => Mapped.nextIdx.incrementAndGet()

      Mapped.refKeepAlive(Mapped.MappedRef(this, idx)) = ()
      idx
    end mapIdx
  end Mapped

  object Mapped:
    private final class MappedRef(mapped: Mapped, val mapIdx: Int)
        extends WeakReference[Mapped](mapped, refQueue)
    private val refQueue = ReferenceQueue[Mapped]()
    private val refKeepAlive =
      scala.collection.concurrent.TrieMap[MappedRef, Unit]()
    private var nextIdx = AtomicInteger()
  end Mapped

  final class Map[@specialized T] private (
      private var array: Array[T],
      default: => T,
  )(using val classTag: ClassTag[T]):
    private def defaultFn: T = default

    def apply(key: Mapped): T =
      val idx = key.mapIdx
      if idx < array.length
      then array(idx)
      else default

    def copyFrom(other: Map[T]): Unit =
      ensureCapacity(other.array.length - 1 `max` 0)
      Array.copy(other.array, 0, array, 0, other.array.length)
      var idx = other.array.length
      while idx < array.length do
        array(idx) = default
        idx += 1

    private def ensureCapacity(idx: Int): Unit =
      val oldLength = array.length
      var length = oldLength
      if length == 0 then length += 1
      while length <= idx do length *= 2
      assert(length >= 1)
      if length != array.length
      then
        array = Array.copyOf(array, length)
        var i = oldLength
        while i < length do
          array(i) = default
          i += 1

    def update(key: Mapped, value: T): Unit =
      val idx = key.mapIdx
      ensureCapacity(idx)
      array(idx) = value

    def remove(key: Mapped): Unit =
      val idx = key.mapIdx
      if idx < array.length
      then array(idx) = default

    def updated(key: Mapped, value: T): Map[T] =
      val next = Map(array.clone(), default)
      next.update(key, value)
      next

    def removed(key: Mapped): Map[T] =
      val next = Map(array.clone(), default)
      next.remove(key)
      next
  end Map

  object Map:
    def empty[T: ClassTag](default: => T): Map[T] = Map(Array.empty, default)
  end Map
end SymbolicMapFactory

object TokenMapFactory extends SymbolicMapFactory
export TokenMapFactory.Map as TokenMap
