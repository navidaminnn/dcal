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

package forja.manip

import munit.Location
import munit.diff.DiffOptions

class SegmentedStackTests extends munit.FunSuite:
  given munit.Compare[Integer | Null, Int] with
    def isEqual(obtained: Integer | Null, expected: Int): Boolean =
      obtained match
        case null         => false
        case int: Integer => int == expected
  given munit.Compare[Int, Integer] with
    def isEqual(obtained: Int, expected: Integer): Boolean =
      obtained == expected

  // 33 is more than twice 16, the segment size
  (0 to 33).foreach: count =>
    test(s"push ascending numbers 0-$count"):
      val stack = SegmentedStack[Integer]()

      (0 to count).foreach: num =>
        stack.push(num)

      (0 to count).reverse.foreach: num =>
        assertEquals(stack.pop(), num)

      (0 until 3).foreach: _ =>
        assertEquals(stack.pop(), null)

    test(s"push-pop ascending numbers 0-$count"):
      val stack = SegmentedStack[Integer]()

      (0 to count).foreach: num =>
        stack.push(num)
        assertEquals(stack.pop(), num)
        stack.push(num)

      (0 to count).reverse.foreach: num =>
        assertEquals(stack.pop(), num)
        stack.push(num)
        assertEquals(stack.pop(), num)

      (0 until 3).foreach: _ =>
        assertEquals(stack.pop(), null)

    test(s"consumeInReverse 0-$count"):
      val stack = SegmentedStack[Integer]()

      (0 to count).foreach: num =>
        stack.push(num)
      var idx = 0
      stack.consumeInReverse: actualIdx =>
        assertEquals(idx, actualIdx)
        idx += 1
      assertEquals(stack.pop(), null)

      // do it twice in case we're corrupted
      (0 to count).foreach: num =>
        stack.push(num)
      idx = 0
      stack.consumeInReverse: actualIdx =>
        assertEquals(idx, actualIdx)
        idx += 1
      assertEquals(stack.pop(), null)
