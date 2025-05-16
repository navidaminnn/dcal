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

package forja

final class TokenTests extends munit.FunSuite:
  import TokenTests.*

  test("tokens =="):
    assert(t1 == t1)
    assert(t2 == t2)

  test("tokens !="):
    assert(t1 != t2)
    assert(t2 != t1)

object TokenTests:
  object t1 extends Token
  object t2 extends Token
end TokenTests
