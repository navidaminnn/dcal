-------------------------- MODULE DefTranslations --------------------------
EXTENDS Integers, FiniteSets

\* DCal: def mt () {}
mt(_state1) ==
    _state1

\* DCal: def resetString() { str = "new string" }
resetString(_state1) ==
    LET
        _state2 == { [s EXCEPT !.str = "new string"]: s \in _state1 }
    IN
        _state2

\* DCal: def sum(p1, p2) { let local = p1 + p2 x := local }
sum(_state1, p1, p2) ==
    LET
        _state2 == UNION {
            LET
                local == p1 + p2
            IN
                { [ss EXCEPT !.x = local] : ss \in { s } }
            : s \in _state1
        }
    IN
        _state2

\* def foo() { let z == y + 1 x := z - 3 await x < 7 }
foo(_state1) ==
    LET
        \* UNION is for flattening the sets produced by nested let expressions
        _state2 == UNION {
            LET
                z == s.y + 1
            IN
                LET
                    _state3 == { [ss EXCEPT !.x = z - 3 ]: ss \in { s } }
                IN
                    LET
                        _state4 == { ss \in _state3: ss.x < 7 }
                    IN
                        _state4
            : s \in _state1
        }
    IN
        _state2

\* DCal: def bar(v) { y := y - v i := i + 1 }
bar(_state1, v) ==
    LET
        _state2 == { [s EXCEPT !.y = s.y - v ]: s \in _state1 }
    IN
        LET
            _state3 == { [s EXCEPT !.i = s.i + 1]: s \in _state2 }
        IN
            _state3

\* states == {[x |-> 0, y |-> 0, str |-> "", i |-> 1]}
=============================================================================