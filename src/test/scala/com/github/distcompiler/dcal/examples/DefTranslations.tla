-------------------------- MODULE DefTranslations --------------------------
EXTENDS Integers, FiniteSets

\* DCal: def mt () {}
mt(_state1) ==
    _state1

\* DCal: def resetString () { str = "new string" }
resetString(_state1) ==
    LET
        _state2 == { [s EXCEPT !.str = "new string"]: s \in _state1 }
    IN
        _state2

\* DCal: def sum (p1, p2) { let local = p1 + p2 x := local }
sum(_state1, p1, p2) ==
    LET
        local == p1 + p2
    IN
        LET
            _state2 == { [s EXCEPT !.x = local]: s \in _state1 }
        IN
            _state2

\* DCal: def change(v) { y := y - v i := i + 1 }
change(_state1, v) ==
    LET
        _state2 == { [s EXCEPT !.y = s.y - v ]: s \in _state1 }
    IN
        LET
            _state3 == { [s EXCEPT !.i = s.i + 1]: s \in _state2 }
        IN
            _state3

\* states == {[x |-> 0, y |-> 0, str |-> "", i |-> 1]}
=============================================================================