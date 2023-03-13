# Languages and paradigms of programming - prolog (June 2022)
Deterministic Finite Automata in Prolog

Automata are represented as:

dfa(TransitionFunction, StartingState, SetOfAcceptingStates)

where parameters are represented as:
TransitionFunction - fp(oldstate, letter, newstate)
StartingState - state
SetOfAcceptingStates - list of states in square brackets

Implemented predicates:
+ correct(+Automaton, -Representation) - succeeds when Automaton is term representing automaton and Representation is some representation of it
+ accept(+Automaton, ?Word) - succeeds when Automaton accepts Word (Word can be fixed term as well as have holes and require code to give every possible answer)
+ empty(+Automaton) - succeeds when Automaton is empty
+ equal(+Automaton1, +Automaton2) - succeeds when languages accepted by automata are equal
+ subsetEq(+Automata1, +Automata2) - succeeds when language recognized by Automata1 is subset of language recognized by Automata2

Examples are in code.
