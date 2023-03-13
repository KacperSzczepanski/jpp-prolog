% author: Kacper Szczepański 418474
% representation: repr(BST, Starting, Terminals, Language, S) where
% BST - structure for quick finding moves in automat
% Starting - starting state
% Terminals - list of terminating states
% Language - finite or infinite
% S - number of states

% insertBST(+Tree, +Key, +Value, -NewTree) iff newTree is new BST after insertion
insertBST(empty, Key, Value, node(empty, Key, Value, empty)).
insertBST(node(L, Key, _, R), Key, Value, node(L, Key, Value, R)).
insertBST(node(L, K, V, R), Key, Value, node(L2, K, V, R)) :-
    Key @< K,
    insertBST(L, Key, Value, L2).
insertBST(node(L, K, V, R), Key, Value, node(L, K, V, R2)) :-
    Key @> K,
    insertBST(R, Key, Value, R2).

% makeBST(+Pairs, -Tree) iff Tree is BST made of pairs (Key, Value)
makeBST(Pairs, Tree) :- makeBST(Pairs, empty, Tree).
makeBST([], Acc, Acc).
makeBST([fp(A, E, B) | L], Acc, Tree) :- % BST of A exists
    findBST(Acc, A, T1),
    \+ findBST(T1, E, _), % found existing edge - automat not determinstic
    insertBST(T1, E, B, T2),
    insertBST(Acc, A, T2, T3),
    makeBST(L, T3, Tree).
makeBST([fp(A, E, B) | L], Acc, Tree) :- % BST of A does not exist
    \+ findBST(Acc, A, _),
    insertBST(empty, E, B, T2),
    insertBST(Acc, A, T2, T3),
    makeBST(L, T3, Tree).


% sizeBST(+Tree, -N) iff Tree is size of N
sizeBST(empty, 0).
sizeBST(node(L, _, _, R), N) :-
    sizeBST(L, Nl),
    sizeBST(R, Nr),
    N is Nl + Nr + 1.

% move(+Tree, +State, +Edge, -Dest) iff Dest is result of move(State, Edge)
move(Tree, State, Edge, Dest) :-
    findBST(Tree, State, Neighbours),
    findBST(Neighbours, Edge, Dest).

% findBST(+Tree, +Key, -Value) iff Key corresponds to Value in BST Tree
findBST(node(L, K, _, _), Key, Value) :-
    findBST(L, Key, Value),
    Key @< K.
findBST(node(_, K, _, R), Key, Value) :-
    findBST(R, Key, Value),
    Key @> K.
findBST(node(_, Key, Value, _), Key, Value).

% path(+BST, +Src, +Dst, +Vis) iff there is a simple path from Src to Dst
path(_, X, X, [X]).
path(BST, A, B, Vis) :-
    \+ member(B, Vis),
    move(BST, A, _, B).
path(BST, A, B, Vis) :-
    move(BST, A, _, X),
    \+ member(X, Vis),
    path(BST, X, B, [X | Vis]).

% anyPath(+BST, +Src, +Dsts) iff there is a simple path from Src to any state in Dsts
anyPath(BST, Src, [Dst | _]) :-
    path(BST, Src, Dst, [Src]).
anyPath(BST, Src, [_ | L]) :-
    anyPath(BST, Src, L).

% dfsInf(+BST, +State, +Terminals, +Word, +Iters)
% iff there is a path of length Iters from State to any state
% from Terminals set obtaining word Word
dfsInf(_, State, Terminals, [], 0) :-
    member(State, Terminals).
dfsInf(Tree, State, Terminals, [E | L], Iters) :-
    move(Tree, State, E, S2),
    I2 is Iters - 1,
    dfsInf(Tree, S2, Terminals, L, I2).

% dfsFin(+BST, +State, +Terminals, +Word, +CurLen, +UB)
% iff there is a path of length not bigger than UB
% ending in any state from Terminals and obtaining word Word
dfsFin(_, State, Terminals, [], CurLen, UB) :-
    CurLen =< UB,
    member(State, Terminals).
dfsFin(BST, State, Terminals, [E | L], CurLen, UB) :-
    CurLen =< UB,
    move(BST, State, E, S2),
    CL2 is CurLen + 1,
    dfsFin(BST, S2, Terminals, L, CL2, UB).

% getWord(+Tree, +State, +Terminals, +Language, +NoOfStates, ?Word)
% iff there is a word Word in automat with such properties
getWord(Tree, State, Terminals, infinite, _, Word) :-
    length(Word, N),
    dfsInf(Tree, State, Terminals, Word, N).
getWord(Tree, State, Terminals, finite, S, Word) :-
    dfsFin(Tree, State, Terminals, Word, 0, S).

% getStatesAndEdges(+Moves, -States, -Edges)
% iff States is set of states and Edges set of edges
getStatesAndEdges(Moves, States, Edges) :-
    getStatesAndEdges(Moves, [], States, [], Edges).
getStatesAndEdges([], AccS, AccS, AccE, AccE).
getStatesAndEdges([fp(A, E, B) | L], AccS, States, AccE, Edges) :-
    getStatesAndEdges(L, [A | [B | AccS]], States, [E | AccE], Edges).
    

partitionQS([], _, [], []).
partitionQS([X | L], P, [X | M], D) :-
  X @< P,
  partitionQS(L, P, M, D).
partitionQS([X | L], P, M, [X | D]) :-
  X @>= P,
  partitionQS(L, P, M, D).

quickSort(L, S) :- quickSort(L, [], S).
quickSort([], A, A).
quickSort([X | L], A, S) :-
    partitionQS(L, X, M, D),
    quickSort(M, [X | S1], S),
    quickSort(D, A, S1).

% cutSubstrings(+List, -Result)
% iff List is a sorted list and Result is List after removing duplications
cutSubstrings([], []).
cutSubstrings([X], [X]).
cutSubstrings([X, X | L], Result) :-
    cutSubstrings([X | L], Result).
cutSubstrings([X, Y | L], [X | Result]) :-
    X \= Y,
    cutSubstrings([Y | L], Result).

% deleteDuplicates(+List, -Result)
% iff Result is List after deleting duplicates (any order)
deleteDuplicates(List, Result) :-
    quickSort(List, Sorted),
    cutSubstrings(Sorted, Result).

% completeness(+Tree, +States, +E) iff every state in States has E edges (is complete)
completeness(_, [], _).
completeness(Tree, [S | L], E) :-
    findBST(Tree, S, T),
    sizeBST(T, E),
    completeness(Tree, L, E).

% replicate(+Times, -List)
replicate(0, []).
replicate(Times, [_ | List]) :-
    Times > 0,
    T1 is Times - 1,
    replicate(T1, List).

% isFinite(+LB, +UB, +BST, +Starting, +Terminals, +S)
% iff such automat generates finite language
isFinite(LB, UB, _, _, _, _) :-
    LB > UB.
isFinite(LB, UB, BST, Starting, Terminals, S) :-
    LB =< UB,
    replicate(LB, Word),
    \+ getWord(BST, Starting, Terminals, finite, S, Word),
    X is LB + 1,
    isFinite(X, UB, BST, Starting, Terminals, S).

% getLanguage(+LB, +UB, +BST, +Starting, +Terminals, +S, -Language)
% iff such automat accepts any word of length between LB and UB
getLanguage(LB, UB, BST, Starting, Terminals, S, finite) :-
    isFinite(LB, UB, BST, Starting, Terminals, S).
getLanguage(LB, UB, BST, Starting, Terminals, S, infinite) :-
    \+ isFinite(LB, UB, BST, Starting, Terminals, S).

% checkLanguage(+BST, +Starting, +Terminals, +LB, -Language)
% iff Language describes finitude of language generated by such automat 
checkLanguage(BST, Starting, Terminals, LB, Language) :-
    UB is LB * 2 - 1,
    getLanguage(LB, UB, BST, Starting, Terminals, UB, Language).

% listDiff(+Original, +ToDelete, -Result)
% iff Result is difference of Lists Original and ToDelete
listDiff([], _, []).
listDiff([X | O], [], [X | R]) :-
    listDiff(O, [], R).
listDiff([X | O], [X | D], R) :-
    listDiff(O, D, R).
listDiff([X | O], [Y | D], [X | R]) :-
    X \= Y,
    listDiff(O, [Y | D], R).

% product(+L1, +L2, -L) iff L is list cartesian products of elements from L1 and L2
product(L1, L2, L) :- product(L1, L1, L2, L).
product(_, _, [], []).
product(_, [], [_], []).
product(Orig, [], [_ | L2], L) :-
    L2 \= [],
    product(Orig, Orig, L2, L).
product(Orig, [X | L1], [Y | L2], [[X, Y] | L]) :-
    product(Orig, L1, [Y | L2], L).

% productM(+L1, +L2, -L) iff L is a list of moves where states are
% cartesian prodcuts of states which have same value on edge
productM(L1, L2, L) :- productM(L1, L1, L2, L).
productM(_, [], [], []).
productM(_, _, [], []).
productM(_, [], [_], []).
productM(Orig, [], [_ | L2], L) :-
    L2 \= [],
    productM(Orig, Orig, L2, L).
productM(Orig, [fp(A1, X, A2) | L1], [fp(B1, X, B2) | L2], [fp([A1, B1], X, [A2, B2]) | L]) :-
    productM(Orig, L1, [fp(B1, X, B2) | L2], L).
productM(Orig, [fp(_, X, _) | L1], [fp(B1, Y, B2) | L2], L) :-
    X \= Y,
    productM(Orig, L1, [fp(B1, Y, B2) | L2], L).

% complement(+Automat1, -Automat2) iff Automat2 is complement of Automat1
complement(dfa(Moves, Starting, Terminals), dfa(Moves, Starting, CTerminals)) :-
    correct(dfa(Moves, Starting, Terminals), _),
    getStatesAndEdges(Moves, StatesDup, _),
    deleteDuplicates(StatesDup, States),
    quickSort(States, SStates),
    quickSort(Terminals, STerminals),
    listDiff(SStates, STerminals, CTerminals).

% intersection(+Automat1, +Automat2, -Intersection)
% iff Intersection is intersection of Automat1 and Automat2
intersection(dfa(M1, S1, T1), dfa(M2, S2, T2), dfa(M, [S1, S2], T)) :-
    product(T1, T2, T),
    productM(M1, M2, M).


% correct(+Automat, -Representation)
correct(dfa(Moves, Starting, Terminals), repr(BST, Starting, Terminals, Language, S)) :-
    ground(Moves), \+var(Starting), ground(Terminals),
    quickSort(Terminals, SortedTerminals),
    deleteDuplicates(Terminals, SortedTerminals),
    makeBST(Moves, BST),
    getStatesAndEdges(Moves, States2, EdgesDup),
    append([Starting], Terminals, States1),
    append(States1, States2, StatesDup),
    deleteDuplicates(StatesDup, States),
    deleteDuplicates(EdgesDup, Edges),
    length(Edges, E),
    length(States, S),
    completeness(BST, States, E),
    checkLanguage(BST, Starting, Terminals, S, Language).

% accept(+Automat, ?Word)
accept(Automat, Word) :-
    correct(Automat, repr(BST, Starting, Terminals, Language, S)),
    getWord(BST, Starting, Terminals, Language, S, Word).


% empty(+Automat)
empty(Automat) :-
    correct(Automat, repr(BST, Starting, Terminals, _, _)),
    \+ anyPath(BST, Starting, Terminals).

% equal(+Automat1, +Automat2)
equal(Automat1, Automat2) :-
    subsetEq(Automat1, Automat2),
    subsetEq(Automat2, Automat1).

% subsetEq(+Automat1, +Automat2)
subsetEq(dfa(M1, S1, T1), Automat2) :-
    complement(Automat2, dfa(Mc, Sc, Tc)),
    getStatesAndEdges(M1, _, E1),
    getStatesAndEdges(Mc, _, Ec),
    deleteDuplicates(E1, Edges1),
    deleteDuplicates(Ec, Edgesc),
    quickSort(Edges1, Alphabet),
    quickSort(Edgesc, Alphabet),
    intersection(dfa(M1, S1, T1), dfa(Mc, Sc, Tc), Intersection),
    empty(Intersection).

/%
% example(IdentyfikatorAutomatu, Automat)
example(a11, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1)], 1, [2,1])).
example(a12, dfa([fp(x,a,y),fp(x,b,x),fp(y,a,x),fp(y,b,x)], x, [x,y])).
example(a2, dfa([fp(1,a,2),fp(2,b,1),fp(1,b,3),fp(2,a,3),
fp(3,b,3),fp(3,a,3)], 1, [1])).
example(a3, dfa([fp(0,a,1),fp(1,a,0)], 0, [0])).
example(a4, dfa([fp(x,a,y),fp(y,a,z),fp(z,a,x)], x, [x])).
example(a5, dfa([fp(x,a,y),fp(y,a,z),fp(z,a,zz),fp(zz,a,x)], x, [x])).
example(a6, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1)], 1, [])).
example(a7, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1),
fp(3,b,3),fp(3,a,3)], 1, [3])).
% bad ones
example(b1, dfa([fp(1,a,1),fp(1,a,1)], 1, [])).
example(b2, dfa([fp(1,a,1),fp(1,a,2)], 1, [])).
example(b3, dfa([fp(1,a,2)], 1, [])).
example(b4, dfa([fp(1,a,1)], 2, [])).
example(b5, dfa([fp(1,a,1)], 1, [1,2])).
example(b6, dfa([], [], [])).
example(d1, dfa([fp(1,a,3), fp(1, b, 2), fp(2, b, 3), fp(2, a, 4), fp(3, a, 4), fp(3, b, 5), fp(4, a, 5), fp(4, b, 5), fp(5, a, 5), fp(5, b, 5)], 1, [2,3,4])).
example(d2, dfa([fp(1,a,3), fp(1, b, 2), fp(2, b, 3), fp(2, a, 4), fp(3, a, 4), fp(3, b, 5), fp(4, a, 5), fp(4, b, 5), fp(5, a, 6), fp(5, b, 6), fp(6, a, 7), fp(6, b, 7), fp(7, a, 5), fp(7, b, 5)], 1, [2,3,4])).
example(c1, dfa([fp([x, 0], a, [y, 1]), fp([y, 0], a, [z, 1]), fp([z, 0], a, [x, 1]), fp([x, 1], a, [y, 0]), fp([y, 1], a, [z, 0]), fp([z, 1], a, [x, 0])], [x, 0], [[x, 1]])).
%/