:- use_module(library(plunit)).
:- use_module(library(lists)).

% --- System Parameters ---
god_types([truly, falsely, random]).

% --- Component Generators ---
is_position(N, P) :- between(1, N, P).
is_random_answer(true).
is_random_answer(false).
is_god(God) :- god_types(Gods), member(God, Gods).

% --- Core Query Logic (State-Aware) ---
query(truly, Q, Path, WS, _) :- evaluate(Q, Path, WS).
query(falsely, Q, Path, WS, _) :- \+ evaluate(Q, Path, WS).
query(random, _Q, Path, _WS, RndAnsList) :-
    length(Path, NumPreviousAnswers),
    CurrentQuestionNum is NumPreviousAnswers + 1,
    nth1(CurrentQuestionNum, RndAnsList, Answer),
    Answer.

query_position(Pos, Q, Path, WS) :-
    member(pos(Pos, GodType, RndAnsList), WS),
    query(GodType, Q, Path, WS, RndAnsList).

at_position(Pos, God, WS) :- member(pos(Pos, God, _), WS).

% --- The Evaluator ---
:- table evaluate/3.
evaluate(true, _, _) :- true.
evaluate(false, _, _) :- false.
evaluate(at_position_question(P, G), _, WS) :- at_position(P, G, WS).
evaluate(query_question(Pos, SubQ), Path, WS) :- query_position(Pos, SubQ, Path, WS).
% --- Add these clauses to your evaluate/3 predicate ---

% The meaning of (Q1, Q2) is that both Q1 and Q2 must be true.
evaluate((Q1, Q2), Path, WorldState) :-
    evaluate(Q1, Path, WorldState),
    evaluate(Q2, Path, WorldState).

% The meaning of (Q1 ; Q2) is that either Q1 or Q2 must be true.
evaluate((Q1 ; Q2), Path, WorldState) :-
    ( evaluate(Q1, Path, WorldState) ; evaluate(Q2, Path, WorldState) ).

% --- The Grammar ---

% --- 1. The Bounded Question Grammar ---
is_question(_, _, true).
is_question(_, _, false).
is_question(NumPos, _, at_position_question(Pos, God)) :- is_position(NumPos, Pos), is_god(God).
is_question(NumPos, MaxQDepth, query_question(Pos, Q)) :-
    MaxQDepth > 0,
    NextQDepth is MaxQDepth - 1,
    is_position(NumPos, Pos),
    is_question(NumPos, NextQDepth, Q).
is_question(NumPos, MaxQDepth, (Q1, Q2)) :-
    MaxQDepth > 0,
    NextQDepth is MaxQDepth - 1,
    is_question(NumPos, NextQDepth, Q1),
    is_question(NumPos, NextQDepth, Q2).

is_question(NumPos, MaxQDepth, (Q1 ; Q2)) :-
    MaxQDepth > 0,
    NextQDepth is MaxQDepth - 1,
    is_question(NumPos, NextQDepth, Q1),
    is_question(NumPos, NextQDepth, Q2).


% --- 2. The Hardcoded World Generators ---
generate_full_problem(Families) :-
    findall(F, generate_permutation_families(3, [truly,falsely,random], F), RawFamilies),
    nub(RawFamilies, Families).

generate_subproblem_1(Families) :- % Random restricted to positions 1 & 2
    findall(F, (
        generate_permutation_families(3, [truly,falsely,random], F),
        member(pos(3,random,_), F), !, fail ; true
    ), RawFamilies),
    nub(RawFamilies, Families).

generate_subproblem_2(Families) :- % Random restricted to positions 1 & 3
    findall(F, (
        generate_permutation_families(3, [truly,falsely,random], F),
        member(pos(2,random,_), F), !, fail ; true
    ), RawFamilies),
    nub(RawFamilies, Families).

% --- 3. The "Golden Question" Solver ---
find_golden_question(MaxQComp, GoldenQuestion) :-
    % 1. Load the problem definitions.
    generate_full_problem(FullProblem),
    generate_subproblem_1(SubProblem1),
    generate_subproblem_2(SubProblem2),

    % 2. need to expand question grammar intelligently
    % we want to find a question Q posed to position P s.t. 
    % 1. for families only belonging to subproblem 1 the answer given by god at position 1 is YES
    % 2. for families only belonging to subproblem 2 the answer given by god at position 1 is NO
    % 3. for families belonging to both subproblem 1 & 2 - no contraint on answer given by god at position 1

    % 
    % target golden question is "is the truly god directly adjacent and to the left of random god"
    % 
    % GoldenQ = ( (at_position_question(1, truly), at_position_question(2, random))
    %           ; (at_position_question(2, truly), at_position_question(3, random)) ),
    %
    % when considering how to build grammar - either by composition of questions or by asking how a god at a position would respond to a question
    % lets represent such transformations to questions as an arbitrary function f 
    % only apply f to question Q if it gets us closer to our contraints in 1 & 2
    % TODO ( for gemini)
