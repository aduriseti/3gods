:- use_module(library(plunit)).
:- use_module(library(lists)).

% nub(+List, -Nub)
% Removes duplicate elements from List, keeping the first occurrence of each.
% Provided because it may not be available in all Prolog environments.
nub(List, Nub) :-
    nub_acc(List, [], Nub).

nub_acc([], _, []).
nub_acc([H | T], Seen, Result) :-
    ( memberchk(H, Seen) ->
        nub_acc(T, Seen, Result)
    ;
        Result = [H | NewResult],
        nub_acc(T, [H | Seen], NewResult)
    ).

% numlist(+L, +U, -List)
% Generates a list of integers from L to U.
% Provided because it may not be available in all Prolog environments.
numlist(L, U, []) :- L > U.
numlist(L, U, [L | T]) :-
    L =< U,
    L1 is L + 1,
    numlist(L1, U, T).

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
generate_permutation_families(N, GodTypes, Family) :-
         permutation(GodTypes, PermutedGods),
         numlist(1, N, Positions),
         is_random_answer(RandomAns), % Non-deterministically picks true or false
         build_family(Positions, PermutedGods, RandomAns, Family).
     
     build_family([], [], _, []).
     build_family([Pos | RestPos], [God | RestGods], RandomAns, [pos(Pos, God, RndAnsList) | RestFamily]) :-
         ( God == random -> RndAnsList = [RandomAns] ; RndAnsList = [] ),
        build_family(RestPos, RestGods, RandomAns, RestFamily).

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

% A question is "golden" if it perfectly separates the unique subproblem families.
is_golden(Q, UniqueSub1, UniqueSub2) :-
    forall(member(Family, UniqueSub1), query_position(1, Q, [], Family)),
    forall(member(Family, UniqueSub2), \+ query_position(1, Q, [], Family)).

% A question is "valid" if it doesn't contradict any known answers for the unique families.
is_valid(Q, UniqueSub1, UniqueSub2) :-
    \+ (member(F, UniqueSub1), \+ query_position(1, Q, [], F)), % No false negatives
    \+ (member(F, UniqueSub2), query_position(1, Q, [], F)).   % No false positives

% Defines the set of atomic questions to seed the search.
base_question(at_position_question(Pos, God)) :-
    is_position(3, Pos),
    is_god(God).

% Defines how to expand a question into more complex ones.
expand_q(Q1, (Q1, Q2)) :- base_question(Q2).
expand_q(Q1, (Q2, Q1)) :- base_question(Q2).
expand_q(Q1, (Q1 ; Q2)) :- base_question(Q2).
expand_q(Q1, (Q2 ; Q1)) :- base_question(Q2).
expand_q(Q, query_question(Pos, Q)) :- is_position(3, Pos).

% Calculates the heuristic score of a question.
score_question(Q, UniqueSub1, UniqueSub2, Score) :-
    findall(F, (member(F, UniqueSub1), query_position(1, Q, [], F)), Correct1),
    findall(F, (member(F, UniqueSub2), \+ query_position(1, Q, [], F)), Correct2),
    length(Correct1, Score1),
    length(Correct2, Score2),
    Score is Score1 + Score2.

% Helper to score and filter a list of new questions.
score_and_filter_new_qs([], _, _, _, []).
score_and_filter_new_qs([Q | Rest], Sub1, Sub2, Visited, [score(Score, Q) | ScoredRest]) :-
    \+ memberchk(Q, Visited),
    is_valid(Q, Sub1, Sub2),
    !,
    score_question(Q, Sub1, Sub2, Score),
    score_and_filter_new_qs(Rest, Sub1, Sub2, Visited, ScoredRest).
score_and_filter_new_qs([_ | Rest], Sub1, Sub2, Visited, ScoredRest) :-
    score_and_filter_new_qs(Rest, Sub1, Sub2, Visited, ScoredRest).

% Best-first heuristic search loop.
heuristic_search([score(_, Q) | _], _, Sub1, Sub2, Q) :-
    is_golden(Q, Sub1, Sub2), !.
heuristic_search([score(_, Q) | Frontier], Visited, Sub1, Sub2, GoldenQ) :-
    findall(NewQ, expand_q(Q, NewQ), ExpandedQs),
    score_and_filter_new_qs(ExpandedQs, Sub1, Sub2, [Q | Visited], NewScoredQs),
    append(Frontier, NewScoredQs, UnsortedFrontier),
    sort(0, @>=, UnsortedFrontier, NextFrontier),
    heuristic_search(NextFrontier, [Q | Visited], Sub1, Sub2, GoldenQ).

find_golden_question(GoldenQuestion) :-
    % 1. Load the problem definitions.
    generate_full_problem(_),
    generate_subproblem_1(SubProblem1),
    generate_subproblem_2(SubProblem2),

    % 2. Exclude families present in both subproblems.
    findall(F, (member(F, SubProblem1), \+ memberchk(F, SubProblem2)), OnlyInSub1),
    findall(F, (member(F, SubProblem2), \+ memberchk(F, SubProblem1)), OnlyInSub2),

    % 3. Seed the search with the initial set of scored and filtered base questions.
    findall(Q, base_question(Q), BaseQs),
    score_and_filter_new_qs(BaseQs, OnlyInSub1, OnlyInSub2, [], InitialFrontier),
    sort(0, @>=, InitialFrontier, SortedInitialFrontier),
    
    % 4. Launch the heuristic search.
    heuristic_search(SortedInitialFrontier, [], OnlyInSub1, OnlyInSub2, GoldenQuestion).

:- begin_tests(golden_question_debug).

test(is_at_pos_3_truly_golden, [fail]) :-
    % This test asserts that at_position_question(3, truly) is NOT a golden question.
    % If this test itself fails, it means the system incorrectly believes it IS golden.
    generate_full_problem(_),
    generate_subproblem_1(SubProblem1),
    generate_subproblem_2(SubProblem2),
    findall(F, (member(F, SubProblem1), \+ memberchk(F, SubProblem2)), OnlyInSub1),
    findall(F, (member(F, SubProblem2), \+ memberchk(F, SubProblem1)), OnlyInSub2),

    % Sanity check: ensure the family lists are not empty, which would cause a vacuous success.
    OnlyInSub1 \= [],
    OnlyInSub2 \= [],

    Q = at_position_question(3, truly),
    is_golden(Q, OnlyInSub1, OnlyInSub2).

:- end_tests(golden_question_debug).