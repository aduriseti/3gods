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

    % 2. PRUNED SEARCH: Iterate through questions, from simplest to most complex.
    between(0, MaxQComp, C),
    is_position(3, Pos),
    is_question(3, C, Q),
    CandidateQ = q(Pos, Q),

    % 3. Partition the full problem based on the candidate question.
    partition_families(FullProblem, CandidateQ, YesGroup, NoGroup),

    % 4. Check if this partition is "golden".
    %    It succeeds if the partitions match our target sub-problems (in any order).
    permutation([SubProblem1, SubProblem2], [YesGroup, NoGroup]),
    !, % Cut! We found the first (and therefore simplest) golden question.
    GoldenQuestion = CandidateQ.


% --- Helpers for the Solver ---
partition_families(Families, q(Pos, Q), YesGroup, NoGroup) :-
    % A question MUST be unambiguous for all families to be a valid partitioner.
    \+ ( member(F, Families), is_ambiguous(q(Pos, Q), F) ),
    % If it's not ambiguous, partition the families.
    findall(F, (member(F, Families), answers_unambiguously(q(Pos, Q), F, true)), YesGroup),
    findall(F, (member(F, Families), answers_unambiguously(q(Pos, Q), F, false)), NoGroup).

is_ambiguous(Question, Family) :-
    get_single_question_signature(Question, 1, Family, [false,true]).

answers_unambiguously(Question, Family, Answer) :-
    get_single_question_signature(Question, 1, Family, [Answer]).

get_single_question_signature(q(Pos, Q), NumQs, Family, SignatureSet) :-
    findall(Ans,
            (   generate_worlds_from_templates(Family, NumQs, W),
                ( query_position(Pos, Q, [], W) -> Ans=true ; Ans=false )
            ),
            Answers),
    sort(Answers, SignatureSet).


% --- Standard Helpers (World Generation, nub, etc.) ---
build_family_template([], [], []).
build_family_template([Pos|Ps], [God|Gs], [pos(Pos, God, _)|T]) :- build_family_template(Ps, Gs, T).
generate_permutation_families(NumPos, GodTypes, FamilyTemplate) :-
    findall(P, between(1, NumPos, P), Positions),
    permutation(GodTypes, PermutedGods),
    build_family_template(Positions, PermutedGods, FamilyTemplate).
fill_random_answer(random, NumQs, RndAnsList) :- length(RndAnsList, NumQs), maplist(is_random_answer, RndAnsList).
fill_random_answer(God, _, _) :- dif(God, random).
generate_worlds_from_templates([], _, []).
generate_worlds_from_templates([pos(P, God, _)|T], NumQs, [pos(P, God, RndAns)|W]) :-
    fill_random_answer(God, NumQs, RndAns),
    generate_worlds_from_templates(T, NumQs, W).
nub([], []).
nub([H|T], [H|T2]) :- \+ member(H, T), nub(T, T2).
nub([H|T], T2) :- member(H, T), nub(T, T2).

% --- The Missing Partition Helper ---

partition_families_by_signature(Families, NumQs, QuestionNode, YesOnly, NoOnly, Both) :-
    % For each family, get its full set of possible answers to this question.
    maplist(get_single_question_signature(QuestionNode, NumQs), Families, Signatures),

    % Group the families based on their answer sets (signatures).
    findall(F, (nth1(I, Families, F), nth1(I, Signatures, [true])),        YesOnly),
    findall(F, (nth1(I, Families, F), nth1(I, Signatures, [false])),       NoOnly),
    findall(F, (nth1(I, Families, F), nth1(I, Signatures, [false,true])),  Both).


:- begin_tests(golden_question_debug).

test('analyze the partition created by the "golden question"') :-
    % 1. Setup (parameters, question, families - this is all the same)
    NumPos = 3,
    NumQs = 3,
    GodTypes = [truly, falsely, random],
    GoldenQ = ( (at_position_question(1, truly), at_position_question(2, random))
              ; (at_position_question(2, truly), at_position_question(3, random)) ),
    QuestionNode = q(1, GoldenQ),
    findall(F, generate_permutation_families(NumPos, GodTypes, F), All6Families),
    
    % 2. Partition logic (this is the same)
    partition_families_by_signature(All6Families, NumQs, QuestionNode, YesOnly, NoOnly, Both),

    % --- The Corrected Debug Prints ---
    % First, get the lengths and bind them to variables.
    length(YesOnly, YesCount),
    length(NoOnly, NoCount),
    length(Both, BothCount),
    
    % Now, print the values of those variables.
    writeln(yes_only_count: YesCount),
    writeln(no_only_count: NoCount),
    writeln(both_count: BothCount),

    % --- Assertions (these are the same) ---
    assertion(length(YesOnly, 2)),
    assertion(length(NoOnly, 2)),
    assertion(length(Both, 2)).

:- end_tests(golden_question_debug).