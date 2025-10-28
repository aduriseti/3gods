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

% --- The Corrected, Ambiguity-Aware Scoring Predicate ---

% --- New Helper for Scoring ---

% --- The Final, Corrected Scoring Predicate ---

score_partition(CandidateQ, Score) :-
    % 1. Load problem definitions
    % generate_full_problem(FullProblem),
    generate_subproblem_1(SubProblem1),
    generate_subproblem_2(SubProblem2),

    % 2. Calculate the key sets: unique parts and the overlap.
    sort(SubProblem1, S1),
    sort(SubProblem2, S2),
    ord_subtract(S1, S2, UniqueToSub1),
    ord_subtract(S2, S1, UniqueToSub2),

    % 3. Score the two possible mappings by counting misplaced unique families.
    
    % Score for the mapping where YesGroup should contain UniqueToSub1
    count_misplaced_uniques(CandidateQ, UniqueToSub1, UniqueToSub2, Score1),

    % Score for the mapping where YesGroup should contain UniqueToSub2
    count_misplaced_uniques(CandidateQ, UniqueToSub2, UniqueToSub1, Score2),

    % 4. The final score is the best possible mapping.
    Score is min(Score1, Score2).


% --- Helper Predicates for the New Scorer ---

% count_misplaced_uniques(Question, TargetYesSet, TargetNoSet, Count)
% Counts how many unique families are unambiguously forced into the wrong partition.
count_misplaced_uniques(Question, TargetYesSet, TargetNoSet, Count) :-
    % Find all errors in the "Yes" group
    findall(F,
            ( member(F, TargetYesSet),
              get_single_question_signature(Question, 1, F, Signature),
              Signature \= [true] % Error if it's not ONLY true
            ),
            YesErrors),
    % Find all errors in the "No" group
    findall(F,
            ( member(F, TargetNoSet),
              get_single_question_signature(Question, 1, F, Signature),
              Signature \= [false] % Error if it's not ONLY false
            ),
            NoErrors),
    length(YesErrors, C1),
    length(NoErrors, C2),
    Count is C1 + C2.

% transform_question(Q_in, Q_out)
% Generates a more complex question Q_out from a simpler Q_in.

% Rule 1: Wrap an existing question in a "question about a question".
transform_question(Q_in, query_question(Pos, Q_in)) :-
    is_position(3, Pos).

% Rule 2: Add a simple clause with AND.
transform_question(Q_in, (Q_in, BaseQ)) :-
    is_question(3, 0, BaseQ), % Get a simple complexity-0 question
    Q_in @> BaseQ. % Enforce canonical order to reduce duplicates.

% Rule 3: Add a simple clause with OR.
transform_question(Q_in, (Q_in ; BaseQ)) :-
    is_question(3, 0, BaseQ),
    Q_in @> BaseQ.


% find_golden_question_heuristic(MaxComplexity, GoldenQuestion)
find_golden_question_heuristic(MaxComplexity, GoldenQuestion) :-
    % 1. Generate ONE complexity-0 question at a time to use as a starting point.
    is_position(3, P),
    is_question(3, 0, Q),
    BaseQ = q(P, Q),

    % 2. Score this starting question.
    score_partition(BaseQ, BaseScore),

    % 3. Run the full "hill-climbing" search starting from this specific base question.
    improve_question_heuristic(MaxComplexity, 0, BaseScore, BaseQ, ResultingQuestion),

    % 4. Check if the final question from this climb is a perfect solution.
    score_partition(ResultingQuestion, FinalScore),
    FinalScore = 0,

    % 5. If it is, we've found our solution. Commit to it and stop.
    GoldenQuestion = ResultingQuestion,
    !.

% improve_question_heuristic(MaxComp, CurrentComp, BestScoreSoFar, BestQSoFar, FinalQ)
improve_question_heuristic(_MaxComp, _CurrentComp, BestScore, BestQ, BestQ) :-
    % Base Case 1: If we found a perfect partition (score=0), stop.
    BestScore = 0, !.
improve_question_heuristic(MaxComp, CurrentComp, _BestScore, BestQ, BestQ) :-
    % Base Case 2: If we've hit the complexity limit, stop.
    CurrentComp >= MaxComp, !.

improve_question_heuristic(MaxComp, CurrentComp, BestScoreSoFar, BestQSoFar, FinalQ) :-
    % 1. Generate all possible "next move" questions from the current best.
    findall(NextQ, transform_question(BestQSoFar, NextQ), NextSteps),
    
    % 2. Score all the new candidate questions.
    findall(Score-Q, (member(Q, NextSteps), score_partition(Q, Score)), ScoredSteps),
    
    % 3. Find the best of these new options.
    keysort(ScoredSteps, [BestNewScore-BestNewQ | _]),
    
    % 4. Decide whether to continue.
    (   BestNewScore < BestScoreSoFar
    ->  % It's an improvement! Continue the search from this new, better question.
        NextComp is CurrentComp + 1,
        improve_question_heuristic(MaxComp, NextComp, BestNewScore, BestNewQ, FinalQ)
    ;   % No improvement was found. The previous question was the best we could do.
        FinalQ = BestQSoFar
    ).


% build_and_filter_layer(MaxComp, CurrentComp, PromisingQs_PrevLevel, FinalQ, FinalScore)

% Base Case 1: A perfect solution (score=0) was found in the previous layer. Stop.
build_and_filter_layer(_, _, [0-BestQSoFar | _], BestQSoFar, 0) :- !.

% Base Case 2: We've reached the maximum complexity limit. Return the best we've found so far.
build_and_filter_layer(MaxComp, CurrentComp, [BestScore-BestQ | _], BestQ, BestScore) :-
    CurrentComp > MaxComp, !.

% Recursive Step: Build and filter the next layer of questions.
build_and_filter_layer(MaxComp, CurrentComp, PromisingQs_PrevLevel, FinalQ, FinalScore) :-
    % 1. Generate all new candidate compositions from the previous layer's promising questions.
    findall(NewQ,
            (   % Get a question from the previous "best of" list
                member(_Score-Q1, PromisingQs_PrevLevel),
                % Transform it into a more complex question
                transform_question(Q1, NewQ)
            ),
            NewCandidates),
    
    % 2. Score all the new candidates.
    score_all_questions(NewCandidates, ScoredNewQs),
    
    % 3. Filter for improvement: Keep only new questions that are better than the best of the last layer.
    PromisingQs_PrevLevel = [BestPrevScore-_ | _],
    findall(Score-Q,
            (   member(Score-Q, ScoredNewQs),
                Score < BestPrevScore
            ),
            ImprovedNewQs),

    % 4. Combine and find the new "best of" list for this level.
    append(PromisingQs_PrevLevel, ImprovedNewQs, AllCandidatesForThisLevel),
    sort(AllCandidatesForThisLevel, PromisingQs_CurrentLevel), % sort/2 on pairs sorts by key (score)

    % 5. Recurse to the next level of complexity.
    NextComp is CurrentComp + 1,
    build_and_filter_layer(MaxComp, NextComp, PromisingQs_CurrentLevel, FinalQ, FinalScore).

% Helper to score a list of questions.
score_all_questions(Questions, ScoredQuestions) :-
    findall(Score-Q, (member(Q, Questions), score_partition(Q, Score)), ScoredQuestions).


% find_golden_question_layered(MaxComplexity, GoldenQuestion, FinalScore)
find_golden_question_layered(MaxComplexity, GoldenQuestion, FinalScore) :-
    % 1. Level 0: Find all base questions, score them, and sort them best-first.
    findall(q(P, Q), (is_position(3, P), is_question(3, 0, Q)), BaseQuestions),
    score_all_questions(BaseQuestions, ScoredBaseQs),
    sort(ScoredBaseQs, BestFirstBaseQs), % sort/2 on pairs sorts by key (score)

    % 2. Start the recursive search. The helper will do all the work of finding the
    %    best question it can within the complexity limit.
    build_and_filter_layer(MaxComplexity, 1, BestFirstBaseQs, GoldenQuestion, FinalScore).

% build_portfolio(MaxComp, CurrentComp, CurrentPortfolio, FinalQ, FinalScore)

% Base Case 1: The best question in our portfolio is a perfect solution. Stop.
build_portfolio(_, _, [0-BestQ | _], BestQ, 0) :- !.

% Base Case 2: We've hit the complexity limit. Return the best question we have.
build_portfolio(MaxComp, CurrentComp, [BestScore-BestQ | _], BestQ, BestScore) :-
    CurrentComp > MaxComp, !.

% Recursive Step: Expand, score, and update the portfolio.
build_portfolio(MaxComp, CurrentComp, CurrentPortfolio, FinalQ, FinalScore) :-
    % 1. Get the questions (not scores) from the current portfolio to use as building blocks.
    pairs_values(CurrentPortfolio, PromisingQs),

    % 2. Generate all new candidate compositions from the current portfolio.
    findall(NewQ,
            (   member(Q_in, PromisingQs),
                transform_question(Q_in, NewQ)
            ),
            NewCandidates),
    
    % 3. Score all the new candidates.
    score_all_questions(NewCandidates, ScoredNewQs),

    % 4. Create the next-level portfolio by merging the old and new,
    %    keeping only the best of everything.
    append(CurrentPortfolio, ScoredNewQs, AllCandidates),
    sort(AllCandidates, NextPortfolio), % sort/2 automatically deduplicates and keeps the best scores

    % 5. Recurse to the next level of complexity with the new, improved portfolio.
    NextComp is CurrentComp + 1,
    build_portfolio(MaxComp, NextComp, NextPortfolio, FinalQ, FinalScore).

% find_golden_question_portfolio(MaxComplexity, GoldenQuestion, FinalScore)
find_golden_question_portfolio(MaxComplexity, GoldenQuestion, FinalScore) :-
    % 1. Level 0: Find ALL base questions and their scores to create the initial portfolio.
    findall(q(P, Q), (is_position(3, P), is_question(3, 0, Q)), BaseQuestions),
    score_all_questions(BaseQuestions, ScoredBaseQs),
    sort(ScoredBaseQs, InitialPortfolio), % sort/2 on pairs sorts by score

    % 2. Start the recursive, portfolio-building search.
    build_portfolio(MaxComplexity, 1, InitialPortfolio, GoldenQuestion, FinalScore).

% evolve_questions(MaxComp, CurrentComp, GenePool, FinalBestQ)

% Base Case: We've hit the max complexity. Return the best question from our current gene pool.
evolve_questions(MaxComp, CurrentComp, [_-BestQ | _], BestQ) :-
    CurrentComp > MaxComp, !.

% Recursive Step: Evolve the next generation of questions.
evolve_questions(MaxComp, CurrentComp, GenePool, FinalBestQ) :-
    % 1. Generate all possible "offspring" from the current gene pool.
    %    An offspring is a new question composed from a "parent" in the gene pool.
    findall(OffspringScore-OffspringQ,
            (   % a) Select a "parent" from the current gene pool.
                member(ParentScore-ParentQ, GenePool),
                % b) Create a new, more complex "offspring" from it.
                transform_question(ParentQ, OffspringQ),
                % c) Score the new offspring.
                score_partition(OffspringQ, OffspringScore),
                % d) THE CRITICAL FILTER: Keep only offspring that are a strict improvement.
                OffspringScore < ParentScore
            ),
            ImprovedOffspring),

    % 2. Check if any improvements were found.
    (   ImprovedOffspring == []
    ->  % No offspring were better than their parents. The search has peaked.
        % The best question is the best one we already have from the current gene pool.
        GenePool = [_-FinalBestQ | _]
    ;   % Improvements were found! Create the next generation's gene pool.
        append(GenePool, ImprovedOffspring, NewGenePool),
        sort(NewGenePool, NextGenerationGenePool), % sort/2 keeps the best-scoring unique questions
        NextComp is CurrentComp + 1,
        evolve_questions(MaxComp, NextComp, NextGenerationGenePool, FinalBestQ)
    ).

% find_improving_question(MaxComplexity, BestQuestion)
find_improving_question(MaxComplexity, BestQuestion) :-
    % 1. Level 0: Find all base questions, score them, and keep only the useful ones.
    findall(q(P, Q), (is_position(3, P), is_question(3, 0, Q)), BaseQuestions),
    score_all_questions(BaseQuestions, ScoredBaseQs),
    
    % 2. Start the recursive, evolutionary search from complexity level 1.
    evolve_questions(MaxComplexity, 1, ScoredBaseQs, BestQuestion).

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

:- begin_tests(golden_question_evolution_trace).

% --- Test Setup: Define the problem context ---
% :- generate_full_problem(FullProblem). % Generate the 6 families of the T,F,R problem

% --- Test 1: Verify the scores of the Level 1 "building block" questions ---
test('evolution_step_0: Level 1 components have non-perfect scores') :-
    % We need to confirm that the simple questions are not already "perfect" (score=0),
    % which would stop the search prematurely. We also need their scores to use as a baseline.
    Q_T1_R2 = q(1, (at_position_question(1, truly), at_position_question(2, random))),
    score_partition(Q_T1_R2, Score_T1_R2),
    
    Q_T2_R3 = q(1, (at_position_question(2, truly), at_position_question(3, random))),
    score_partition(Q_T2_R3, Score_T2_R3),

    % We expect these scores to be > 0.
    assertion(Score_T1_R2 > 0),
    assertion(Score_T2_R3 > 0).

% --- Test 2: Verify the jump from Level 1 to Level 2 ---
test('evolution_step_1: The AND composition is a strict improvement') :-
    % Let's check if (T@1 AND R@2) is an improvement over just R@2.
    
    % The "parent" question (Level 1)
    ParentQ = q(1, at_position_question(2, random)),
    score_partition(ParentQ, ParentScore),

    % The "offspring" question (Level 2)
    OffspringQ = q(1, (at_position_question(1, truly), at_position_question(2, random))),
    score_partition(OffspringQ, OffspringScore),

    % The critical filter: The offspring MUST be a strict improvement.
    assertion(OffspringScore < ParentScore).

% --- Test 3: Verify the jump from Level 2 to Level 3 ---
test('evolution_step_2: The final OR composition is a strict improvement') :-
    % Let's check if the full GoldenQ is an improvement over its main component.
    
    % The "parent" question (Level 2)
    ParentQ = q(1, (at_position_question(1, truly), at_position_question(2, random))),
    score_partition(ParentQ, ParentScore),
    
    % The "offspring" question (Level 3 - The Golden Question)
    GoldenQ_Term = ( (at_position_question(1, truly), at_position_question(2, random))
                   ; (at_position_question(2, truly), at_position_question(3, random)) ),
    OffspringQ = q(1, GoldenQ_Term),
    score_partition(OffspringQ, OffspringScore),

    % The critical filter: The final composition MUST be a strict improvement.
    assertion(OffspringScore < ParentScore).

:- end_tests(golden_question_evolution_trace).