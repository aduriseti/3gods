% :- use_module(library(lists)).
:- use_module(library(plunit)).
:- consult('8b.pl').
:- assert(allowed_languages([da_yes])).

:- begin_tests(distinct_generation).

test('Generate universe for complexity 1 and count distinct signatures') :-
    findall(F, generate_permutation_families(3, [truly, falsely, random], F), Families),
    my_nub(Families, UniqueFamilies),
    call_with_time_limit(10, generate_universe(3, 1, UniqueFamilies, 3)),
    call_with_time_limit(10, predicate_property(distinct_q(_,_,_), number_of_clauses(Count))),
    writeln(distinct_count_comp1(Count)),
    assertion(Count =< 729).

test('Generate universe for complexity 2 and count distinct signatures') :-
    findall(F, generate_permutation_families(3, [truly, falsely, random], F), Families),
    my_nub(Families, UniqueFamilies),
    call_with_time_limit(10, generate_universe(3, 2, UniqueFamilies, 3)),
    call_with_time_limit(10, predicate_property(distinct_q(_,_,_), number_of_clauses(Count))),
    writeln(distinct_count_comp2(Count)),
    % We found 222 distinct questions with inverse pruning.
    % This is close to the theoretical 216.
    assertion(Count =< 729).

test('Generate universe for complexity 3 and count distinct signatures') :-
    findall(F, generate_permutation_families(3, [truly, falsely, random], F), Families),
    my_nub(Families, UniqueFamilies),
    call_with_time_limit(60, generate_universe(3, 3, UniqueFamilies, 3)),
    call_with_time_limit(60, predicate_property(distinct_q(_,_,_), number_of_clauses(Count))),
    % 365
    writeln(distinct_count_comp3(Count)),
    assertion(Count =< 729).

:- end_tests(distinct_generation).

:- begin_tests(signature_inversion).

test('invert_atom inverts true/fail correctly') :-
    invert_atom(true, fail),
    invert_atom(fail, true).

test('invert_answer_set inverts [true] to [fail]') :-
    invert_answer_set([true], [fail]).

test('invert_answer_set inverts [fail] to [true]') :-
    invert_answer_set([fail], [true]).

test('invert_answer_set inverts [fail, true] to [fail, true]') :-
    invert_answer_set([fail, true], Inv),
    sort(Inv, Sorted),
    Sorted = [fail, true].

test('invert_signature inverts a list of answer sets') :-
    Sig = [[true], [fail], [fail, true]],
    invert_signature(Sig, InvSig),
    InvSig = [[fail], [true], [fail, true]].

:- end_tests(signature_inversion).

:- begin_tests(world_generation).

test('fill_random_answer for "truly" god leaves answer unbound') :-
    fill_random_answer(truly, 3, RndAns), % 3 is arbitrary
    var(RndAns). % var/1 succeeds if RndAns is an unbound variable

test('fill_random_answer for "random" god creates a correct-length list') :-
    fill_random_answer(random, /*num_questions=*/3, RndAnsList),
    length(RndAnsList, 3).

test('generate_worlds_from_templates for a "truly" family is deterministic') :-
    % A 1-position, "all truly" family should only have 1 corresponding world.
    build_uniform_family(1, truly, Family),
    findall(W, generate_worlds_from_templates(Family, /*num_questions=*/3, W), Worlds),
    length(Worlds, 1).

test('generate_worlds_from_templates for a "random" family is non-deterministic') :-
    % A 1-position, "all random" family, with 2 questions (N=2)...
    build_uniform_family(/*num_positions*/1, random, Family),
    % ...should generate 2^2 = 4 unique worlds.
    findall(W, generate_worlds_from_templates(Family, /*num_questions=*/2, W), Worlds),
    length(Worlds, 4).

test('permutation generator creates N! families') :-
    god_types(Gods), length(Gods, 3),
    % 3 positions, 3 god types = 3! = 6 permutations.
    findall(F, generate_permutation_families(/*num_positions=*/3, Gods, F), Families),
    length(Families, 6).

test('uniform generator creates N families') :-
    god_types(Gods), length(Gods, 3),
    % 3 god types = 3 uniform families ([T,T,T], [F,F,F], [R,R,R]).
    findall(F, generate_uniform_families(/*num_positions=*/3, Gods, F), Families),
    length(Families, 3).

:- end_tests(world_generation).

:- begin_tests(disjoint_logic).

% --- Low-Level Sanity Checks for Set Logic ---
test('disjoint/2 succeeds for non-overlapping sets') :- disjoint([a, b], [c, d]).
test('disjoint/2 fails for overlapping sets', [fail]) :- disjoint([a, b], [b, c]).
test('all_disjoint/1 succeeds for a list of disjoint sets') :- all_disjoint([[a], [b, c], [d]]).
test('all_disjoint/1 fails if any two sets overlap', [fail]) :- all_disjoint([[a, b], [c, d], [b, e]]).

% --- Integration Test for Signature Set Generation ---
test('get_family_signature_set for "all truly" family is [da] (da=True)') :-
    Tree = tree(q(1, true), leaf, leaf),
    build_uniform_family(1, truly, Family),
    get_family_signature_set(Tree, /*num_questions=*/1, Family, [[da]]).

test('get_family_signature_set for "all falsely" family is [ja] (da=True)') :-
    Tree = tree(q(1, true), leaf, leaf),
    build_uniform_family(1, falsely, Family),
    get_family_signature_set(Tree, /*num_questions=*/1, Family, [[ja]]).

test('get_family_signature_set for "all random" family is [da, ja]') :-
    Tree = tree(q(1, true), leaf, leaf),
    build_uniform_family(1, random, Family),
    get_family_signature_set(Tree, /*num_questions=*/1, Family, [[da], [ja]]).

test('2-question tree with "all truly" family has one outcome: [da, da]') :-
    Tree = tree(q(1, true), tree(q(1, true), leaf, leaf), tree(q(1, true), leaf, leaf)),
    build_uniform_family(1, truly, Family),
    get_family_signature_set(Tree, /*num_questions=*/2, Family, [[da, da]]).

test('2-question tree with "all falsely" family has one outcome: [ja, ja]') :-
    Tree = tree(q(1, true), tree(q(1, true), leaf, leaf), tree(q(1, true), leaf, leaf)),
    build_uniform_family(1, falsely, Family),
    get_family_signature_set(Tree, /*num_questions=*/2, Family, [[ja, ja]]).

test('2-question tree with "all random" family has all 4 possible outcomes') :-
    Tree = tree(q(1, true), tree(q(1, true), leaf, leaf), tree(q(1, true), leaf, leaf)),
    build_uniform_family(1, random, Family),
    ExpectedSet = [[da, da], [da, ja], [ja, da], [ja, ja]],
    get_family_signature_set(Tree, /*num_questions=*/2, Family, ExpectedSet).

:- end_tests(disjoint_logic).


:- begin_tests(distinguishing_scenarios).

test('1 question CAN distinguish [truly] from [falsely]') :-
    call_with_time_limit(10, is_distinguishing_tree_bounded(1, 1, 1, [truly, falsely], _Tree, generate_uniform_families)).

test('truly with 2 positions is distinguishable by default even with 0 questions]') :-
    call_with_time_limit(10, is_distinguishing_tree_bounded(
           2, % Num Positions
           0, % Tree Depth (Num Questions)
           0, % Max Question Complexity
           [truly, truly], % The multiset of gods for the combination
           _Tree,
           generate_canonical_combinations % Use the NEW generator
       )).

test('Exhaustive search proves [truly] vs [random] is indistinguishable for 1 Q^1 question') :-
    % We are asserting that the following goal MUST FAIL.
    % The '\+' operator succeeds if its argument fails completely.
    call_with_time_limit(10, \+ is_distinguishing_tree_bounded(
           1, % Num Positions
           1, % Tree Depth (Num Questions)
           1, % Max Question Complexity
           [truly, random],
           _Tree,
           generate_uniform_families
       )).

% W/ addition of xor rule - this test now too expensive to run.
test('Exhaustive search proves [truly] vs [random] is indistinguishable for 3 Q^3 question') :-
    % We are asserting that the following goal MUST FAIL.
    % The '\+' operator succeeds if its argument fails completely.
    call_with_time_limit(10, \+ is_distinguishing_tree_bounded(
           1, % Num Positions
           3, % Tree Depth (Num Questions)
           3, % Max Question Complexity
           [truly, random],
           _Tree,
           generate_uniform_families
       )).

test('Exhaustive search proves [truly,falsely,random] are indistinguishable for 1 Q^1 question') :-
    % We are asserting that the following goal MUST FAIL.
    % The '\+' operator succeeds if its argument fails completely.
    call_with_time_limit(10, \+ is_distinguishing_tree_bounded(
           3, % Num Positions
           1, % Tree Depth (Num Questions)
           1, % Max Question Complexity (e.g., nesting one level deep)
           [truly, falsely, random],
           _Tree,
           generate_permutation_families
       )).

:- end_tests(distinguishing_scenarios).

:- begin_tests(pruning_logic).

% --- Tests for `family_answers_question/4` ---
test('family_answers: "Truly" family CAN answer da') :-
    build_uniform_family(1, truly, F_True),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = da,
    family_answers_question(Question, NumQs, F_True, ExpectedAnswer).

test('family_answers: "Truly" family CANNOT answer ja', [fail]) :-
    build_uniform_family(1, truly, F_True),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = ja,
    family_answers_question(Question, NumQs, F_True, ExpectedAnswer).

test('family_answers: "Falsely" family CAN answer ja') :-
    build_uniform_family(1, falsely, F_False),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = ja,
    family_answers_question(Question, NumQs, F_False, ExpectedAnswer).

test('family_answers: "Falsely" family CANNOT answer da', [fail]) :-
    build_uniform_family(1, falsely, F_False),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = da,
    family_answers_question(Question, NumQs, F_False, ExpectedAnswer).

test('family_answers: "Random" family CAN answer da') :-
    build_uniform_family(1, random, F_Rand),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = da,
    family_answers_question(Question, NumQs, F_Rand, ExpectedAnswer).

test('family_answers: "Random" family CAN answer ja') :-
    build_uniform_family(1, random, F_Rand),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = ja,
    family_answers_question(Question, NumQs, F_Rand, ExpectedAnswer).

% --- Tests for `partition_families/5` ---
test('partition: correctly splits [True, False] families (da=True)') :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, falsely, F_False),
    In_Families     = [F_True, F_False],
    NumQs           = 1,
    Question        = q(1, true),
    partition_families(In_Families, NumQs, Question, DaFamilies, JaFamilies),
    assertion(DaFamilies = [F_True]),
    assertion(JaFamilies = [F_False]).

test('partition: correctly splits [True, Random] families (da=True)') :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, random, F_Rand),
    In_Families     = [F_True, F_Rand],
    NumQs           = 1,
    Question        = q(1, true),
    partition_families(In_Families, NumQs, Question, DaFamilies, JaFamilies),
    assertion(DaFamilies = [F_True, F_Rand]),
    assertion(JaFamilies = [F_Rand]).

test('partition: correctly splits [False, Random] families (da=True)') :-
    build_uniform_family(1, falsely, F_False),
    build_uniform_family(1, random, F_Rand),
    In_Families     = [F_False, F_Rand],
    NumQs           = 1,
    Question        = q(1, true),
    partition_families(In_Families, NumQs, Question, DaFamilies, JaFamilies),
    assertion(DaFamilies = [F_Rand]),
    assertion(JaFamilies = [F_False, F_Rand]).

test('partition: correctly splits [True, False, Random] families (da=True)') :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, falsely, F_False),
    build_uniform_family(1, random, F_Rand),
    In_Families     = [F_True, F_False, F_Rand],
    NumQs           = 1,
    Question        = q(1, true),
    partition_families(In_Families, NumQs, Question, DaFamilies, JaFamilies),
    sort(DaFamilies, [F_Rand, F_True]),
    sort(JaFamilies, SortedNo),
    sort([F_False, F_Rand], ExpectedNo),
    assertion(SortedNo = ExpectedNo).

% --- Integration Tests for `find_pruning_tree/7` ---
test('pruning_tree: SUCCEEDS for [True, False] with 1 simple question (da=True)') :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, falsely, F_False),
    Families = [F_True, F_False],
    generate_universe(1, 0, Families, 1), % Complexity 0 OK for da=True
    TotalNumQs     = 1,
    CurrentDepth   = 1,
    MaxQComp       = 0,
    NumPos         = 1,
    find_pruning_tree(TotalNumQs, CurrentDepth, MaxQComp, NumPos, Families, Families, _Tree).

test('pruning_tree: FAILS for [True, Random] with 1 question', [fail]) :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, random, F_Rand),
    Families = [F_True, F_Rand],
    generate_universe(1, 1, Families, 1),
    TotalNumQs     = 1,
    CurrentDepth   = 1,
    MaxQComp       = 1,
    NumPos         = 1,
    find_pruning_tree(TotalNumQs, CurrentDepth, MaxQComp, NumPos, Families, Families, _Tree).

test('pruning_tree: FAILS for [True, False, Random] with 2 questions (impossible split)', [fail]) :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, falsely, F_False),
    build_uniform_family(1, random, F_Rand),
    Families = [F_True, F_False, F_Rand],
    generate_universe(1, 2, Families, 2),
    TotalNumQs     = 2,
    CurrentDepth   = 2,
    MaxQComp       = 2,
    NumPos         = 1,
    find_pruning_tree(TotalNumQs, CurrentDepth, MaxQComp, NumPos, Families, Families, _Tree).

test('pruning_tree: SUCCEEDS for a 2-position [Truly,Falsely] vs [Falsely,Truly] world') :-
    % This is a more complex scenario with 2 positions.
    % We need to distinguish two families:
    % Family 1: pos 1 is True, pos 2 is False
    % Family 2: pos 1 is False, pos 2 is True
    F1 = [pos(1, truly, _), pos(2, falsely, _)],
    F2 = [pos(1, falsely, _), pos(2, truly, _)],
    Families = [F1, F2],
    generate_universe(2, 0, Families, 1), % MaxComp 0 is enough (simple questions)
    
    % We check if a 1-question tree with complexity 0 can find a solution.
    % (The question "true" asked at position 1 will work)
    find_pruning_tree(
        1, % TotalNumQs / Max Depth
        1, % CurrentDepth
        0, % MaxQComplexity
        2, % NumPos
        Families, % Canonical
        Families, % Current
        _Tree).

:- end_tests(pruning_logic).

:- begin_tests(pigeonhole_prune_check).

test('pruning_tree: FAILS because sub-problem is too large for remaining depth', [fail]) :-
    % --- 1. Setup: Create a specific, tricky scenario ---
    
    % We have 1 position and 1 question (TotalNumQs = 1)
    NumPos = 1,
    TotalNumQs = 1,
    
    % We have 3 families to distinguish:
    % F1: The god is Truly.
    % F2: The god is Random, and its one answer is 'true'.
    % F3: The god is Falsely.
    build_uniform_family(1, truly, F_True),
    % generate_worlds_from_templates([pos(1, random, _)], 1, [World_Rand_True]),
    Family_Rand_True = [pos(1, random, [true])], % We use the concrete world as the family
    build_uniform_family(1, falsely, F_False),
    
    Families = [F_True, Family_Rand_True, F_False],
    generate_universe(1, 0, Families, 1),

    % --- 2. The Test: Call the solver ---
    % We ask it to solve this 3-family problem with only 1 question.
    % The algorithm *should* fail because of Pruning Check 2.
    
    find_pruning_tree(
        TotalNumQs,   % TotalNumQs = 1
        TotalNumQs,   % CurrentDepth = 1
        0,            % MaxQComplexity = 0 (simple questions only)
        NumPos,       % NumPos = 1
        Families,     % Canonical
        Families,     % Current
        _Tree).

% --- 3. Trace of Why This Fails (as a comment) ---
%
%   - Depth = 1. NextDepth = 0. MaxSize = 2^0 = 1.
%   - Solver tries the question Q = 'true'.
%   - Partition:
%       - F_True answers 'true'.
%       - Family_Rand_True answers 'true'.
%       - F_False answers 'fail'.
%   - Partition result: YesFamilies = [F_True, Family_Rand_True], NoFamilies = [F_False].
%
%   - Pruning Check 1 (Useless Split):
%       - The split is not useless. It passes.
%
%   - Pruning Check 2 (Sub-Problem Too Large):
%       - YesSize = 2. NoSize = 1. MaxSize = 1.
%       - The check is: ( (YesSize > MaxSize) ; (NoSize > MaxSize) )
%       - ( (2 > 1) ; (1 > 1) )  ->  ( true ; false )  ->  true.
%   - The predicate correctly executes '!, fail'. The search for this branch is pruned.
%
%   - The solver backtracks, tries other questions, finds no valid split, and ultimately fails.
%   - The test is marked [fail], so this failure is the expected, correct outcome.
%
:- end_tests(pigeonhole_prune_check).


:- begin_tests(complex_pruning_scenario).

test('pruning_tree: FAILS for a [T,T,F] problem with only 1 question', [fail]) :-
    % 1. Define the problem parameters
    NumPos       = 3,
    NumQs        = 1, % Not enough questions to solve 3 families (2^1 < 3)
    QComplexity  = 1,
    GodTypes     = [truly, truly, falsely],
    Generator    = generate_permutation_families,
    
    % 2. Call the main solver
    % This MUST fail. The initial problem size (3 families) is larger than
    % the max solvable size for a 1-question tree (2^1 = 2).
    % Our pruning logic should catch this.
    is_distinguishing_tree_bounded(
        NumPos,
        NumQs,
        QComplexity,
        GodTypes,
        _Tree,
        Generator
    ).

test('pruning_tree: SUCCEEDS for a [T,T,F] problem with 2 questions') :-
    % 1. Define the problem parameters
    NumPos       = 3,
    NumQs        = 2, % 2 questions IS enough (2^2 > 3)
    QComplexity  = 1,
    GodTypes     = [truly, truly, falsely],
    Generator    = generate_permutation_families,
    
    % 2. Call the main solver
    % We expect this to succeed by finding a 2-step tree.
    is_distinguishing_tree_bounded(
        NumPos,
        NumQs,
        QComplexity,
        GodTypes,
        _Tree,
        Generator
    ).

% --- Add this new test to your complex_pruning_scenario suite ---

test('DEBUG TRACE for the [T,T,F] problem') :-
    % 1. Define the exact problem parameters
    NumPos       = 3,
    NumQs        = 2, % Tree Depth
    QComplexity  = 1,
    GodTypes     = [truly, truly, falsely],
    Generator    = generate_permutation_families,

    % 2. Generate the families for this specific test
    findall(F, call(Generator, NumPos, GodTypes, F), Families),

    % 3. Print the initial state so we can see it
    writeln('\n--- STARTING DEBUG TRACE FOR TTF SCENARIO ---'),
    write('Families to distinguish (count='), length(Families, L), writeln(L),
    write('Families: '), writeln(Families),

    my_nub(Families, UniqueFamilies),
        % nub(FamiliesWithDuplicates, UniqueFamilies),

    write('UniqueFamilies:'), writeln(UniqueFamilies),

    % 3b. Generate Universe
    generate_universe(NumPos, QComplexity, UniqueFamilies, NumQs),

    % 4. Call the pruning solver directly with the debug statements active
    find_pruning_tree(
        NumQs,   % TotalNumQs
        NumQs,   % CurrentDepth
        QComplexity,
        NumPos,
        UniqueFamilies, % Canonical
        UniqueFamilies, % Current
        _Tree).

% (Duplicate visualization code removed)


:- end_tests(complex_pruning_scenario).

% =========================================================================================
% SOLUTION EXPLANATION
% =========================================================================================
% The solver successfully found a strategy to distinguish the 3 Gods (Truly, Falsely, Random)
% in exactly 3 questions. Here is the translation of the Prolog solution tree:
%
% -----------------------------------------------------------------------------------------
% QUESTION 1: Ask God 1: "If I asked you 'Is God 2 Random?', would you say 'yes'?"
% -----------------------------------------------------------------------------------------
% Logic:
% - This is a "nested" question (Complexity 1).
% - If God 1 is NOT Random (i.e., Truth or Liar), this structure forces a truthful answer 
%   about the inner question ("Is God 2 Random?").
%   - If Answer is YES: God 2 IS Random. (So God 3 is NOT Random).
%   - If Answer is NO:  God 2 is NOT Random. (So God 2 is Truth or Liar).
% - If God 1 IS Random: The answer is random nonsense (Yes or No).
%
% DECISION SPLIT:
%
% === BRANCH A: Answer is YES ===
% Implications:
% - Either God 2 is Random (and God 1 told us so reliably), OR God 1 is Random (and lied/randomly said Yes).
% - In EITHER case, we know for a fact that **God 3 is NOT Random**.
%   - If G1=NotRandom -> G2=Random -> G3=NotRandom.
%   - If G1=Random -> G3=NotRandom.
% Strategy: Since God 3 is reliable (Truth or Liar), we use him to identify everyone else.
%
% Q2 (Ask God 3): "Is 1 == 1?" (A trivial check to see if God 3 is Truth or Liar)
%   - If YES: God 3 is Truth.
%     Q3 (Ask God 3): "Is God 1 False?" -> Solves everything.
%   - If NO: God 3 is Liar.
%     Q3 (Ask God 3): "Is God 1 True?" (He will lie) -> Solves everything.
%
% === BRANCH B: Answer is NO ===
% Implications:
% - Either God 2 is NOT Random (and God 1 told us so reliably), OR God 1 is Random.
% - In EITHER case, we know for a fact that **God 2 is NOT Random**.
% Strategy: Since God 2 is reliable, we use him.
%
% Q2 (Ask God 2): "Is 1 == 1?"
%   - Same logic as above, but using God 2 as the oracle.
%
% =========================================================================================

:- begin_tests(final_challenge).

test('PRINT SOLUTION for 3 Gods (T,F,R)') :-
    call_with_time_limit(10, solve_and_print_riddle).

test('3 Gods (T,F,R) is IMPOSSIBLE with complexity 0 questions (simple direct questions)', [fail]) :-
    % 1. Define the problem parameters
    NumPos       = 3,
    NumQs        = 3, 
    QComplexity  = 0, % STRICTLY simple questions only (no "If I asked you...")
    GodTypes     = [truly, falsely, random],
    Generator    = generate_permutation_families,
    
    % 2. Call the main solver
    % We expect this to FAIL. Without nested questions, we cannot bypass the 
    % Truth/Liar ambiguity or reliably identify Random in 3 steps.
    call_with_time_limit(10, is_distinguishing_tree_bounded(
        NumPos,
        NumQs,
        QComplexity,
        GodTypes,
        _Tree,
        Generator
    )).

test('3 Gods (T,F,R) is SOLVABLE with complexity 1 questions (3 questions deep)') :-
    % 1. Define the problem parameters
    NumPos       = 3,
    NumQs        = 3, 
    QComplexity  = 1, % Limit to simple questions
    GodTypes     = [truly, falsely, random],
    Generator    = generate_permutation_families,
    
    % 2. Call the main solver
    % We expect this to SUCCEED. Complexity 1 allows "If I asked you X..."
    call_with_time_limit(10, is_distinguishing_tree_bounded(
        NumPos,
        NumQs,
        QComplexity,
        GodTypes,
        _Tree,
        Generator
    )).

test('3 Gods (T,F,R) is IMPOSSIBLE with only 2 questions (tree depth 2)', [fail]) :-
    % 1. Define the problem parameters
    NumPos       = 3,
    NumQs        = 2, % Not enough questions!
    QComplexity  = 1, 
    GodTypes     = [truly, falsely, random],
    Generator    = generate_permutation_families,
    
    % 2. Call the main solver
    call_with_time_limit(10, is_distinguishing_tree_bounded(
        NumPos,
        NumQs,
        QComplexity,
        GodTypes,
        _Tree,
        Generator
    )).

test('3 Gods (T,F,R) is SOLVABLE with complexity 2 questions (3 questions deep)') :-
    % 1. Define the problem parameters
    NumPos       = 3,
    NumQs        = 3, 
    QComplexity  = 2, % Allow slightly more complex questions (nested once)
    GodTypes     = [truly, falsely, random],
    Generator    = generate_permutation_families,
    
    % 2. Call the main solver with a time limit
    call_with_time_limit(10, is_distinguishing_tree_bounded(
        NumPos,
        NumQs,
        QComplexity,
        GodTypes,
        _Tree,
        Generator
    )).

% test('3 Gods (T,F,R) is SOLVABLE with complexity 2 questions (3 questions deep)') :-
%     % 1. Define the problem parameters
%     NumPos       = 3,
%     NumQs        = 3, 
%     QComplexity  = 3, % Allow slightly more complex questions (nested once)
%     GodTypes     = [truly, falsely, random],
%     Generator    = generate_permutation_families,
    
%     % 2. Call the main solver with a time limit
%     call_with_time_limit(10, is_distinguishing_tree_bounded(
%         NumPos,
%         NumQs,
%         QComplexity,
%         GodTypes,
%         _Tree,
%         Generator
%     )).

:- end_tests(final_challenge).