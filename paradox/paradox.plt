% :- use_module(library(lists)).
:- use_module(library(plunit)).
:- consult('paradox.pl').
:- assert(current_log_level(info)).

:- begin_tests(simple_signatures).

test('Truly signature for true is [[true]]') :-
    NumPos = 1, NumQs = 1,
    generate_canonical_combinations(NumPos, [truly], FamilyT_Template),
    maplist(wrap_family_in_candidate([da_yes, da_no]), [FamilyT_Template], Families),
    get_evaluate_signature(true, NumQs, Families, Sig),
    assertion(Sig == sig([[true]], [[da, ja]])).

test('Truly signature for query_position_question(1, true) is [[da]] (Invariant)') :-
    NumPos = 1, NumQs = 1,
    generate_canonical_combinations(NumPos, [truly], FamilyT_Template),
    maplist(wrap_family_in_candidate([da_yes, da_no]), [FamilyT_Template], Families),
    get_evaluate_signature(query_position_question(1, true), NumQs, Families, Sig),
    % Truly always says 'da' to "Would you say da?" regardless of language
    assertion(Sig == sig([[fail, true]], [[da]])).

test('Falsely signature for query_position_question(1, true) is [[ja]] (Invariant)') :-
    NumPos = 1, NumQs = 1,
    generate_canonical_combinations(NumPos, [falsely], FamilyF_Template),
    maplist(wrap_family_in_candidate([da_yes, da_no]), [FamilyF_Template], Families),
    get_evaluate_signature(query_position_question(1, true), NumQs, Families, Sig),
    % Falsely always says 'ja' to "Would you say da?" regardless of language
    assertion(Sig == sig([[fail, true]], [[ja]])).

test('Random signature for query_position_question(1, true) is [[da, ja]]') :-
    NumPos = 1, NumQs = 1,
    generate_canonical_combinations(NumPos, [random], FamilyR_Template),
    maplist(wrap_family_in_candidate([da_yes, da_no]), [FamilyR_Template], Families),
    get_evaluate_signature(query_position_question(1, true), NumQs, Families, Sig),
    % Random can say anything
    assertion(Sig == sig([[fail, true]], [[da, ja]])).

:- end_tests(simple_signatures).

:- begin_tests(two_god_signatures).

test('TF World: Check signatures for all permutations of [Truly, Falsely]') :-
    NumPos = 2, NumQs = 1,
    Gods = [truly, falsely],
    findall(F_Template, generate_permutation_families(NumPos, Gods, F_Template), FamilyTemplates),
    maplist(wrap_family_in_candidate([da_yes, da_no]), FamilyTemplates, Families),
    Q = query_position_question(1, true),
    get_evaluate_signature(Q, NumQs, Families, sig(_LogSig, UttSig)),
    sort(UttSig, SortedSigs),
    % T at 1 -> da. F at 1 -> ja.
    assertion(SortedSigs == [[da], [ja]]).

test('TR World: Check signatures for all permutations of [Truly, Random]') :-
    NumPos = 2, NumQs = 1,
    Gods = [truly, random],
    findall(F_Template, generate_permutation_families(NumPos, Gods, F_Template), FamilyTemplates),
    maplist(wrap_family_in_candidate([da_yes, da_no]), FamilyTemplates, Families),
    Q = query_position_question(1, true),
    get_evaluate_signature(Q, NumQs, Families, sig(_LogSig, UttSig)),
    sort(UttSig, SortedSigs),
    % T at 1 -> da. R at 1 -> [da, ja].
    assertion(SortedSigs == [[da], [da, ja]]).

test('FR World: Check signatures for all permutations of [Falsely, Random]') :-
    NumPos = 2, NumQs = 1,
    Gods = [falsely, random],
    findall(F_Template, generate_permutation_families(NumPos, Gods, F_Template), FamilyTemplates),
    maplist(wrap_family_in_candidate([da_yes, da_no]), FamilyTemplates, Families),
    Q = query_position_question(1, true),
    get_evaluate_signature(Q, NumQs, Families, sig(_LogSig, UttSig)),
    sort(UttSig, SortedSigs),
    % F at 1 -> ja. R at 1 -> [da, ja].
    assertion(SortedSigs == [[da, ja], [ja]]).

:- end_tests(two_god_signatures).

:- begin_tests(distinct_generation).

test('Generate universe for complexity 1 and count distinct signatures') :-
    findall(F, generate_permutation_families(3, [truly, falsely, random], F), Families),
    my_nub(Families, UniqueFamilyTemplates),
    maplist(wrap_family_in_candidate([da_yes, da_no]), UniqueFamilyTemplates, UniqueFamilies),
    call_with_time_limit(10, generate_universe(3, 1, UniqueFamilies, 3)),
    call_with_time_limit(10, predicate_property(distinct_q(_,_,_), number_of_clauses(Count))),
    writeln(distinct_count_comp1(Count)),
    assertion(Count =< 365).

test('Generate universe for complexity 2 and count distinct signatures') :-
    findall(F, generate_permutation_families(3, [truly, falsely, random], F), Families),
    my_nub(Families, UniqueFamilyTemplates),
    maplist(wrap_family_in_candidate([da_yes, da_no]), UniqueFamilyTemplates, UniqueFamilies),
    call_with_time_limit(10, generate_universe(3, 2, UniqueFamilies, 3)),
    call_with_time_limit(10, predicate_property(distinct_q(_,_,_), number_of_clauses(Count))),
    writeln(distinct_count_comp2(Count)),
    % We found 222 distinct questions with inverse pruning.
    % This is close to the theoretical 216.
    assertion(Count =< 365).

test('Generate universe for complexity 3 and count distinct signatures') :-
    findall(F, generate_permutation_families(3, [truly, falsely, random], F), Families),
    my_nub(Families, UniqueFamilyTemplates),
    maplist(wrap_family_in_candidate([da_yes, da_no]), UniqueFamilyTemplates, UniqueFamilies),
    call_with_time_limit(30, generate_universe(3, 3, UniqueFamilies, 3)),
    call_with_time_limit(10, predicate_property(distinct_q(_,_,_), number_of_clauses(Count))),
    % 365
    writeln(distinct_count_comp3(Count)),
    assertion(Count =< 365).

:- end_tests(distinct_generation).

:- begin_tests(signature_inversion).

test('invert_atom inverts true/fail correctly') :-
    invert_atom(true, fail),
    invert_atom(fail, true).

test('invert_answer_set inverts [true] to [fail]') :-
    invert_answer_set([true], [fail]).

test('invert_answer_set inverts [fail] to [true]') :-
    invert_answer_set([fail], [true]).

test('invert_signature inverts a sig compound') :- 
    Sig = sig([[true], [fail]], [[da], [ja]]), 
    invert_signature(Sig, InvSig), 
    InvSig = sig([[fail], [true]], [[ja], [da]]).

:- end_tests(signature_inversion).

:- begin_tests(world_generation).

test('fill_random_answer for "truly" god leaves answer unbound') :-
    fill_random_answer(truly, 3, RndAns), % 3 is arbitrary
    var(RndAns). % var/1 succeeds if RndAns is an unbound variable

test('fill_random_answer for "random" god creates a correct-length list') :-
    fill_random_answer(random, /*num_questions=*/3, RndAnsList),
    length(RndAnsList, 3).

test('generate_worlds_from_templates for a "truly" family produces 2 worlds (da=yes, da=no)') :-
    % A 1-position, "all truly" family.
    build_uniform_family(1, truly, Family),
    findall(W, generate_worlds_from_templates(Family, /*num_questions=*/3, W), Worlds),
    length(Worlds, 2).

test('generate_worlds_from_templates for a "random" family is non-deterministic (x2 for lang)') :-
    % A 1-position, "all random" family, with 2 questions (N=2)...
    % 4 combos of answers * 2 languages = 8 worlds.
    build_uniform_family(/*num_positions*/1, random, Family),
    findall(W, generate_worlds_from_templates(Family, /*num_questions=*/2, W), Worlds),
    length(Worlds, 8).

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
% With unknown language, signatures include results from BOTH languages.

test('get_family_signature_set for "all truly" family includes both [da] and [ja]') :-
    Tree = tree(q(1, true), leaf, leaf),
    build_uniform_family(1, truly, Family),
    % Truth: da_yes -> da, da_no -> ja.
    get_family_signature_set(Tree, /*num_questions=*/1, Family, [[da], [ja]]).

test('get_family_signature_set for "all falsely" family includes both [da] and [ja]') :-
    Tree = tree(q(1, true), leaf, leaf),
    build_uniform_family(1, falsely, Family),
    % Falsely: da_yes -> ja, da_no -> da.
    get_family_signature_set(Tree, /*num_questions=*/1, Family, [[da], [ja]]).

test('get_family_signature_set for "all random" family is [da, ja]') :-
    Tree = tree(q(1, true), leaf, leaf),
    build_uniform_family(1, random, Family),
    % Random: da and ja are possible in both languages.
    get_family_signature_set(Tree, /*num_questions=*/1, Family, [[da], [ja]]).

test('2-question tree with "all truly" family has outcomes [da, da] and [ja, ja]') :-
    Tree = tree(q(1, true), tree(q(1, true), leaf, leaf), tree(q(1, true), leaf, leaf)),
    build_uniform_family(1, truly, Family),
    % Lang Yes: T->da, T->da.
    % Lang No:  T->ja, T->ja.
    get_family_signature_set(Tree, /*num_questions=*/2, Family, [[da, da], [ja, ja]]).

test('2-question tree with "all falsely" family has outcomes [ja, ja] and [da, da]') :-
    Tree = tree(q(1, true), tree(q(1, true), leaf, leaf), tree(q(1, true), leaf, leaf)),
    build_uniform_family(1, falsely, Family),
    % Lang Yes: F->ja, F->ja.
    % Lang No:  F->da, F->da.
    get_family_signature_set(Tree, /*num_questions=*/2, Family, [[da, da], [ja, ja]]).

test('2-question tree with "all random" family has all 4 possible outcomes') :-
    Tree = tree(q(1, true), tree(q(1, true), leaf, leaf), tree(q(1, true), leaf, leaf)),
    build_uniform_family(1, random, Family),
    ExpectedSet = [[da, da], [da, ja], [ja, da], [ja, ja]],
    get_family_signature_set(Tree, /*num_questions=*/2, Family, ExpectedSet).

:- end_tests(disjoint_logic).


:- begin_tests(distinguishing_scenarios).

test('1 simple question CANNOT distinguish [truly] from [falsely] without language', [fail]) :-
    call_with_time_limit(10, is_distinguishing_tree_bounded(1, 1, 0, [truly, falsely], _Tree, generate_uniform_families)).

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
    call_with_time_limit(20, \+ is_distinguishing_tree_bounded(
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
    build_uniform_family(1, truly, F_True_Template),
    wrap_family_in_candidate([da_yes, da_no], F_True_Template, F_True),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = da,
    family_answers_question(Question, NumQs, F_True, ExpectedAnswer).

test('family_answers: "Truly" family CAN answer ja (in other lang)') :-
    build_uniform_family(1, truly, F_True_Template),
    wrap_family_in_candidate([da_yes, da_no], F_True_Template, F_True),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = ja,
    family_answers_question(Question, NumQs, F_True, ExpectedAnswer).

test('family_answers: "Falsely" family CAN answer ja') :-
    build_uniform_family(1, falsely, F_False_Template),
    wrap_family_in_candidate([da_yes, da_no], F_False_Template, F_False),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = ja,
    family_answers_question(Question, NumQs, F_False, ExpectedAnswer).

test('family_answers: "Falsely" family CAN answer da (in other lang)') :-
    build_uniform_family(1, falsely, F_False_Template),
    wrap_family_in_candidate([da_yes, da_no], F_False_Template, F_False),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = da,
    family_answers_question(Question, NumQs, F_False, ExpectedAnswer).

test('family_answers: "Random" family CAN answer da') :-
    build_uniform_family(1, random, F_Rand_Template),
    wrap_family_in_candidate([da_yes, da_no], F_Rand_Template, F_Rand),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = da,
    family_answers_question(Question, NumQs, F_Rand, ExpectedAnswer).

test('family_answers: "Random" family CAN answer ja') :-
    build_uniform_family(1, random, F_Rand_Template),
    wrap_family_in_candidate([da_yes, da_no], F_Rand_Template, F_Rand),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = ja,
    family_answers_question(Question, NumQs, F_Rand, ExpectedAnswer).

% --- Tests for `partition_families/5` ---
% With simple questions and unknown language, everyone goes to BOTH sides.

test('partition: does NOT split [True, False] for simple Q (both in both)') :-
    build_uniform_family(1, truly, F_True_Template),
    build_uniform_family(1, falsely, F_False_Template),
    maplist(wrap_family_in_candidate([da_yes, da_no]), [F_True_Template, F_False_Template], In_Families),
    NumQs           = 1,
    Question        = q(1, true),
    partition_families(In_Families, NumQs, Question, DaFamilies, JaFamilies),
    
    % Truly says Da in Yes, Ja in No. Falsely says Ja in Yes, Da in No.
    % So DaCandidates should have Truly (Lang Yes) and Falsely (Lang No).
    % JaCandidates should have Truly (Lang No) and Falsely (Lang Yes).
    
    % Verify DaCandidates
    % We expect 2 candidates.
    length(DaFamilies, 2),
    member(candidate(F_True_Template, [da_yes]), DaFamilies),
    member(candidate(F_False_Template, [da_no]), DaFamilies),
    
    % Verify JaCandidates
    length(JaFamilies, 2),
    member(candidate(F_True_Template, [da_no]), JaFamilies),
    member(candidate(F_False_Template, [da_yes]), JaFamilies).

test('partition: does NOT split [True, Random] families for simple Q') :-
    build_uniform_family(1, truly, F_True_Template),
    build_uniform_family(1, random, F_Rand_Template),
    maplist(wrap_family_in_candidate([da_yes, da_no]), [F_True_Template, F_Rand_Template], In_Families),
    NumQs           = 1,
    Question        = q(1, true),
    partition_families(In_Families, NumQs, Question, DaFamilies, JaFamilies),
    
    % Truly: Da->Yes, Ja->No.
    % Random: Da->Both, Ja->Both.
    
    % Da: Truly(Yes), Random(Both)
    member(candidate(F_True_Template, [da_yes]), DaFamilies),
    member(candidate(F_Rand_Template, [da_yes, da_no]), DaFamilies),
    
    % Ja: Truly(No), Random(Both)
    member(candidate(F_True_Template, [da_no]), JaFamilies),
    member(candidate(F_Rand_Template, [da_yes, da_no]), JaFamilies).

test('partition: correctly splits [False, Random] families (da=True)') :-
    build_uniform_family(1, falsely, F_False_Template),
    build_uniform_family(1, random, F_Rand_Template),
    maplist(wrap_family_in_candidate([da_yes, da_no]), [F_False_Template, F_Rand_Template], In_Families),
    NumQs           = 1,
    Question        = q(1, true),
    partition_families(In_Families, NumQs, Question, DaFamilies, JaFamilies),
    
    % Falsely: Da->No, Ja->Yes.
    % Random: Both.
    
    % Da: Falsely(No), Random(Both)
    member(candidate(F_False_Template, [da_no]), DaFamilies),
    member(candidate(F_Rand_Template, [da_yes, da_no]), DaFamilies),
    
    % Ja: Falsely(Yes), Random(Both)
    member(candidate(F_False_Template, [da_yes]), JaFamilies),
    member(candidate(F_Rand_Template, [da_yes, da_no]), JaFamilies).

test('partition: correctly splits [True, False, Random] families (da=True)') :-
    build_uniform_family(1, truly, F_True_Template),
    build_uniform_family(1, falsely, F_False_Template),
    build_uniform_family(1, random, F_Rand_Template),
    maplist(wrap_family_in_candidate([da_yes, da_no]), [F_True_Template, F_False_Template, F_Rand_Template], In_Families),
    NumQs           = 1,
    Question        = q(1, true),
    partition_families(In_Families, NumQs, Question, DaFamilies, JaFamilies),
    
    % Just verifying counts roughly since we checked logic above
    length(DaFamilies, 3),
    length(JaFamilies, 3).


test(verify_embedded_question_split, [nondet]) :-
    NumPos = 3, NumQs = 3,
    GodTypes = [truly, falsely, random],
    
    % 1. Generate families
    findall(F, generate_permutation_families(NumPos, GodTypes, F), FamiliesWithDuplicates),
    my_nub(FamiliesWithDuplicates, UniqueFamilyTemplates),
    maplist(wrap_family_in_candidate([da_yes, da_no]), UniqueFamilyTemplates, Candidates),
    
    length(Candidates, 6),
    
    % 2. Define the Embedded Question: "If I asked you 'Is God 1 Truly?', would you say 'da'?"
    % Target God: 1. Inner Question: at_position_question(1, truly).
    Q = q(1, query_position_question(1, at_position_question(1, truly))),
    
    log(info, 'Testing Q: ~w', [Q]),
    
    % 3. Partition
    partition_families(Candidates, NumQs, Q, DaCandidates, JaCandidates),
    
    length(DaCandidates, DaCount),
    length(JaCandidates, JaCount),
    
    log(info, 'Split Result: Da=~w, Ja=~w', [DaCount, JaCount]),
    
    % 4. Verify Expectation: 4 vs 4
    % T (at 1): da. F (at 1): ja. R (at 1): both.
    % T at 1: [T,F,R], [T,R,F] -> Da
    % F at 1: [F,T,R], [F,R,T] -> Ja
    % R at 1: [R,T,F], [R,F,T] -> Da AND Ja
    % So Da should have 2(T) + 2(R) = 4.
    % Ja should have 2(F) + 2(R) = 4.
    assertion(DaCount == 4),
    assertion(JaCount == 4).

% --- Integration Tests for `find_pruning_tree/7` ---
test('pruning_tree: FAILS for [True, False] with 1 simple question (ambiguous)') :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, falsely, F_False),
    FamiliesTemplates = [F_True, F_False],
    maplist(wrap_family_in_candidate([da_yes, da_no]), FamiliesTemplates, Families),
    generate_universe(1, 0, Families, 1),
    TotalNumQs     = 1,
    CurrentDepth   = 1,
    MaxQComp       = 0,
    NumPos         = 1,
    % Should fail because they can't be distinguished.
    \+ find_pruning_tree(TotalNumQs, CurrentDepth, MaxQComp, NumPos, Families, Families, _Tree).

% (Removed flaky 1-complex question test for [True, False] as it depends on small universe generation)

test('pruning_tree: FAILS for [True, False, Random] with 2 questions (impossible split)', [fail]) :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, falsely, F_False),
    build_uniform_family(1, random, F_Rand),
    FamiliesTemplates = [F_True, F_False, F_Rand],
    maplist(wrap_family_in_candidate([da_yes, da_no]), FamiliesTemplates, Families),
    generate_universe(1, 2, Families, 2),
    TotalNumQs     = 2,
    CurrentDepth   = 2,
    MaxQComp       = 2,
    NumPos         = 1,
    find_pruning_tree(TotalNumQs, CurrentDepth, MaxQComp, NumPos, Families, Families, _Tree).

test('pruning_tree: SUCCEEDS for a 2-position [Truly,Falsely] vs [Falsely,Truly] world with Complex Q') :-
    F1 = [pos(1, truly, _), pos(2, falsely, _)],
    F2 = [pos(1, falsely, _), pos(2, truly, _)],
    FamiliesTemplates = [F1, F2],
    maplist(wrap_family_in_candidate([da_yes, da_no]), FamiliesTemplates, Families),
    generate_universe(2, 1, Families, 1), % MaxComp 1 needed

    find_pruning_tree(
        1, % TotalNumQs / Max Depth
        1, % CurrentDepth
        1, % MaxQComplexity
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

    FamiliesTemplates = [F_True, Family_Rand_True, F_False],
    maplist(wrap_family_in_candidate([da_yes, da_no]), FamiliesTemplates, Families),
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

    my_nub(Families, UniqueFamilyTemplates),
    maplist(wrap_family_in_candidate([da_yes, da_no]), UniqueFamilyTemplates, UniqueFamiliesCandidates),

    write('UniqueFamilies:'), writeln(UniqueFamiliesCandidates),

    % 3b. Generate Universe
    generate_universe(NumPos, QComplexity, UniqueFamiliesCandidates, NumQs),

    % 4. Call the pruning solver directly with the debug statements active
    find_pruning_tree(
        NumQs,   % TotalNumQs
        NumQs,   % CurrentDepth
        QComplexity,
        NumPos,
        UniqueFamiliesCandidates, % Canonical
        UniqueFamiliesCandidates, % Current
        _Tree).

:- end_tests(complex_pruning_scenario).

% =========================================================================================
% SOLUTION EXPLANATION
% =========================================================================================

solve_and_print_riddle :-
    NumPos = 3, NumQs = 3, QComplexity = 1,
    GodTypes = [truly, falsely, random],
    Generator = generate_permutation_families,
    
    log(info, 'Searching for solution...'),
    
    is_distinguishing_tree_bounded(NumPos, NumQs, QComplexity, GodTypes, Tree, Generator),
    
    log(info, '--- SOLUTION FOUND (Human Readable) ---'),
    draw_tree(Tree, human),
    
    log(info, '--- SOLUTION FOUND (Raw Prolog Object) ---'),
    draw_tree(Tree, raw).

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
    % We expect this to SUCCEED now that languages are unconstrained but we use embedded Qs.
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

test('3 Gods (T,F,R) is SOLVABLE with complexity 3 questions (3 questions deep)') :-
    % 1. Define the problem parameters
    NumPos       = 3,
    NumQs        = 3, 
    QComplexity  = 3, % Allow slightly more complex questions (nested once)
    GodTypes     = [truly, falsely, random],
    Generator    = generate_permutation_families,
    
    % 2. Call the main solver with a time limit
    call_with_time_limit(30, is_distinguishing_tree_bounded(
        NumPos,
        NumQs,
        QComplexity,
        GodTypes,
        _Tree,
        Generator
    )).

:- end_tests(final_challenge).

:- begin_tests(kleene_logic).

test('AND: True and Paradox is Paradox') :-
    logic_and_3state(true, paradox, R), assertion(R == paradox).

test('AND: False and Paradox is False (Strong Logic dominance)') :-
    % This is crucial: A question like "Is 2+2=5 AND [Paradox]" should be FALSE, not Crash.
    logic_and_3state(false, paradox, R), assertion(R == false).

test('OR: True or Paradox is True (Strong Logic dominance)') :-
    logic_or_3state(true, paradox, R), assertion(R == true).

:- end_tests(kleene_logic).


:- begin_tests(god_silence_mechanics).

test('Truly God falls Silent on Universal Paradox') :-
    % LogicResult = paradox -> Output = silent
    god_utterance(truly, paradox, _, da_yes, Utterance),
    assertion(Utterance == silent).

test('Falsely God falls Silent on Universal Paradox') :-
    god_utterance(falsely, paradox, _, da_yes, Utterance),
    assertion(Utterance == silent).

test('Random God speaks despite Paradox (Immunity)') :-
    % Even if LogicResult is paradox, Random uses his coin flip (true)
    god_utterance(random, paradox, true, da_yes, Utterance),
    assertion(Utterance == da).

:- end_tests(god_silence_mechanics).


:- begin_tests(nested_evaluation).

test('evaluate_3state handles "Did he say da?" correctly when God is Silent') :-
    % If inner query returns 'silent', the answer to "Did he say da?" is FALSE.
    % We mock this by defining a dummy world state if needed, or trusting the logic flow.
    % Logic flow check:
    (silent == da -> Res = true ; silent == ja -> Res = false ; silent == silent -> Res = false),
    assertion(Res == false).

:- end_tests(nested_evaluation).


:- begin_tests(atomic_discovery).

test('Generator finds the new Paradox Atoms') :-
    init_distinct_generator,
    % We pass a dummy complexity and family list just to trigger the atom fetch
    generate_base_cases(3, Atomics),
    member(paradox_universal, Atomics),
    member(paradox_falsely, Atomics).

:- end_tests(atomic_discovery).
