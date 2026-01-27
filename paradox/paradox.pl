:- use_module(library(lists)).

iamrunningthelatestversion.

:- use_module(paradox_utils).

% --- Logging Infrastructure ---
:- dynamic current_log_level/1.
current_log_level(info).

level_value(debug, 10).
level_value(info, 20).
level_value(warning, 30).
level_value(error, 40).
level_value(fatal, 50).

should_log(MsgLevel) :-
    current_log_level(CurLevel),
    level_value(MsgLevel, MsgVal),
    level_value(CurLevel, CurVal),
    MsgVal >= CurVal.

log(Level, Message) :-
    (   should_log(Level)
    ->  upcase_atom(Level, UpperLevel),
        format('~w: ~w~n', [UpperLevel, Message])
    ;   true
    ).

log(Level, Format, Args) :-
    (   should_log(Level)
    ->  format(string(Msg), Format, Args),
        upcase_atom(Level, UpperLevel),
        format('~w: ~s~n', [UpperLevel, Msg])
    ;   true
    ).

% --- The World ---

% --- Core Query Logic ---

% This rule now generates all positions from 1 to N.
is_position(P) :-
    num_positions(N),
    between(1, N, P).

is_position(N, P) :- between(1, N, P).


is_god(truly).
is_god(falsely).
is_god(random).

% --- 1. THE EVALUATOR: 3-State Logic (True, False, Paradox) ---

% Performance Hack: Memoize results so we don't re-simulate identical questions
:- table evaluate_3state/4.

% evaluate_3state(+Question, +Path, +WorldState, -Result)
% Result is one of: [true, false, paradox]

% A. Logical AND (Kleene Strong Logic)
evaluate_3state((Q1, Q2), Path, WS, Result) :-
    evaluate_3state(Q1, Path, WS, R1),
    evaluate_3state(Q2, Path, WS, R2),
    logic_and_3state(R1, R2, Result).

% B. Logical OR (Kleene Strong Logic)
evaluate_3state((Q1 ; Q2), Path, WS, Result) :-
    evaluate_3state(Q1, Path, WS, R1),
    evaluate_3state(Q2, Path, WS, R2),
    logic_or_3state(R1, R2, Result).

% C. Logical XOR
evaluate_3state((Q1 xor Q2), Path, WS, Result) :-
    evaluate_3state(Q1, Path, WS, R1),
    evaluate_3state(Q2, Path, WS, R2),
    logic_xor_3state(R1, R2, Result).

% D. Nested Questions (Handling Silence)
% Default: "Will they say 'da'?"
evaluate_3state(query_position_question(Pos, SubQ), Path, WS, Result) :-
    query_position_3state(Pos, SubQ, Path, WS, Response),
    (   Response == da -> Result = true
    ;   Result = false 
    ).

% "Will they say 'ja'?"
evaluate_3state(query_position_question_ja(Pos, SubQ), Path, WS, Result) :-
    query_position_3state(Pos, SubQ, Path, WS, Response),
    (   Response == ja -> Result = true
    ;   Result = false
    ).

% "Will they be silent?"
evaluate_3state(query_position_question_silent(Pos, SubQ), Path, WS, Result) :-
    query_position_3state(Pos, SubQ, Path, WS, Response),
    (   Response == silent -> Result = true
    ;   Result = false
    ).

% E. Base Cases
evaluate_3state(true, _, _, true).
evaluate_3state(fail, _, _, false).
evaluate_3state(at_position_question(P, G), _, WS, Result) :-
    (at_position(P, G, WS) -> Result = true ; Result = false).

% F. The Paradox Physics (Definitions)
evaluate_3state(paradox_universal, _, _, paradox).
evaluate_3state(paradox_falsely, _, _, paradox_falsely). % Pass token to God Logic
evaluate_3state(paradox_truly, _, _, paradox_truly).     % Pass token to God Logic

% at_position(Position, GodType, WorldState)
at_position(Position, God, w(PosList, _)) :-
    member(pos(Position, God, _), PosList).

% --- 2. LOGIC TABLES (Kleene Strong Logic) ---

% AND: False dominates Paradox
logic_and_3state(true, true, true).
logic_and_3state(true, false, false).
logic_and_3state(true, paradox, paradox).
logic_and_3state(false, _, false).       % Short-circuit
logic_and_3state(paradox, true, paradox).
logic_and_3state(paradox, false, false). % Strong Logic
logic_and_3state(paradox, paradox, paradox).
% Handle specialized paradox tokens as generic paradoxes for logic ops
logic_and_3state(paradox_falsely, _, paradox). 
logic_and_3state(_, paradox_falsely, paradox).
logic_and_3state(paradox_truly, _, paradox).
logic_and_3state(_, paradox_truly, paradox).

% OR: True dominates Paradox
logic_or_3state(true, _, true).          % Short-circuit
logic_or_3state(false, false, false).
logic_or_3state(false, true, true).
logic_or_3state(false, paradox, paradox).
logic_or_3state(paradox, true, true).    % Strong Logic
logic_or_3state(paradox, false, paradox).
logic_or_3state(paradox, paradox, paradox).
% Handle specialized paradox tokens as generic paradoxes for logic ops
logic_or_3state(paradox_falsely, _, paradox).
logic_or_3state(_, paradox_falsely, paradox).
logic_or_3state(paradox_truly, _, paradox).
logic_or_3state(_, paradox_truly, paradox).

% XOR
logic_xor_3state(true, false, true).
logic_xor_3state(false, true, true).
logic_xor_3state(true, true, false).
logic_xor_3state(false, false, false).
logic_xor_3state(paradox, _, paradox).
logic_xor_3state(_, paradox, paradox).
logic_xor_3state(paradox_falsely, _, paradox).
logic_xor_3state(_, paradox_falsely, paradox).
logic_xor_3state(paradox_truly, _, paradox).
logic_xor_3state(_, paradox_truly, paradox).

% get_utterance(LogicalAnswer, Language, Utterance)
get_utterance(true, da_yes, da).
get_utterance(fail, da_yes, ja).
get_utterance(true, da_no, ja).
get_utterance(fail, da_no, da).

% --- 3. GOD PSYCHOLOGY: Who Crashes? ---

query_position_3state(Pos, Question, Path, w(PosList, Lang), Utterance) :-
    member(pos(Pos, GodType, RandomAnswer), PosList),
    
    % 1. Evaluate Logic (Get True/False/Paradox)
    evaluate_3state(Question, Path, w(PosList, Lang), LogicResult),
    
    % 2. Determine Response (Handle Crashes)
    god_utterance(GodType, LogicResult, RandomAnswer, Path, Lang, Utterance).

% TRULY: Crashes on Paradox
god_utterance(truly, true, _, _, Lang, U) :- get_utterance(true, Lang, U).
god_utterance(truly, false, _, _, Lang, U) :- get_utterance(fail, Lang, U).
god_utterance(truly, paradox, _, _, _, silent).
god_utterance(truly, paradox_truly, _, _, _, silent). % Specific Trap
god_utterance(truly, paradox_falsely, _, _, Lang, U) :- get_utterance(fail, Lang, U). % Treat as false

% FALSELY: Crashes on Paradox
god_utterance(falsely, true, _, _, Lang, U) :- get_utterance(fail, Lang, U).
god_utterance(falsely, false, _, _, Lang, U) :- get_utterance(true, Lang, U).
god_utterance(falsely, paradox, _, _, _, silent).
god_utterance(falsely, paradox_falsely, _, _, _, silent). % Specific Trap
god_utterance(falsely, paradox_truly, _, _, Lang, U) :- get_utterance(true, Lang, U). % Treat as false -> Lies -> True

% RANDOM: Immune (Standard Physics)
% Ignores 'LogicResult' entirely and uses 'RandomAnswer' (the coin flip)
god_utterance(random, _, RandomAnswers, Path, Lang, Utterance) :-
    length(Path, N),
    Idx is N + 1,
    nth1(Idx, RandomAnswers, Ans),
    (   Ans == silent
    ->  Utterance = silent
    ;   get_utterance(Ans, Lang, Utterance)
    ).

% --- The Grammar ---
% Base Case 1: Trivial questions are allowed.
is_question(_, _, true).
is_question(_, _, fail).
is_question(_, _, paradox_universal).
is_question(_, _, paradox_truly).
is_question(_, _, paradox_falsely).

% Base Case 2: Questions about the world are allowed.
is_question(NumPos, _, at_position_question(Pos, God)) :-
    is_position(NumPos, Pos), is_god(God).

% Base Case 3: Questions about how gods at positions might respond to questions are allowed.
is_question(NumPos, MaxQDepth, query_position_question(Pos, Q)) :-
    MaxQDepth > 0, % Only recurse if we have budget
    NextQDepth is MaxQDepth - 1,
    is_position(NumPos, Pos),
    is_question(NumPos, NextQDepth, Q).

is_question(NumPos, MaxQDepth, query_position_question_ja(Pos, Q)) :-
    MaxQDepth > 0,
    NextQDepth is MaxQDepth - 1,
    is_position(NumPos, Pos),
    is_question(NumPos, NextQDepth, Q).

is_question(NumPos, MaxQDepth, query_position_question_silent(Pos, Q)) :-
    MaxQDepth > 0,
    NextQDepth is MaxQDepth - 1,
    is_position(NumPos, Pos),
    is_question(NumPos, NextQDepth, Q).

% Recursive rules for AND, OR, XOR.
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

is_question(NumPos, MaxQDepth, (Q1 xor Q2)) :-
    MaxQDepth > 0,
    NextQDepth is MaxQDepth - 1,
    is_question(NumPos, NextQDepth, Q1),
    is_question(NumPos, NextQDepth, Q2).

% --- The Logic ---
% --- World State Generation ---
god_types([truly, random, falsely]).

is_random_answer(true).
is_random_answer(fail).
is_random_answer(silent).

% --- Family Generation ---

% Helper to zip positions and gods into a template list.
build_family_template([], [], []).
build_family_template([Pos|Ps], [God|Gs], [pos(Pos, God, _)|Template]) :-
    build_family_template(Ps, Gs, Template).

% --- World Generation from a Family Template ---

% generate_worlds_from_templates(FamilyTemplate, ConcreteWorld)
% Takes a template and generates a concrete world by filling in random answers AND picking a language.
fill_random_answer(random, NumQuestions, RndAnsList) :-
    length(RndAnsList, NumQuestions),
    maplist(is_random_answer, RndAnsList). % Generate a list of N random answers
fill_random_answer(God, _, _) :-
    dif(God, random). % For non-random gods, the answer is unbound.

:- dynamic allowed_languages/1.

generate_worlds_from_templates(Template, NumQs, AllowedLangs, w(PosList, Lang)) :-
    generate_pos_list(Template, NumQs, PosList),
    member(Lang, AllowedLangs).

% Original generate_worlds_from_templates for backward compatibility/default behavior
generate_worlds_from_templates(Template, NumQs, w(PosList, Lang)) :-
    generate_pos_list(Template, NumQs, PosList),
    (   allowed_languages(Langs)
    ->  member(Lang, Langs)
    ;   member(Lang, [da_yes, da_no])
    ).

generate_pos_list([], _, []).
generate_pos_list([pos(P, God, _)|T], NumQs, [pos(P, God, RndAns)|W]) :-
    fill_random_answer(God, NumQs, RndAns),
    generate_pos_list(T, NumQs, W).

% --- Helper Predicates ---

% get_answer_path(Tree, World, PathOfAnswersFromTreeRoot, PathOfAnswers)
% Simulates the questioning process for a given tree and world.
get_answer_path(Tree, World, Path) :- get_answer_path_recursive(Tree, World, [], Path).
get_answer_path_recursive(leaf(_), _, _, []).
get_answer_path_recursive(tree(q(Pos, Q), DaT, JaT, SilentT), World, PathSoFar, [Ans|Rest]) :-
    query_position_3state(Pos, Q, PathSoFar, World, Ans), % Ans will be da, ja, or silent
    (
        Ans = da
    ->  get_answer_path_recursive(DaT, World, [da|PathSoFar], Rest)
    ;   Ans = ja
    ->  get_answer_path_recursive(JaT, World, [ja|PathSoFar], Rest)
    ;   Ans = silent
    ->  get_answer_path_recursive(SilentT, World, [silent|PathSoFar], Rest)
    ).

% get_family_signature_set(Tree, Family, SignatureSet)
% Calculates the set of all possible answer paths for a given family.
get_family_signature_set(Tree, NumQs, Family, SignatureSet) :-
    % Find all possible worlds within this family
    findall(W, generate_worlds_from_templates(Family, NumQs, [da_yes, da_no], W), Worlds),
    % For each world, find the answer path, collecting them all
    findall(Path,
            (   member(World, Worlds),
                get_answer_path(Tree, World, Path)
            ),
            Paths),
    % Sort the list to create a canonical set of paths
    sort(Paths, SignatureSet).

% --- The Main Distinguishing Question Logic ---

% --- New Parameterized Generators ---

% Generates all permutation families for a given number of positions and set of gods.
generate_permutation_families(NumPos, GodTypes, FamilyTemplate) :-
    findall(P, between(1, NumPos, P), Positions),
    permutation(GodTypes, PermutedGods),
    build_family_template(Positions, PermutedGods, FamilyTemplate).

% Generates all uniform families for a given number of positions and set of gods.
generate_uniform_families(NumPos, GodTypes, FamilyTemplate) :-
    member(GodType, GodTypes),
    build_uniform_family(NumPos, GodType, FamilyTemplate).

% Helper that now takes NumPos as an argument.
build_uniform_family(NumPos, GodType, FamilyTemplate) :-
    findall(P, between(1, NumPos, P), Positions),
    length(Positions, Len),
    length(Gods, Len),
    maplist(=(GodType), Gods),
    build_family_template(Positions, Gods, FamilyTemplate).

% --- New Generator for Canonical Combinations ---

% generate_canonical_combinations(NumPos, GodTypes, CanonicalFamily)
% This generator is for a problem where the GodTypes list defines ONE combination.
% It produces only one result: the single, canonical family for that combination.
generate_canonical_combinations(NumPos, GodTypes, CanonicalFamily) :-
    % Ensure the number of positions matches the number of gods provided.
    length(GodTypes, NumPos),
    % Call the helper to create the single, canonical version.
    generate_canonical_combination_family(GodTypes, CanonicalFamily).

% (This is the helper from our previous discussion)
generate_canonical_combination_family(GodTypes, CanonicalFamily) :-
    sort(GodTypes, CanonicalGods),
    length(GodTypes, NumPos),
    findall(P, between(1, NumPos, P), Positions),
    build_family_template(Positions, CanonicalGods, CanonicalFamily).

% This is the main logic, now using the bounded tree generator.
% --- The NEW, Efficient Pruning Algorithm with Memoization ---

:- dynamic distinct_q/3. % distinct_q(Question, Signature, Complexity)
:- dynamic seen_signature/1.

init_distinct_generator :-
    retractall(distinct_q(_, _, _)),
    retractall(seen_signature(_)).

% Generates the universe of distinct questions up to MaxComplexity
generate_universe(NumPos, MaxComplexity, CanonicalFamilies, NumQs) :-
    init_distinct_generator,

    length(CanonicalFamilies, NumFamilies),
    log(info, 'Generating universe for ~w families. No hard cap.', [NumFamilies]),
    
    % Complexity 0 (Base Cases)
    generate_base_cases(NumPos, BaseQuestions),
    process_candidates(BaseQuestions, 0, NumPos, NumQs, CanonicalFamilies),
    
    % Iterative Step - Run loop, catch failure or explicit stop
    (   run_generation_loop(NumPos, MaxComplexity, NumQs, CanonicalFamilies)
    ->  true
    ;   true
    ).

run_generation_loop(NumPos, MaxComplexity, NumQs, CanonicalFamilies) :-
    between(1, MaxComplexity, C),
    
    generate_candidates_at_complexity(C, NumPos, Candidates),
    length(Candidates, NumCandidates),
    log(info, 'Generated ~w candidates at complexity ~w', [NumCandidates, C]),
    process_candidates(Candidates, C, NumPos, NumQs, CanonicalFamilies),
    predicate_property(distinct_q(_,_,_), number_of_clauses(N)),
    log(info, 'Distinct questions so far: ~w', [N]),
    fail. % Force loop through all complexities

% Generates base case questions (Complexity 0)
generate_base_cases(NumPos, Questions) :-
    findall(true, true, L1),
    findall(fail, true, L2),
    findall(paradox_universal, true, L3),
    findall(paradox_truly, true, L3a),
    findall(paradox_falsely, true, L3b),
    findall(at_position_question(Pos, God),
            (is_position(NumPos, Pos), is_god(God)),
            L4),
    append([L1, L2, L3, L3a, L3b, L4], Questions).

% Generates candidate questions at a specific complexity C > 0
generate_candidates_at_complexity(C, NumPos, Candidates) :-
    findall(Q, candidate_at_complexity(C, NumPos, Q), Candidates).

candidate_at_complexity(C, NumPos, query_position_question(Pos, SubQ)) :-
    SubC is C - 1,
    distinct_q(SubQ, _, SubC),
    is_position(NumPos, Pos).

candidate_at_complexity(C, NumPos, query_position_question_ja(Pos, SubQ)) :-
    SubC is C - 1,
    distinct_q(SubQ, _, SubC),
    is_position(NumPos, Pos).

candidate_at_complexity(C, NumPos, query_position_question_silent(Pos, SubQ)) :-
    SubC is C - 1,
    distinct_q(SubQ, _, SubC),
    is_position(NumPos, Pos).

candidate_at_complexity(C, _, (Q1, Q2)) :-
    % (Q1, Q2) complexity is max(C1, C2) + 1 = C => max(C1, C2) = C - 1
    TargetSubC is C - 1,
    % One question must be at TargetSubC, the other <= TargetSubC.
    distinct_q(Q1, _, C1),
    distinct_q(Q2, _, C2),
    max_member(TargetSubC, [C1, C2]).

candidate_at_complexity(C, _, (Q1 ; Q2)) :-
    TargetSubC is C - 1,
    distinct_q(Q1, _, C1),
    distinct_q(Q2, _, C2),
    max_member(TargetSubC, [C1, C2]).

candidate_at_complexity(C, _, (Q1 xor Q2)) :-
    TargetSubC is C - 1,
    distinct_q(Q1, _, C1),
    distinct_q(Q2, _, C2),
    max_member(TargetSubC, [C1, C2]).

% Processes a list of candidate questions: computes signatures and stores new ones.
process_candidates([], _, _, _, _).
process_candidates([Q|Rest], C, NumPos, NumQs, Families) :-
    get_evaluate_signature(Q, NumPos, NumQs, Families, Sig),
    
    invert_signature(Sig, InvSig),

    (   (seen_signature(Sig) ; seen_signature(InvSig))
    ->  true
    ;   assertz(seen_signature(Sig)),
        assertz(distinct_q(Q, Sig, C)),
        log(debug, 'Found new Q: ~w (C=~w)', [Q, C]),
        % # of distinct questions so far
        predicate_property(distinct_q(_,_,_), number_of_clauses(N)),
        log(debug, 'Distinct questions so far: ~w', [N])
    ),
    process_candidates(Rest, C, NumPos, NumQs, Families).

% Inverts a signature to check for symmetry.
% Sig: sig(LogList, PosUttsList) - Both are lists parallel to Families.
invert_signature(sig(LogList, PosUttsList), sig(InvLogList, InvPosUttsList)) :-
    maplist(invert_answer_set, LogList, InvLogList),
    maplist(maplist(invert_answer_set), PosUttsList, InvPosUttsList).

invert_answer_set(Set, InvSet) :-
    maplist(invert_atom, Set, InvList),
    sort(InvList, InvSet).

invert_atom(true, fail).
invert_atom(fail, true).
invert_atom(da, ja).
invert_atom(ja, da).
invert_atom(silent, silent).
invert_atom(paradox, paradox).

% Computes the signature of `evaluate(Q)` across all worlds in Families.
% Returns sig(ListOfFamilyLogicalSigs, ListOfFamilyPositionalUtterances).
get_evaluate_signature(Q, NumPos, NumQs, Families, sig(LogicSigList, UtteranceSig)) :-
    maplist(get_family_eval_set(Q, NumPos, NumQs), Families, Pairs),
    maplist(arg(1), Pairs, LogicSigList), % Keep list of lists
    maplist(arg(2), Pairs, UtteranceSig). % List of Lists of UtteranceSets

% Helper to get the set of unique answers a specific family gives to Q
% Returns pair(LogicalSet, ListOfUtteranceSets_PerPosition)
get_family_eval_set(Q, NumPos, NumQs, candidate(FamilyTemplate, AllowedLangs), pair(LogicalSet, PositionalUtterances)) :-
    % 1. Calculate Logical Truth of Q (Position Independent approximation)
    findall(AnsToken,
            (   generate_worlds_from_templates(FamilyTemplate, NumQs, AllowedLangs, World),
                evaluate_3state(Q, [], World, Val),
                (Val == true -> AnsToken=true ; Val == false -> AnsToken=fail ; AnsToken=paradox)
            ),
            LogRaw),
    sort(LogRaw, LogicalSet),

    % 2. Calculate Utterances for EACH Position
    findall(P, between(1, NumPos, P), Positions),
    maplist(get_position_utterance_set(Q, NumQs, FamilyTemplate, AllowedLangs), Positions, PositionalUtterances).

% Helper: Get utterance set for a specific position P
get_position_utterance_set(Q, NumQs, FamilyTemplate, AllowedLangs, Pos, UtteranceSet) :-
    findall(Utterance,
            (   generate_worlds_from_templates(FamilyTemplate, NumQs, AllowedLangs, World),
                World = w(PosList, Lang),
                % Find God at Pos
                member(pos(Pos, GodType, RndAns), PosList),
                % Evaluate Q (Logic)
                evaluate_3state(Q, [], World, Val),
                % Determine Utterance using REAL God Physics
                god_utterance(GodType, Val, RndAns, [], Lang, Utterance)
            ),
            UttRaw),
    sort(UttRaw, UtteranceSet).

% evaluate_on_world removed as it is no longer used directly by maplist

% distinct_question now returns q(Pos, Q) using the precomputed universe.
% Note: We iterate Pos here, but Q comes from distinct_q.
distinct_question(MaxComplexity, NumPos, _NumQs, _CanonicalFamilies, q(Pos, Q)) :-
    is_position(NumPos, Pos),
    distinct_q(Q, _, C),
    C =< MaxComplexity.

% ... (rest of the file) ...

% --- Self-Contained nub/2 to replace the broken library version ---
my_nub([], []).
my_nub([H | T], [H | T2]) :-
    \+ memberchk(H, T),
    my_nub(T, T2).
my_nub([H | T], T2) :-
    memberchk(H, T),
    my_nub(T, T2).

% Helper to wrap a FamilyTemplate into a candidate/2 term with default languages
wrap_family_in_candidate(InitialLangs, FamilyTemplate, candidate(FamilyTemplate, InitialLangs)).

% --- Statistics for Pruning Ratio ---
:- dynamic explored_stats/2. % explored_stats(Depth, Count)
:- dynamic pruned_stats/2.   % pruned_stats(Depth, Count)

init_stats :-
    retractall(explored_stats(_, _)),
    retractall(pruned_stats(_, _)).

inc_explored(Depth) :-
    (   retract(explored_stats(Depth, N))
    ->  N1 is N + 1,
        assertz(explored_stats(Depth, N1))
    ;   assertz(explored_stats(Depth, 1))
    ).

inc_pruned(Depth) :-
    (   retract(pruned_stats(Depth, N))
    ->  N1 is N + 1,
        assertz(pruned_stats(Depth, N1))
    ;   assertz(pruned_stats(Depth, 1))
    ).

is_distinguishing_tree_bounded(NumPos, NumQs, QComplexity, GodTypes, Tree, GeneratorGoal) :-

    % 1. Generate all family permutations (this may contain duplicates).

    findall(F, call(GeneratorGoal, NumPos, GodTypes, F), FamiliesWithDuplicates),

    % 2. Remove duplicates.

    my_nub(FamiliesWithDuplicates, UniqueFamilyTemplates),
    maplist(wrap_family_in_candidate([da_yes, da_no]), UniqueFamilyTemplates, UniqueFamilies),

    

    % 3. Generate the Universe of Distinct Questions.

    log(info, 'Generating universe of questions (MaxComplexity=~w)...', [QComplexity]),

    generate_universe(NumPos, QComplexity, UniqueFamilies, NumQs),

    predicate_property(distinct_q(_,_,_), number_of_clauses(N)),

    log(info, 'Universe generated. Distinct questions found: ~w', [N]),



    % 4. Call the recursive pruning solver.

    init_stats,
    find_pruning_tree(NumQs, NumQs, QComplexity, NumPos, UniqueFamilies, UniqueFamilies, Tree).

% --- The Recursive Solver (Corrected Signature) ---
% Signature: find_pruning_tree(TotalNumQs, CurrentDepth, MaxQComp, NumPos, CanonicalFamilies, CurrentFamilies, Tree)

% --- Base Cases (use CurrentDepth) ---
find_pruning_tree(_, _, _, _, _, [], leaf([])).
find_pruning_tree(_, _, _, _, _, [Candidate], leaf([Candidate])).
find_pruning_tree(_, 0, _, _, _, Candidates, leaf(Candidates)) :- % Base case for depth
    (length(Candidates, 1) -> true ; !, fail). % Ran out of depth

% --- Recursive Pruning Step (Corrected) ---
find_pruning_tree(TotalNumQs, CurrentDepth, MaxQComplexity, NumPos, CanonicalFamilies, Candidates, tree(q(Pos, Q), DaTree, JaTree, SilentTree)) :-
    CurrentDepth > 0,
    NextDepth is CurrentDepth - 1,

    % --- Generate Distinct Questions ---
    % Now we just pull from our precomputed universe.
    distinct_question(MaxQComplexity, NumPos, TotalNumQs, CanonicalFamilies, q(Pos, Q)),
    inc_explored(CurrentDepth),

    % --- DEBUG: Print what we're trying ---
    length(Candidates, CandidateCount),
    log(debug, 'try(depth: ~w, q: ~w, families_in: ~w)', [CurrentDepth, q(Pos, Q), CandidateCount]),

    partition_families(Candidates, TotalNumQs, q(Pos, Q), DaCandidates, JaCandidates, SilentCandidates),

    % --- DEBUG: Print the result of the partition ---
    length(DaCandidates, DaSize),
    length(JaCandidates, JaSize),
    length(SilentCandidates, SilentSize),
    log(debug, 'partition(da: ~w, ja: ~w, silent: ~w)', [DaSize, JaSize, SilentSize]),

    % --- Pruning Check 1: Useless Split (Corrected) ---
    dif(DaCandidates, Candidates),
    dif(JaCandidates, Candidates),
    dif(SilentCandidates, Candidates),

    % --- Pruning Check 2: Sub-Problem Too Large (Corrected) ---
    MaxSize is 3^NextDepth, % NOTE TO GEMINI-CLI - DO NOT MODIFY THIS LINE EVER
    (   ( DaSize > MaxSize ; JaSize > MaxSize ; SilentSize > MaxSize )
    ->  % This branch is taken if the sub-problem is too big
        inc_pruned(CurrentDepth),
        log(debug, 'prune(reason: sub-problem too large [da: ~w, ja: ~w, silent: ~w] - [MaxSize: ~w])', [DaSize, JaSize, SilentSize, MaxSize]),
        fail % Just fail, allowing Prolog to backtrack to the 'between' loop
    ;   % This branch is taken if the check passes
        true
    ),

    % If we get here, the question was good. No commit cut is needed.
    log(info, 'commit(q: ~w)', [q(Pos, Q)]),

    % --- Recurse ---
    find_pruning_tree(TotalNumQs, NextDepth, MaxQComplexity, NumPos, CanonicalFamilies, DaCandidates, DaTree),
    find_pruning_tree(TotalNumQs, NextDepth, MaxQComplexity, NumPos, CanonicalFamilies, JaCandidates, JaTree),
    find_pruning_tree(TotalNumQs, NextDepth, MaxQComplexity, NumPos, CanonicalFamilies, SilentCandidates, SilentTree).

% --- Helpers required by the new algorithm ---

% Partitions families based on ALL their possible answers to a question.
% --- The Final, Corrected Partition Logic ---

% This new partition_families is much smarter.
% --- The Final, Corrected Partition Logic ---

partition_families(CandidatesIn, NumQs, QuestionNode, DaCandidates, JaCandidates, SilentCandidates) :-
    % For each input candidate, try to refine it into Da, Ja, and Silent branches
    findall(DaC, 
            (   member(Cand, CandidatesIn),
                refine_candidate(QuestionNode, NumQs, Cand, da, DaC)
            ),
            DaCandidates),
    findall(JaC, 
            (   member(Cand, CandidatesIn),
                refine_candidate(QuestionNode, NumQs, Cand, ja, JaC)
            ),
            JaCandidates),
    findall(SilentC, 
            (   member(Cand, CandidatesIn),
                refine_candidate(QuestionNode, NumQs, Cand, silent, SilentC)
            ),
            SilentCandidates).

% refine_candidate(QuestionNode, NumQs, CandidateIn, ExpectedAnswer, CandidateOut)
% Takes an input candidate (FamilyTemplate, AllowedLangsIn) and an ExpectedAnswer (da/ja).
% Returns CandidateOut (FamilyTemplate, AllowedLangsOut) if there are languages in AllowedLangsIn
% consistent with the family giving ExpectedAnswer to QuestionNode. Otherwise, fails.
refine_candidate(QuestionNode, NumQs, candidate(FamilyTemplate, AllowedLangsIn), ExpectedAnswer, candidate(FamilyTemplate, AllowedLangsOut)) :-
    findall(Lang,
            (   member(Lang, AllowedLangsIn),
                % Check if the family, assuming 'Lang', can actually produce ExpectedAnswer
                can_family_answer_with_lang(QuestionNode, NumQs, FamilyTemplate, Lang, ExpectedAnswer)
            ),
            AllowedLangsOut),
    % Only succeed if there's at least one consistent language
    AllowedLangsOut \= [].

% can_family_answer_with_lang(QuestionNode, NumQs, FamilyTemplate, Language, ExpectedAnswer)
% Succeeds if there exists a world within FamilyTemplate and Language where the family
% gives the ExpectedAnswer to QuestionNode.
can_family_answer_with_lang(q(Pos, Q), NumQs, FamilyTemplate, Language, ExpectedAnswer) :-
    once((
        generate_worlds_from_templates(FamilyTemplate, NumQs, [Language], World),
        query_position_3state(Pos, Q, [], World, Answer),
        Answer == ExpectedAnswer
    )).

get_single_world_answer(q(Pos, Q), _NumQs, _FamilyTemplate, World, Answer) :-
    query_position_3state(Pos, Q, [], World, Answer).


% Succeeds if a family can EVER produce 'Answer' (da/ja) for the given question.
family_answers_question(q(Pos, Q), NumQs, candidate(FamilyTemplate, AllowedLangs), Answer) :-
    generate_worlds_from_templates(FamilyTemplate, NumQs, AllowedLangs, World),
    query_position_3state(Pos, Q, [], World, Ans),
    Ans == Answer,
    !. % We only need to find one such world, not all of them.

% find_tree_by_simplest_question(MaxQComplexity, NumPos, NumQs, GodTypes, Tree, Generator)
find_tree_by_simplest_question(MaxQComplexity, NumPos, NumQs, GodTypes, Tree, GeneratorGoal) :-
    % 1. Iterate through question complexities, from 0 up to the max.
    between(0, MaxQComplexity, CurrentQComplexity),

    log(info, '--- Searching with question complexity limit: ~w ---', [CurrentQComplexity]),

    % 2. Call a version of is_distinguishing_tree that uses the bounded generator.
    is_distinguishing_tree_bounded(NumPos, NumQs, CurrentQComplexity, GodTypes, Tree, GeneratorGoal),

    % 3. Cut ('!') to stop the search as soon as the FIRST solution is found.
    !.


solve_3gods_tf(Tree) :- is_distinguishing_tree_bounded(
       3,                          % Num Positions
       3,                          % Num Questions
       10,                          % Max Question Complexity
       [truly, falsely, random],   % God Types to use
       Tree,                       % The variable to hold the solution
       generate_permutation_families % The name of the generator predicate to use
   ).

solve_and_print_riddle :-
    NumPos = 3, NumQs = 3, QComplexity = 1,
    GodTypes = [truly, falsely, random],
    Generator = generate_permutation_families,
    
    log(info, 'Searching for solution...'),
    
    (   is_distinguishing_tree_bounded(NumPos, NumQs, QComplexity, GodTypes, Tree, Generator)
    ->  log(info, '--- SOLUTION FOUND (Human Readable) ---'),
        draw_tree(Tree, human),
        
        log(info, '--- SOLUTION FOUND (Raw Prolog Object) ---'),
        draw_tree(Tree, raw),

        print_stats
    ;   log(info, '--- NO SOLUTION FOUND ---')
    ), !.

print_stats :-
    log(info, '--- SEARCH STATISTICS ---'),
    log(info, 'Note: Tree Level 1 is the Root.'),

    % Collect all depths (Budgets) that have activity
    findall(D, (explored_stats(D, _) ; pruned_stats(D, _)), Depths),
    sort(Depths, SortedBudgets),       % Ascending [1, 2, 3]
    reverse(SortedBudgets, DescendingBudgets), % Descending [3, 2, 1]
    
    (   DescendingBudgets = [MaxBudget|_]
    ->  true
    ;   MaxBudget = 0
    ),

    % Header
    log(info, 'Level | Explored | Pruned | Ratio'),
    log(info, '-----------------------------------'),

    forall(member(Budget, DescendingBudgets), print_level_stat(Budget, MaxBudget)),

    % Totals
    sum_explored(TotalExplored),
    sum_pruned(TotalPruned),
    (   TotalExplored > 0
    ->  TotalRatio is TotalPruned / TotalExplored,
        log(info, '-----------------------------------'),
        log(info, 'TOTAL | ~w | ~w | ~w', [TotalExplored, TotalPruned, TotalRatio])
    ;   true
    ).

print_level_stat(Budget, MaxBudget) :-
    TreeLevel is MaxBudget - Budget + 1,
    (explored_stats(Budget, E) -> Explored = E ; Explored = 0),
    (pruned_stats(Budget, P) -> Pruned = P ; Pruned = 0),
    (   Explored > 0
    ->  Ratio is Pruned / Explored
    ;   Ratio = 0.0
    ),
    log(info, '  ~w   |    ~w   |   ~w   | ~w', [TreeLevel, Explored, Pruned, Ratio]).

sum_explored(Sum) :-
    findall(C, explored_stats(_, C), Cs),
    sum_list(Cs, Sum).

sum_pruned(Sum) :-
    findall(C, pruned_stats(_, C), Cs),
    sum_list(Cs, Sum).


% all_disjoint(ListOfSets)