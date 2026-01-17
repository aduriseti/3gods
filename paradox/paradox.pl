% :- use_module(library(lists)).

iamrunningthelatestversion.

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

query(truly, Question, Path, WorldState, _) :-
    evaluate(Question, Path, WorldState).
query(falsely, Question, Path, WorldState, _) :-
    \+ evaluate(Question, Path, WorldState).
% Random god ignores question and positions of other gods, its answer is predetermined by WorldState compound term
query(random, _Question, Path, _WorldState, RndAnsList) :-
    length(Path, NumPreviousAnswers),
    CurrentQuestionNum is NumPreviousAnswers + 1,
    nth1(CurrentQuestionNum, RndAnsList, Answer),
    Answer. % Succeeds if the Nth answer is 'true'

% --- The Evaluator ---

% evaluate(Question, WorldState)
% This predicate handles each type of question from your grammar explicitly.
% :- table evaluate/3.

% 1. Evaluate logical AND
evaluate((Q1, Q2), Path, WorldState) :-
    evaluate(Q1, Path, WorldState),
    evaluate(Q2, Path, WorldState).

% 2. Evaluate logical OR
evaluate((Q1 ; Q2), Path, WorldState) :-
    ( evaluate(Q1, Path, WorldState) ; evaluate(Q2, Path, WorldState) ).

% 3. Evaluate logical XOR
evaluate((Q1 xor Q2), Path, WorldState) :-
    ( evaluate(Q1, Path, WorldState), \+ evaluate(Q2, Path, WorldState) )
    ;
    ( \+ evaluate(Q1, Path, WorldState), evaluate(Q2, Path, WorldState) ).

% 4. Evaluate state-dependent questions by calling them with the WorldState
evaluate(at_position_question(P, G), _, WS) :-
    at_position(P, G, WS). % at_position doesn't need the path
evaluate(query_position_question(Pos, SubQ), Path, WS) :-
    % Succeeds if the god at Pos would say 'da' to SubQ
    query_position(Pos, SubQ, Path, WS, da).

% 5. Evaluate base cases (these don't depend on the WorldState)
evaluate(true, _, _) :- true.
evaluate(fail, _, _) :- fail.


% =========================================================
% 2. THE EVALUATOR: 3-State Logic (True, False, Paradox)
% =========================================================

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
evaluate_3state(query_position_question(Pos, SubQ), Path, WS, Result) :-
    % query_position_3state now returns: da, ja, OR silent
    query_position_3state(Pos, SubQ, Path, WS, Response),
    (   Response == da -> Result = true
    ;   Response == ja -> Result = false
    ;   Response == silent -> Result = false 
        % Logic: "Did he say 'da'?" -> No, he was silent. So the statement is False.
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


% =========================================================
% 3. LOGIC TABLES (Kleene Strong Logic)
% =========================================================

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
logic_or_3state(paradox_falsely, _, paradox).
logic_or_3state(_, paradox_falsely, paradox).

% XOR
logic_xor_3state(true, false, true).
logic_xor_3state(false, true, true).
logic_xor_3state(true, true, false).
logic_xor_3state(false, false, false).
logic_xor_3state(paradox, _, paradox).
logic_xor_3state(_, paradox, paradox).


% =========================================================
% 4. GOD PSYCHOLOGY: Who Crashes?
% =========================================================

query_position_3state(Pos, Question, Path, w(PosList, Lang), Utterance) :-
    member(pos(Pos, GodType, RndAnsOrList), PosList),
    
    log(debug, 'QPos3State: Pos=~w, God=~w, Q=~w', [Pos, GodType, Question]),

    % 1. Evaluate Logic (Get True/False/Paradox)
    evaluate_3state(Question, Path, w(PosList, Lang), LogicResult),
    
    % 2. Extract Coin Flip for Random (if applicable)
    (   GodType == random
    ->  length(Path, Len),
        Idx is Len + 1,
        log(debug, '  Random: Len=~w, Idx=~w, List=~w', [Len, Idx, RndAnsOrList]),
        nth1(Idx, RndAnsOrList, CoinFlip),
        log(debug, '  CoinFlip=~w', [CoinFlip])
    ;   CoinFlip = _
    ),

    % 3. Determine Response (Handle Crashes)
    god_utterance(GodType, LogicResult, CoinFlip, Lang, Utterance),
    log(debug, '  GodUtterance: ~w', [Utterance]).

% TRULY: Crashes on Paradox
god_utterance(truly, true, _, Lang, U) :- get_utterance(true, Lang, U).
god_utterance(truly, false, _, Lang, U) :- get_utterance(fail, Lang, U).
god_utterance(truly, paradox, _, _, silent).       % <--- SILENCE
god_utterance(truly, paradox_truly, _, _, silent). % Specific Trap

% FALSELY: Crashes on Paradox
god_utterance(falsely, true, _, Lang, U) :- get_utterance(fail, Lang, U).
god_utterance(falsely, false, _, Lang, U) :- get_utterance(true, Lang, U).
god_utterance(falsely, paradox, _, _, silent).     % <--- SILENCE
god_utterance(falsely, paradox_falsely, _, _, silent). % Specific Trap

% RANDOM: Immune (Standard Physics)
% Ignores 'LogicResult' entirely and uses 'RandomAnswer' (the coin flip)
god_utterance(random, _, true, Lang, U) :- get_utterance(true, Lang, U).
god_utterance(random, _, fail, Lang, U) :- get_utterance(fail, Lang, U).


% at_position(Position, GodType, WorldState)
% Succeeds if the god at Position is GodType within the WorldState.
% Worldstate is w(PosList, Language)
at_position(Position, God, w(PosList, _)) :-
    member(pos(Position, God, _), PosList).

% get_utterance(LogicalAnswer, Language, Utterance)
get_utterance(true, da_yes, da).
get_utterance(fail, da_yes, ja).
get_utterance(true, da_no, ja).
get_utterance(fail, da_no, da).

% query_position now finds the full details for a position and returns the Utterance (da/ja).
query_position(Position, Question, Path, w(PosList, Language), Utterance) :-
    % Find the specific pos(...) term for the given Position.
    member(pos(Position, GodType, RandomAnswer), PosList),
    % Call the updated query/4 predicate to get Logical Result
    ( query(GodType, Question, Path, w(PosList, Language), RandomAnswer)
    -> LogicalAns = true
    ;  LogicalAns = fail
    ),
    % Map Logical Result to Utterance based on Language
    get_utterance(LogicalAns, Language, Utterance).

% --- The Grammar ---
% Base Case 1: Trivial questions are allowed.
is_question(_, _, true).
is_question(_, _, fail).

% Base Case 2: Questions about the world are allowed.
is_question(NumPos, _, at_position_question(Pos, God)) :-
    is_position(NumPos, Pos), is_god(God).

% NEW: The Paradox Atoms
% 1. Universal Paradox: "Is the answer to this question 'No'?"
% is_question(_, _, paradox_universal).
% 2. False-Killer: "Will you answer 'da'?" (Traps Liars)
% is_question(_, _, paradox_falsely).
% 3. True-Killer: "Will you answer 'ja'?" (Traps Truth-Tellers)
% is_question(_, _, paradox_truly).

% Base Case 3: Questions about how gods at positions might respond to questions are allowed.
is_question(NumPos, MaxQDepth, query_position_question(Pos, Q)) :-
    MaxQDepth > 0, % Only recurse if we have budget
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
get_answer_path_recursive(leaf, _, _, []).
get_answer_path_recursive(tree(q(Pos, Q), DaT, JaT), World, PathSoFar, [Ans|Rest]) :-
    query_position(Pos, Q, PathSoFar, World, Ans), % Ans will be da or ja
    (
        Ans = da
    ->  get_answer_path_recursive(DaT, World, [da|PathSoFar], Rest)
    ;
        get_answer_path_recursive(JaT, World, [ja|PathSoFar], Rest)
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
    MaxQuestions is ceil((3^NumFamilies) / 2),
    log(info, 'Max distinct questions limit set to: ~w (ceil(3^~w / 2))', [MaxQuestions, NumFamilies]),
    
    % Complexity 0 (Base Cases)
    generate_base_cases(NumPos, BaseQuestions),
    process_candidates(BaseQuestions, 0, NumQs, CanonicalFamilies, MaxQuestions),
    
    % Iterative Step - Run loop, catch failure or explicit stop
    (   run_generation_loop(NumPos, MaxComplexity, NumQs, CanonicalFamilies, MaxQuestions)
    ->  true
    ;   true
    ).

run_generation_loop(NumPos, MaxComplexity, NumQs, CanonicalFamilies, MaxQuestions) :-
    between(1, MaxComplexity, C),
    
    % Check if we already hit the limit before generating complex candidates
    predicate_property(distinct_q(_,_,_), number_of_clauses(CurrentCount)),
    (   CurrentCount >= MaxQuestions
    ->  log(info, 'Hit max question limit (~w). Stopping generation.', [MaxQuestions]),
        !, fail % Cut choicepoints and FAIL the loop to exit
    ;   true
    ),

    generate_candidates_at_complexity(C, NumPos, Candidates),
    length(Candidates, NumCandidates),
    log(info, 'Generated ~w candidates at complexity ~w', [NumCandidates, C]),
    process_candidates(Candidates, C, NumQs, CanonicalFamilies, MaxQuestions),
    % get # of distinct questions so far
    predicate_property(distinct_q(_,_,_), number_of_clauses(N)),
    log(info, 'Distinct questions so far: ~w', [N]),
    fail. % Force loop through all complexities

% Generates base case questions (Complexity 0)
generate_base_cases(NumPos, Questions) :-
    findall(true, true, L1),
    findall(fail, true, L2),
    findall(at_position_question(Pos, God),
            (is_position(NumPos, Pos), is_god(God)),
            L3),
    append([L1, L2, L3], Questions).

% Generates candidate questions at a specific complexity C > 0
generate_candidates_at_complexity(C, NumPos, Candidates) :-
    findall(Q, candidate_at_complexity(C, NumPos, Q), Candidates).

candidate_at_complexity(C, NumPos, query_position_question(Pos, SubQ)) :-
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
process_candidates(_, _, _, _, MaxQuestions) :-
    predicate_property(distinct_q(_,_,_), number_of_clauses(Count)),
    Count >= MaxQuestions,
    !,
    log(debug, 'Max questions limit reached (~w). Pruning remaining candidates.', [MaxQuestions]).
process_candidates([Q|Rest], C, NumQs, Families, MaxQuestions) :-
    % ... (comments about signature logic) ...
    get_evaluate_signature(Q, NumQs, Families, Sig),
    
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
    process_candidates(Rest, C, NumQs, Families, MaxQuestions).

% Inverts a signature (List of answer sets) to check for symmetry.
% Symmetry: Q splits families into (A, B). "not Q" splits into (B, A).
% Since A, B partition the space, {A, B} is the same partition as {B, A}.
invert_signature(sig(Log, Utt), sig(InvLog, InvUtt)) :-
    maplist(invert_answer_set, Log, InvLog),
    maplist(invert_answer_set, Utt, InvUtt).

invert_answer_set(Set, InvSet) :-
    maplist(invert_atom, Set, InvList),
    sort(InvList, InvSet).


invert_atom(true, fail).
invert_atom(fail, true).
invert_atom(da, ja).
invert_atom(ja, da).

% Computes the signature of `evaluate(Q)` across all worlds in Families.
% Returns sig(LogicalSig, UtteranceSig).
get_evaluate_signature(Q, NumQs, Families, sig(LogicalSig, UtteranceSig)) :-
    maplist(get_family_eval_set(Q, NumQs), Families, Pairs),
    maplist(arg(1), Pairs, LogicalSig),
    maplist(arg(2), Pairs, UtteranceSig).

% Helper to get the set of unique answers a specific family gives to Q
get_family_eval_set(Q, NumQs, candidate(FamilyTemplate, AllowedLangs), pair(LogicalSet, UtteranceSet)) :-
    findall(pair(Ans, Utterance),
            (   generate_worlds_from_templates(FamilyTemplate, NumQs, AllowedLangs, World),
                
                % Use new evaluator
                evaluate_3state(Q, [], World, Val),
                
                % Map 3-state logic to Signature Token
                (Val == true -> Ans=true 
                ; Val == false -> Ans=fail 
                ; Ans=paradox % New Token
                ),
                
                World = w(_, Lang),
                % Simulate the utterance to capture silence
                % We pick Position 1 arbitrarily for the signature 'utterance' profile 
                % (or we can just skip this if not using utterance sigs anymore)
                (Val == true -> get_utterance(true, Lang, Utterance)
                ; Val == false -> get_utterance(fail, Lang, Utterance)
                ; Utterance = silent
                )
            ),
            RawPairs),
    maplist(arg(1), RawPairs, LogRaw),
    maplist(arg(2), RawPairs, UttRaw),
    sort(LogRaw, LogicalSet),
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
find_pruning_tree(_, _, _, _, _, [], leaf).
find_pruning_tree(_, _, _, _, _, [_Candidate], leaf).
find_pruning_tree(_, 0, _, _, _, Candidates, leaf) :- % Base case for depth
    (length(Candidates, 1) -> true ; !, fail). % Ran out of depth

% --- Recursive Pruning Step (Corrected) ---
find_pruning_tree(TotalNumQs, CurrentDepth, MaxQComplexity, NumPos, CanonicalFamilies, Candidates, tree(q(Pos, Q), DaTree, JaTree)) :-
    CurrentDepth > 0,
    NextDepth is CurrentDepth - 1,

    % --- Generate Distinct Questions ---
    % Now we just pull from our precomputed universe.
    distinct_question(MaxQComplexity, NumPos, TotalNumQs, CanonicalFamilies, q(Pos, Q)),
    inc_explored(CurrentDepth),

    % --- DEBUG: Print what we're trying ---
    length(Candidates, CandidateCount),
    log(debug, 'try(depth: ~w, q: ~w, families_in: ~w)', [CurrentDepth, q(Pos, Q), CandidateCount]),

    partition_families(Candidates, TotalNumQs, q(Pos, Q), DaCandidates, JaCandidates),

    % --- DEBUG: Print the result of the partition ---
    length(DaCandidates, DaSize),
    length(JaCandidates, JaSize),
    log(debug, 'partition(da_count: ~w, ja_count: ~w)', [DaSize, JaSize]),

    % --- Pruning Check 1: Useless Split (Corrected) ---
    % Succeeds only if the split is useful (neither side is empty).
    dif(DaCandidates, []),
    dif(JaCandidates, []),

    % --- Pruning Check 2: Sub-Problem Too Large (Corrected) ---
    % Succeeds only if the sub-problems are not too large. Allows backtracking on failure.
    MaxSize is 2^NextDepth,
    (   ( DaSize > MaxSize ; JaSize > MaxSize )
    ->  % This branch is taken if the sub-problem is too big
        inc_pruned(CurrentDepth),
        log(debug, 'prune(reason: sub-problem too large)'),
        fail % Just fail, allowing Prolog to backtrack to the 'between' loop
    ;   % This branch is taken if the check passes
        true
    ),

    % If we get here, the question was good. No commit cut is needed.
    log(info, 'commit(q: ~w)', [q(Pos, Q)]),

    % --- Recurse ---
    find_pruning_tree(TotalNumQs, NextDepth, MaxQComplexity, NumPos, CanonicalFamilies, DaCandidates, DaTree),
    find_pruning_tree(TotalNumQs, NextDepth, MaxQComplexity, NumPos, CanonicalFamilies, JaCandidates, JaTree).

% --- Helpers required by the new algorithm ---

% Partitions families based on ALL their possible answers to a question.
% --- The Final, Corrected Partition Logic ---

% This new partition_families is much smarter.
% --- The Final, Corrected Partition Logic ---

partition_families(CandidatesIn, NumQs, QuestionNode, DaCandidates, JaCandidates) :-
    % For each input candidate, try to refine it into a Da-branch candidate and a Ja-branch candidate
    findall(DaC, 
            (   member(Cand, CandidatesIn),
                refine_candidate(QuestionNode, NumQs, Cand, da, DaC)
            ),
            DaCandidates),
    findall(JaC, 
            (   member(Cand, CandidatesIn),
                refine_candidate(QuestionNode, NumQs, Cand, ja, JaC)
            ),
            JaCandidates).

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
can_family_answer_with_lang(QuestionNode, NumQs, FamilyTemplate, Language, ExpectedAnswer) :-
    generate_worlds_from_templates(FamilyTemplate, NumQs, [Language], World),
    get_single_world_answer(QuestionNode, NumQs, FamilyTemplate, World, Answer),
    Answer == ExpectedAnswer.

get_single_world_answer(q(Pos, Q), _NumQs, _FamilyTemplate, World, Answer) :-
    query_position(Pos, Q, [], World, Answer).


% Succeeds if a family can EVER produce 'Answer' (da/ja) for the given question.
family_answers_question(q(Pos, Q), NumQs, candidate(FamilyTemplate, AllowedLangs), Answer) :-
    generate_worlds_from_templates(FamilyTemplate, NumQs, AllowedLangs, World),
    query_position(Pos, Q, [], World, Ans), % Returns da/ja
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
% --- Visualization ---

draw_tree(Tree) :-
    draw_tree(Tree, human).

draw_tree(Tree, Mode) :-
    writeln(''),
    draw_tree_rec(Tree, "", Mode).

draw_tree_rec(leaf, _Prefix, _Mode) :-
    writeln('  -> SOLVED').

draw_tree_rec(tree(q(Pos, Q), DaTree, JaTree), Prefix, Mode) :-
    (   Mode == raw
    ->  format(string(QStr), "~w", [Q])
    ;   render_question_short(Q, QStr)
    ),
    format('~wAsk God ~w: "~s"?~n', [Prefix, Pos, QStr]),
    
    string_concat(Prefix, "        ", NextPrefix),
    
    % Da Branch
    format('~w/ (da)~n', [Prefix]),
    draw_tree_rec(DaTree, NextPrefix, Mode),
    
    % Ja Branch
    format('~w\\ (ja)~n', [Prefix]),
    draw_tree_rec(JaTree, NextPrefix, Mode).

% Compact question renderer for the ASCII tree
render_question_short(at_position_question(P, G), S) :-
    format(string(S), "Is G~w ~w", [P, G]).
render_question_short(query_position_question(P, SubQ), S) :-
    render_question_short(SubQ, SubS),
    format(string(S), "If asked G~w '~s', say da", [P, SubS]).
render_question_short((Q1, Q2), S) :-
    render_question_short(Q1, S1),
    render_question_short(Q2, S2),
    format(string(S), "(~s AND ~s)", [S1, S2]).
render_question_short((Q1; Q2), S) :-
    render_question_short(Q1, S1),
    render_question_short(Q2, S2),
    format(string(S), "(~s OR ~s)", [S1, S2]).
render_question_short((Q1 xor Q2), S) :-
    render_question_short(Q1, S1),
    render_question_short(Q2, S2),
    format(string(S), "(~s XOR ~s)", [S1, S2]).
render_question_short(true, "True").
render_question_short(fail, "False").


solve_and_print_riddle :-
    NumPos = 3, NumQs = 3, QComplexity = 1,
    GodTypes = [truly, falsely, random],
    Generator = generate_permutation_families,
    
    log(info, 'Searching for solution...'),
    
    is_distinguishing_tree_bounded(NumPos, NumQs, QComplexity, GodTypes, Tree, Generator),
    
    log(info, '--- SOLUTION FOUND (Human Readable) ---'),
    draw_tree(Tree, human),
    
    log(info, '--- SOLUTION FOUND (Raw Prolog Object) ---'),
    draw_tree(Tree, raw),

    print_stats.

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
% Succeeds if every set in the list is disjoint from every other set.
all_disjoint([]) :- !.
all_disjoint([_]) :- !.
all_disjoint([Set1 | RestOfSets]) :-
    % Check that Set1 has no intersection with any of the other sets
    maplist(disjoint(Set1), RestOfSets),
    % Recursively check the rest of the list
    all_disjoint(RestOfSets).

% disjoint(Set1, Set2)
% Succeeds if the intersection of the two sets is empty.
disjoint(Set1, Set2) :-
    intersection(Set1, Set2, Intersection),
    Intersection == [], !.

% render_question(QuestionTerm, OutputString)
% Translates a question data structure into an English string.

% --- Base Cases ---
render_question(true, "1 == 1?").
render_question(fail, "1 == 0?").
render_question(at_position_question(Pos, God), String) :-
    format(string(String), "Is the god at position `~w` the `~w` god?", [Pos, God]).

render_question(query_position_question(Pos, Q), String) :-
    % 1. Get the raw, un-indented multi-line string for the sub-question
    render_question(Q, RawSubString),
    % 2. Indent the entire sub-string block
    indent_lines(RawSubString, IndentedSubString),
    % 3. Assemble the final string
    format(string(String), "Would the god at `~w` say 'yes' to the question:~n~s", [Pos, IndentedSubString]).

render_question((Q1, Q2), String) :-
    render_question(Q1, RawSubString1),
    indent_lines(RawSubString1, S1),
    render_question(Q2, RawSubString2),
    indent_lines(RawSubString2, S2),
    format(string(String), "(\n~s\n  AND\n~s\n)", [S1, S2]).

render_question((Q1 ; Q2), String) :-
    render_question(Q1, RawSubString1),
    indent_lines(RawSubString1, S1),
    render_question(Q2, RawSubString2),
    indent_lines(RawSubString2, S2),
    format(string(String), "(\n~s\n  OR\n~s\n)", [S1, S2]).

render_question((Q1 xor Q2), String) :-
    render_question(Q1, RawSubString1),
    indent_lines(RawSubString1, S1),
    render_question(Q2, RawSubString2),
    indent_lines(RawSubString2, S2),
    format(string(String), "(\n~s\n  XOR\n~s\n)", [S1, S2]).

% --- Modified Recursive Cases ---
% indent_lines(RawString, IndentedString)
% Adds a fixed indent to each line in a multi-line string.
indent_lines(Input, Output) :-
    Indent = "    ", % A single, fixed indent of 4 spaces
    % Split the input string into a list of lines
    split_string(Input, "\n", "", Lines),
    % Prepend the indent to each line
    maplist({Indent}/[Line, IndentedLine]>>(
        atom_concat(Indent, Line, IndentedLine)
    ), Lines, IndentedLines),
    % Join the lines back together with newlines
    atomic_list_concat(IndentedLines, "\n", Output).
