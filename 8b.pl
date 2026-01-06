% :- use_module(library(lists)).

iamrunningthelatestversion.

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

% Backwards compatibility for tests that might call query_position/4 expecting logical truth
% (Though we should update tests ideally, but this helps safe-guard)
% query_position(Position, Question, Path, WorldState) :-
%    query_position(Position, Question, Path, WorldState, da). 
%    % NOTE: This would mean "Did he say da?". 
%    % But old tests expected logical result. 
%    % Better to update tests.

% --- The Grammar ---
% Base Case 1: Trivial questions are allowed.
is_question(_, _, true).
is_question(_, _, fail).

% Base Case 2: Questions about the world are allowed.
is_question(NumPos, _, at_position_question(Pos, God)) :-
    is_position(NumPos, Pos), is_god(God).

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

generate_worlds_from_templates(Template, NumQs, w(PosList, Lang)) :-
    generate_pos_list(Template, NumQs, PosList),
    member(Lang, [da_yes, da_no]).

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
    findall(W, generate_worlds_from_templates(Family, NumQs, W), Worlds),
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
    
    % Complexity 0 (Base Cases)
    generate_base_cases(NumPos, BaseQuestions),
    process_candidates(BaseQuestions, 0, NumQs, CanonicalFamilies),
    
    % Iterative Step
    between(1, MaxComplexity, C),
    generate_candidates_at_complexity(C, NumPos, Candidates),
    process_candidates(Candidates, C, NumQs, CanonicalFamilies),
    fail. % Force loop through all complexities
generate_universe(_, _, _, _).

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
process_candidates([], _, _, _).
process_candidates([Q|Rest], C, NumQs, Families) :-
    % For base cases, the Q is the full question logic (e.g. true, at_pos).
    % But for query_position(Pos, SubQ), the Q *is* the question.
    % Wait, distinct_q stores the INNER question structure?
    % Yes. But to compute signature, we need to know "who asks whom"?
    % No, distinct_q stores `Q` such that `q(Pos, Q)` is asked.
    % Actually, the grammar defines `is_question(..., Q)`.
    % And the solver asks `q(Pos, Q)`.
    % The signature of `Q` depends on `Pos` if `Q` is `true`? No.
    % `evaluate(true)` is true regardless of Pos.
    % `evaluate(at_position(P,G))` is true regardless of "Current Pos".
    % `evaluate(query_position(P, SubQ))` is true regardless of "Current Pos".
    
    % So `Q` has a unique signature independent of where it is asked!
    % Wait, `evaluate` calls `at_position(P, G, WS)`. WS is global.
    % `evaluate` calls `query_position(P, SubQ, WS)`. WS is global.
    % So yes, the signature of Q is global.
    % The only thing that depends on `Pos` is `q(Pos, Q)` in the tree node?
    % NO! `q(Pos, Q)` means "Ask God at Pos the question Q".
    % But `evaluate` is what runs.
    % `query(GodType, Q)` calls `evaluate(Q)`.
    % `evaluate` does NOT depend on GodType (except indirectly via random).
    
    % BUT, `truly` passes `evaluate` through.
    % `falsely` inverts it.
    % `random` ignores it.
    
    % So the signature of `Q` (from `evaluate` perspective) is what matters?
    % Yes.
    
    % Let's verify `get_question_signature`.
    % It calls `get_family_response`.
    % Which calls `get_single_question_signature`.
    % Which calls `query_position(Pos, Q, ..., World)`.
    % WAIT. `query_position` uses `Pos`.
    % The solver iterates `q(Pos, Q)`.
    % So `distinct_question` needs to return `q(Pos, Q)`.
    % But my generator builds `Q`.
    
    % User wanted `(Q1, Q2)` optimization.
    % `Q1` and `Q2` are `Q`s.
    % `distinct_q` should store `Q`.
    % `distinct_question` should bind `q(Pos, Q)`.
    % But `distinct_question` takes `CanonicalFamilies` to compute signature of `q(Pos, Q)`.
    
    % If I store `distinct_q(Q, Sig, C)`, what is Sig?
    % Sig should be `evaluate(Q)` across worlds?
    % Yes!
    % If `distinct_q` stores `evaluate(Q)` results, then:
    % `(Q1, Q2)` signature is `Sig1 & Sig2`.
    % `(Q1; Q2)` signature is `Sig1 | Sig2`.
    % `query_position(P, SubQ)` signature depends on `Sig(SubQ)` and World structure.
    
    % So `process_candidates` computes signature of `Q` via `evaluate`.
    get_evaluate_signature(Q, NumQs, Families, Sig),
    
    (   seen_signature(Sig)
    ->  true
    ;   assertz(seen_signature(Sig)),
        assertz(distinct_q(Q, Sig, C))
        % writef('Found new Q: %w (C=%w)\n', [Q, C])
    ),
    process_candidates(Rest, C, NumQs, Families).

% Computes the signature of `evaluate(Q)` across all worlds in Families.
% Sig is a list of [true, fail] (one per world).
get_evaluate_signature(Q, NumQs, Families, Sig) :-
    % Flatten families into a list of worlds
    findall(W, (member(F, Families), generate_worlds_from_templates(F, NumQs, W)), Worlds),
    maplist(evaluate_on_world(Q), Worlds, Sig).

evaluate_on_world(Q, World, Result) :-
    ( evaluate(Q, [], World) -> Result = true ; Result = fail ).

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

is_distinguishing_tree_bounded(NumPos, NumQs, QComplexity, GodTypes, Tree, GeneratorGoal) :-

    % 1. Generate all family permutations (this may contain duplicates).

    findall(F, call(GeneratorGoal, NumPos, GodTypes, F), FamiliesWithDuplicates),

    % 2. Remove duplicates.

    my_nub(FamiliesWithDuplicates, UniqueFamilies),

    

    % 3. Generate the Universe of Distinct Questions.

    writef('Generating universe of questions (MaxComplexity=%w)...\n', [QComplexity]),

    generate_universe(NumPos, QComplexity, UniqueFamilies, NumQs),

    predicate_property(distinct_q(_,_,_), number_of_clauses(N)),

    writef('Universe generated. Distinct questions found: %w\n', [N]),



    % 4. Call the recursive pruning solver.

    find_pruning_tree(NumQs, NumQs, QComplexity, NumPos, UniqueFamilies, UniqueFamilies, Tree).

% --- The Recursive Solver (Corrected Signature) ---
% Signature: find_pruning_tree(TotalNumQs, CurrentDepth, MaxQComp, NumPos, CanonicalFamilies, CurrentFamilies, Tree)

% --- Base Cases (use CurrentDepth) ---
find_pruning_tree(_, _, _, _, _, [], leaf).
find_pruning_tree(_, _, _, _, _, [_Family], leaf).
find_pruning_tree(_, 0, _, _, _, Families, leaf) :- % Base case for depth
    (length(Families, 1) -> true ; !, fail). % Ran out of depth

% --- Recursive Pruning Step (Corrected) ---
find_pruning_tree(TotalNumQs, CurrentDepth, MaxQComplexity, NumPos, CanonicalFamilies, Families, tree(q(Pos, Q), DaTree, JaTree)) :-
    CurrentDepth > 0,
    NextDepth is CurrentDepth - 1,

    % --- Generate Distinct Questions ---
    % Now we just pull from our precomputed universe.
    distinct_question(MaxQComplexity, NumPos, TotalNumQs, CanonicalFamilies, q(Pos, Q)),

    % --- DEBUG: Print what we're trying ---
    % length(Families, FamilyCount),
    % writeln(try(depth: CurrentDepth, q: (Pos,Q), families_in: FamilyCount)),

    partition_families(Families, TotalNumQs, q(Pos, Q), DaFamilies, JaFamilies),

    % --- DEBUG: Print the result of the partition ---
    length(DaFamilies, DaSize),
    length(JaFamilies, JaSize),
    % writeln(partition(da_count: DaSize, ja_count: JaSize)),

    % --- Pruning Check 1: Useless Split (Corrected) ---
    % Succeeds only if the split is useful (neither side is empty).
    dif(DaFamilies, []),
    dif(JaFamilies, []),

    % --- Pruning Check 2: Sub-Problem Too Large (Corrected) ---
    % Succeeds only if the sub-problems are not too large. Allows backtracking on failure.
    MaxSize is 2^NextDepth,
    (   ( DaSize > MaxSize ; JaSize > MaxSize )
    ->  % This branch is taken if the sub-problem is too big
        % writeln(prune(reason: 'sub-problem too large')),
        fail % Just fail, allowing Prolog to backtrack to the 'between' loop
    ;   % This branch is taken if the check passes
        true
    ),

    % If we get here, the question was good. No commit cut is needed.
    % writeln(commit(q: (Pos,Q))),

    % --- Recurse ---
    find_pruning_tree(TotalNumQs, NextDepth, MaxQComplexity, NumPos, CanonicalFamilies, DaFamilies, DaTree),
    find_pruning_tree(TotalNumQs, NextDepth, MaxQComplexity, NumPos, CanonicalFamilies, JaFamilies, JaTree).

% --- Helpers required by the new algorithm ---

% Partitions families based on ALL their possible answers to a question.
% --- The Final, Corrected Partition Logic ---

% This new partition_families is much smarter.
% --- The Final, Corrected Partition Logic ---

partition_families(Families, NumQs, QuestionNode, DaFamilies, JaFamilies) :-
    % For each family, get its full signature set for this ONE question.
    maplist(get_single_question_signature(QuestionNode, NumQs), Families, Signatures),
    % writeln(debug_sigs(QuestionNode, Signatures)),

    % Group families based on their signature for this question.
    % FIX: If signature contains da, it goes to DaFamilies.
    findall(F, (nth1(I, Families, F), nth1(I, Signatures, Sig), member(da, Sig)), DaFamilies),
    % FIX: If signature contains ja, it goes to JaFamilies.
    findall(F, (nth1(I, Families, F), nth1(I, Signatures, Sig), member(ja, Sig)), JaFamilies).

% This helper calculates the full set of answers a family can give for a single question.
get_single_question_signature(q(Pos, Q), NumQs, Family, SignatureSet) :-
    findall(Ans,
            (   generate_worlds_from_templates(Family, NumQs, World),
                query_position(Pos, Q, [], World, Ans) % Returns da/ja
            ),
            Answers),
    sort(Answers, SignatureSet).

% Succeeds if a family can EVER produce 'Answer' (da/ja) for the given question.
family_answers_question(q(Pos, Q), NumQs, Family, Answer) :-
    generate_worlds_from_templates(Family, NumQs, World),
    query_position(Pos, Q, [], World, Ans), % Returns da/ja
    Ans == Answer,
    !. % We only need to find one such world, not all of them.

% find_tree_by_simplest_question(MaxQComplexity, NumPos, NumQs, GodTypes, Tree, Generator)
find_tree_by_simplest_question(MaxQComplexity, NumPos, NumQs, GodTypes, Tree, GeneratorGoal) :-
    % 1. Iterate through question complexities, from 0 up to the max.
    between(0, MaxQComplexity, CurrentQComplexity),

    writef('--- Searching with question complexity limit: %w ---\n', [CurrentQComplexity]),

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
    
    writeln('Searching for solution...'),
    
    is_distinguishing_tree_bounded(NumPos, NumQs, QComplexity, GodTypes, Tree, Generator),
    
    writeln('\n--- SOLUTION FOUND (Human Readable) ---'),
    draw_tree(Tree, human),
    
    writeln('\n--- SOLUTION FOUND (Raw Prolog Object) ---'),
    draw_tree(Tree, raw).


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
