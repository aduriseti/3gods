% :- use_module(library(lists)).
:- use_module(library(plunit)).

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


:- begin_tests(world_generation).

test('fill_random_answer for "truly" god leaves answer unbound') :-
    fill_random_answer(truly, 3, RndAns), % 3 is arbitrary
    var(RndAns). % var/1 succeeds if RndAns is an unbound variable

test('fill_random_answer for "random" god creates a correct-length list') :-
    fill_random_answer(random, /*num_questions=*/3, RndAnsList),
    length(RndAnsList, 3).

test('generate_worlds_from_templates for a "truly" family produces 2 worlds (da/ja)') :-
    % A 1-position, "all truly" family should have 2 worlds: da_yes and da_no
    build_uniform_family(1, truly, Family),
    findall(W, generate_worlds_from_templates(Family, /*num_questions=*/3, W), Worlds),
    length(Worlds, 2).

test('generate_worlds_from_templates for a "random" family produces 2 * 2^N worlds') :-
    % A 1-position, "all random" family, with 2 questions (N=2)...
    % 2^2 = 4 answers patterns.
    % 2 languages.
    % Total 8 worlds.
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
% Note: With 2 languages, a "truly" god saying 'true' will say 'da' in one world and 'ja' in another.
% So the signature for a single question "true" for a Truly family is [da, ja].

test('get_family_signature_set for "all truly" family is [da, ja]') :-
    Tree = tree(q(1, true), leaf, leaf),
    build_uniform_family(1, truly, Family),
    get_family_signature_set(Tree, /*num_questions=*/1, Family, [[da], [ja]]).

test('get_family_signature_set for "all falsely" family is [da, ja]') :-
    Tree = tree(q(1, true), leaf, leaf),
    build_uniform_family(1, falsely, Family),
    get_family_signature_set(Tree, /*num_questions=*/1, Family, [[da], [ja]]).

% For Random family, it can say da or ja in all language contexts because it lies/tells truth arbitrarily.
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

% With da/ja, 1 simple question cannot distinguish T from F, because both can say da or ja.
% T (da=yes) -> da.  T (da=no) -> ja.
% F (da=yes) -> ja.  F (da=no) -> da.
% Signatures overlap perfectly: [da, ja] vs [da, ja].
test('1 question CAN distinguish [truly] from [falsely] with embedded question') :-
    is_distinguishing_tree_bounded(1, 1, 1, [truly, falsely], _Tree, generate_uniform_families).

test('truly with 2 positions is distinguishable by default even with 0 questions]') :-
    is_distinguishing_tree_bounded(
           2, % Num Positions
           0, % Tree Depth (Num Questions)
           0, % Max Question Complexity
           [truly, truly], % The multiset of gods for the combination
           _Tree,
           generate_canonical_combinations % Use the NEW generator
       ).

test('Exhaustive search proves [truly] vs [random] is indistinguishable for 1 Q^1 question') :-
    % We are asserting that the following goal MUST FAIL.
    % The '\+' operator succeeds if its argument fails completely.
    \+ is_distinguishing_tree_bounded(
           1, % Num Positions
           1, % Tree Depth (Num Questions)
           1, % Max Question Complexity
           [truly, random],
           _Tree,
           generate_uniform_families
       ).

test('Exhaustive search proves [truly] vs [random] is indistinguishable for 3 Q^3 question') :-
    % We are asserting that the following goal MUST FAIL.
    % The '\+' operator succeeds if its argument fails completely.
    \+ is_distinguishing_tree_bounded(
           1, % Num Positions
           3, % Tree Depth (Num Questions)
           3, % Max Question Complexity
           [truly, random],
           _Tree,
           generate_uniform_families
       ).

test('Exhaustive search proves [truly,falsely,random] are indistinguishable for 1 Q^1 question') :-
    % We are asserting that the following goal MUST FAIL.
    % The '\+' operator succeeds if its argument fails completely.
    \+ is_distinguishing_tree_bounded(
           3, % Num Positions
           1, % Tree Depth (Num Questions)
           1, % Max Question Complexity (e.g., nesting one level deep)
           [truly, falsely, random],
           _Tree,
           generate_permutation_families
       ).

:- end_tests(distinguishing_scenarios).

:- begin_tests(pruning_logic).

% --- Tests for `family_answers_question/4` ---
test('family_answers: "Truly" family CAN answer da') :-
    build_uniform_family(1, truly, F_True),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = da,
    family_answers_question(Question, NumQs, F_True, ExpectedAnswer).

test('family_answers: "Truly" family CAN answer ja') :-
    build_uniform_family(1, truly, F_True),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = ja,
    family_answers_question(Question, NumQs, F_True, ExpectedAnswer).

test('family_answers: "Falsely" family CAN answer da') :-
    build_uniform_family(1, falsely, F_False),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = da,
    family_answers_question(Question, NumQs, F_False, ExpectedAnswer).

test('family_answers: "Falsely" family CAN answer ja') :-
    build_uniform_family(1, falsely, F_False),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = ja,
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

test('partition: simple question does NOT split [True, False] (both answer da/ja)') :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, falsely, F_False),
    In_Families     = [F_True, F_False],
    NumQs           = 1,
    Question        = q(1, true),
    partition_families(In_Families, NumQs, Question, DaFamilies, JaFamilies),
    % Both families can answer da and ja, so they appear in both lists
    assertion(DaFamilies = [F_True, F_False]),
    assertion(JaFamilies = [F_True, F_False]).

test('partition: embedded question SPLITS [True, False] perfectly') :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, falsely, F_False),
    In_Families     = [F_True, F_False],
    NumQs           = 1,
    % "If asked 'Am I Truly?', say da"
    % Truly -> da. Falsely -> ja.
    SubQ            = at_position_question(1, truly),
    Question        = q(1, query_position_question(1, SubQ)),
    
    partition_families(In_Families, NumQs, Question, DaFamilies, JaFamilies),
    assertion(DaFamilies = [F_True]),
    assertion(JaFamilies = [F_False]).

test('partition: correctly splits [True, Random] with embedded question') :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, random, F_Rand),
    In_Families     = [F_True, F_Rand],
    NumQs           = 1,
    SubQ            = at_position_question(1, truly),
    Question        = q(1, query_position_question(1, SubQ)),
    
    partition_families(In_Families, NumQs, Question, DaFamilies, JaFamilies),
    % Truly -> da. Random -> da/ja.
    assertion(DaFamilies = [F_True, F_Rand]),
    assertion(JaFamilies = [F_Rand]).

% --- Integration Tests for `find_pruning_tree/7` ---
test('pruning_tree: SUCCEEDS for [True, False] with 1 question (embedded)') :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, falsely, F_False),
    Families = [F_True, F_False],
    % Complexity 1 allows embedded questions
    generate_universe(1, 1, Families, 1), 
    TotalNumQs     = 1,
    CurrentDepth   = 1,
    MaxQComp       = 1,
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
    % write('Families: '), writeln(Families),

    my_nub(Families, UniqueFamilies),

    write('UniqueFamilies:'), length(UniqueFamilies, UL), writeln(UL),

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

:- end_tests(complex_pruning_scenario).

:- begin_tests(final_challenge).

test('PRINT SOLUTION for 3 Gods (T,F,R)') :-
    solve_and_print_riddle.

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
    is_distinguishing_tree_bounded(
        NumPos,
        NumQs,
        QComplexity,
        GodTypes,
        _Tree,
        Generator
    ).

test('3 Gods (T,F,R) is SOLVABLE with complexity 1 questions (3 questions deep)') :-
    % 1. Define the problem parameters
    NumPos       = 3,
    NumQs        = 3, 
    QComplexity  = 1, % Limit to simple questions
    GodTypes     = [truly, falsely, random],
    Generator    = generate_permutation_families,
    
    % 2. Call the main solver
    % We expect this to SUCCEED. Complexity 1 allows "If I asked you X..."
    is_distinguishing_tree_bounded(
        NumPos,
        NumQs,
        QComplexity,
        GodTypes,
        _Tree,
        Generator
    ).

test('3 Gods (T,F,R) is IMPOSSIBLE with only 2 questions (tree depth 2)', [fail]) :-
    % 1. Define the problem parameters
    NumPos       = 3,
    NumQs        = 2, % Not enough questions!
    QComplexity  = 1, 
    GodTypes     = [truly, falsely, random],
    Generator    = generate_permutation_families,
    
    % 2. Call the main solver
    is_distinguishing_tree_bounded(
        NumPos,
        NumQs,
        QComplexity,
        GodTypes,
        _Tree,
        Generator
    ).

test('3 Gods (T,F,R) is SOLVABLE with complexity 2 questions (3 questions deep)') :-
    % 1. Define the problem parameters
    NumPos       = 3,
    NumQs        = 3, 
    QComplexity  = 2, % Allow slightly more complex questions (nested once)
    GodTypes     = [truly, falsely, random],
    Generator    = generate_permutation_families,
    
    % 2. Call the main solver
    % We expect this to SUCCEED. Complexity 2 allows "If I asked X, would you say Y?"
    % which is sufficient for the solution.
    is_distinguishing_tree_bounded(
        NumPos,
        NumQs,
        QComplexity,
        GodTypes,
        _Tree,
        Generator
    ).

:- end_tests(final_challenge).