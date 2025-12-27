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

% 3. Evaluate state-dependent questions by calling them with the WorldState
evaluate(at_position_question(P, G), _, WS) :-
    at_position(P, G, WS). % at_position doesn't need the path
evaluate(query_position_question(Pos, SubQ), Path, WS) :-
    query_position(Pos, SubQ, Path, WS).

% 4. Evaluate base cases (these don't depend on the WorldState)
evaluate(true, _, _) :- true.
evaluate(fail, _, _) :- fail.

% at_position(Position, GodType, WorldStateList)
% Succeeds if the god at Position is GodType within the WorldStateList.
% Worldstate looks something like:
% [
%   pos(a, truly, _),
%   pos(b, random, true),
%   pos(c, falsely, _)
% ]
at_position(Position, God, WorldState) :-
    % Searches the list for a matching entry.
    member(pos(Position, God, _), WorldState).

% a -> b ==== !a | b
% query_position now finds the full details for a position.
query_position(Position, Question, Path, WorldState) :-
    % Find the specific pos(...) term for the given Position.
    member(pos(Position, GodType, RandomAnswer), WorldState),
    % Call the updated query/4 predicate with all necessary info.
    query(GodType, Question, Path, WorldState, RandomAnswer).

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

% Recursive rules for AND and OR remain the same.
is_question(NumPos, MaxQDepth, (Q1, Q2)) :-
    MaxQDepth > 0, % Only recurse if we have budget
    NextQDepth is MaxQDepth - 1,
    is_question(NumPos, NextQDepth, Q1),
    is_question(NumPos, NextQDepth, Q2).

is_question(NumPos, MaxQDepth, (Q1 ; Q2)) :-
    MaxQDepth > 0, % Only recurse if we have budget
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
% Takes a template and generates a concrete world by filling in random answers.
fill_random_answer(random, NumQuestions, RndAnsList) :-
    length(RndAnsList, NumQuestions),
    maplist(is_random_answer, RndAnsList). % Generate a list of N random answers
fill_random_answer(God, _, _) :-
    dif(God, random). % For non-random gods, the answer is unbound.
generate_worlds_from_templates([], _, []).
% [
%   pos(a, truly, _),
%   pos(b, random, true),
%   pos(c, falsely, _)
% ]
generate_worlds_from_templates([pos(P, God, _)|T], NumQs, [pos(P, God, RndAns)|W]) :-
    fill_random_answer(God, NumQs, RndAns),
    generate_worlds_from_templates(T, NumQs, W).

% --- Helper Predicates ---

% get_answer_path(Tree, World, PathOfAnswersFromTreeRoot, PathOfAnswers)
% Simulates the questioning process for a given tree and world.
get_answer_path(Tree, World, Path) :- get_answer_path_recursive(Tree, World, [], Path).
get_answer_path_recursive(leaf, _, _, []).
get_answer_path_recursive(tree(q(Pos, Q), YesT, NoT), World, PathSoFar, [Ans|Rest]) :-
    ( query_position(Pos, Q, PathSoFar, World) -> Ans = true ; Ans = fail ),
    (   Ans = true
    ->  get_answer_path_recursive(YesT, World, [true|PathSoFar], Rest)
    ;   get_answer_path_recursive(NoT, World, [fail|PathSoFar], Rest)
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
% --- The NEW, Efficient Pruning Algorithm ---

% This is the new main predicate. It replaces your old 'generate-and-test' version.
% Its job is to get the list of families and then call the real, recursive solver.
% --- The Wrapper Predicate ---
% --- The Wrapper Predicate ---
% It now passes NumQs in two roles: as the Total, and as the initial Depth.
% This is the wrapper predicate that you call from your tests.


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

     % 2. FIX: Remove the duplicates to get the true, unique set of families.
    my_nub(FamiliesWithDuplicates, UniqueFamilies),
    
    % --- Optional: Add this debug line to confirm the fix ---
    % write('DEBUG: Unique families to distinguish (count='), length(UniqueFamilies, L), writeln(L),
    
    % 3. Call the recursive pruning solver with the clean, unique list.
    find_pruning_tree(NumQs, NumQs, QComplexity, NumPos, UniqueFamilies, Tree).

% --- The Recursive Solver (Corrected Signature) ---
% Signature: find_pruning_tree(TotalNumQs, CurrentDepth, MaxQComp, NumPos, Families, Tree)

% --- Base Cases (use CurrentDepth) ---
find_pruning_tree(_, _, _, _, [], leaf).
find_pruning_tree(_, _, _, _, [_Family], leaf).
find_pruning_tree(_, 0, _, _, Families, leaf) :- % Base case for depth
    (length(Families, 1) -> true ; !, fail). % Ran out of depth

% --- Recursive Pruning Step (Corrected) ---
% --- The Recursive Solver (with DEBUG statements) ---
% --- The FINAL Recursive Pruning Step (with Debugging) ---
find_pruning_tree(TotalNumQs, CurrentDepth, MaxQComplexity, NumPos, Families, tree(q(Pos, Q), YesTree, NoTree)) :-
    CurrentDepth > 0,
    NextDepth is CurrentDepth - 1,

    % --- Iterative Deepening for Question Complexity ---
    between(0, MaxQComplexity, C),
    is_position(NumPos, Pos),
    is_question(NumPos, C, Q),

    % --- DEBUG: Print what we're trying ---
    length(Families, FamilyCount),
    % writeln(try(depth: CurrentDepth, q: (Pos,Q), families_in: FamilyCount)),

    partition_families(Families, TotalNumQs, q(Pos, Q), YesFamilies, NoFamilies),

    % --- DEBUG: Print the result of the partition ---
    length(YesFamilies, YesSize),
    length(NoFamilies, NoSize),
    % writeln(partition(yes_count: YesSize, no_count: NoSize)),

    % --- Pruning Check 1: Useless Split (Corrected) ---
    % Succeeds only if the split is useful. Allows backtracking on failure.
    \+ ( length(Families, YesSize) ; length(Families, NoSize) ),

    % --- Pruning Check 2: Sub-Problem Too Large (Corrected) ---
    % Succeeds only if the sub-problems are not too large. Allows backtracking on failure.
    MaxSize is 2^NextDepth,
    (   ( YesSize > MaxSize ; NoSize > MaxSize )
    ->  % This branch is taken if the sub-problem is too big
        % writeln(prune(reason: 'sub-problem too large')),
        fail % Just fail, allowing Prolog to backtrack to the 'between' loop
    ;   % This branch is taken if the check passes
        true
    ),

    % If we get here, the question was good. No commit cut is needed.
    % writeln(commit(q: (Pos,Q))),

    % --- Recurse ---
    find_pruning_tree(TotalNumQs, NextDepth, MaxQComplexity, NumPos, YesFamilies, YesTree),
    find_pruning_tree(TotalNumQs, NextDepth, MaxQComplexity, NumPos, NoFamilies, NoTree).

% --- Helpers required by the new algorithm ---

% Partitions families based on ALL their possible answers to a question.
% --- The Final, Corrected Partition Logic ---

% This new partition_families is much smarter.
% --- The Final, Corrected Partition Logic ---

partition_families(Families, NumQs, QuestionNode, YesFamilies, NoFamilies) :-
    % For each family, get its full signature set for this ONE question.
    maplist(get_single_question_signature(QuestionNode, NumQs), Families, Signatures),

    % Group families based on their signature for this question.
    % FIX: If signature contains true, it goes to YesFamilies.
    findall(F, (nth1(I, Families, F), nth1(I, Signatures, Sig), member(true, Sig)), YesFamilies),
    % FIX: If signature contains fail, it goes to NoFamilies.
    findall(F, (nth1(I, Families, F), nth1(I, Signatures, Sig), member(fail, Sig)), NoFamilies).

% This helper calculates the full set of answers a family can give for a single question.
get_single_question_signature(q(Pos, Q), NumQs, Family, SignatureSet) :-
    findall(Ans,
            (   generate_worlds_from_templates(Family, NumQs, World),
                ( query_position(Pos, Q, [], World) -> Ans=true ; Ans=fail )
            ),
            Answers),
    sort(Answers, SignatureSet).

% Succeeds if a family can EVER produce 'Answer' (true/fail) for the given question.
family_answers_question(q(Pos, Q), NumQs, Family, Answer) :-
    generate_worlds_from_templates(Family, NumQs, World),
    ( query_position(Pos, Q, [], World) -> Ans=true ; Ans=fail ), % We are at the root, so path is []
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
test('get_family_signature_set for "all truly" family is correct') :-
    Tree = tree(q(1, true), leaf, leaf),
    build_uniform_family(1, truly, Family),
    get_family_signature_set(Tree, /*num_questions=*/1, Family, [[true]]).

test('get_family_signature_set for "all falsely" family is correct') :-
    Tree = tree(q(1, true), leaf, leaf),
    build_uniform_family(1, falsely, Family),
    get_family_signature_set(Tree, /*num_questions=*/1, Family, [[fail]]).

test('get_family_signature_set for "all random" family is correct') :-
    Tree = tree(q(1, true), leaf, leaf),
    build_uniform_family(1, random, Family),
    get_family_signature_set(Tree, /*num_questions=*/1, Family, [[fail], [true]]).

test('2-question tree with "all truly" family has one outcome: [true, true]') :-
    Tree = tree(q(1, true), tree(q(1, true), leaf, leaf), tree(q(1, true), leaf, leaf)),
    build_uniform_family(1, truly, Family),
    get_family_signature_set(Tree, /*num_questions=*/2, Family, [[true, true]]).

test('2-question tree with "all falsely" family has one outcome: [fail, fail]') :-
    Tree = tree(q(1, true), tree(q(1, true), leaf, leaf), tree(q(1, true), leaf, leaf)),
    build_uniform_family(1, falsely, Family),
    get_family_signature_set(Tree, /*num_questions=*/2, Family, [[fail, fail]]).

test('2-question tree with "all random" family has all 4 possible outcomes') :-
    Tree = tree(q(1, true), tree(q(1, true), leaf, leaf), tree(q(1, true), leaf, leaf)),
    build_uniform_family(1, random, Family),
    ExpectedSet = [[fail, fail], [fail, true], [true, fail], [true, true]],
    get_family_signature_set(Tree, /*num_questions=*/2, Family, ExpectedSet).



:- end_tests(disjoint_logic).


:- begin_tests(distinguishing_scenarios).

test('1 question CAN distinguish [truly] from [falsely]') :-
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
test('family_answers: "Truly" family CAN answer true') :-
    build_uniform_family(1, truly, F_True), % Setup is now INSIDE the test
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = true,
    family_answers_question(Question, NumQs, F_True, ExpectedAnswer).

test('family_answers: "Truly" family CANNOT answer false', [fail]) :-
    build_uniform_family(1, truly, F_True),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = fail,
    family_answers_question(Question, NumQs, F_True, ExpectedAnswer).

test('family_answers: "Falsely" family CAN answer false') :-
    build_uniform_family(1, falsely, F_False),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = fail,
    family_answers_question(Question, NumQs, F_False, ExpectedAnswer).

test('family_answers: "Falsely" family CANNOT answer true', [fail]) :-
    build_uniform_family(1, falsely, F_False),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = true,
    family_answers_question(Question, NumQs, F_False, ExpectedAnswer).

test('family_answers: "Random" family CAN answer true') :-
    build_uniform_family(1, random, F_Rand),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = true,
    family_answers_question(Question, NumQs, F_Rand, ExpectedAnswer).

test('family_answers: "Random" family CAN answer false') :-
    build_uniform_family(1, random, F_Rand),
    Question       = q(1, true),
    NumQs          = 1,
    ExpectedAnswer = fail,
    family_answers_question(Question, NumQs, F_Rand, ExpectedAnswer).

% --- Tests for `partition_families/5` ---
test('partition: correctly splits [True, False] families') :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, falsely, F_False),
    In_Families     = [F_True, F_False],
    NumQs           = 1,
    Question        = q(1, true),
    partition_families(In_Families, NumQs, Question, YesFamilies, NoFamilies),
    assertion(YesFamilies = [F_True]),
    assertion(NoFamilies = [F_False]).

test('partition: correctly splits [True, Random] families') :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, random, F_Rand),
    In_Families     = [F_True, F_Rand],
    NumQs           = 1,
    Question        = q(1, true),
    partition_families(In_Families, NumQs, Question, YesFamilies, NoFamilies),
    assertion(YesFamilies = [F_True, F_Rand]),
    assertion(NoFamilies = [F_Rand]). % Random can also say 'fail' (No)

test('partition: correctly splits [False, Random] families') :-
    build_uniform_family(1, falsely, F_False),
    build_uniform_family(1, random, F_Rand),
    In_Families     = [F_False, F_Rand],
    NumQs           = 1,
    Question        = q(1, true),
    partition_families(In_Families, NumQs, Question, YesFamilies, NoFamilies),
    assertion(YesFamilies = [F_Rand]),
    assertion(NoFamilies = [F_False, F_Rand]). % Random can also say 'fail' (No)

test('partition: correctly splits [True, False, Random] families') :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, falsely, F_False),
    build_uniform_family(1, random, F_Rand),
    In_Families     = [F_True, F_False, F_Rand],
    NumQs           = 1,
    Question        = q(1, true),
    partition_families(In_Families, NumQs, Question, YesFamilies, NoFamilies),
    sort(YesFamilies, [F_Rand, F_True]),
    sort(NoFamilies, SortedNo),
    sort([F_False, F_Rand], ExpectedNo),
    assertion(SortedNo = ExpectedNo).

% --- Integration Tests for `find_pruning_tree/6` ---
test('pruning_tree: SUCCEEDS for [True, False] with 1 question') :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, falsely, F_False),
    TotalNumQs     = 1,
    CurrentDepth   = 1,
    MaxQComp       = 1,
    NumPos         = 1,
    Families       = [F_True, F_False],
    find_pruning_tree(TotalNumQs, CurrentDepth, MaxQComp, NumPos, Families, _Tree).

test('pruning_tree: FAILS for [True, Random] with 1 question', [fail]) :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, random, F_Rand),
    TotalNumQs     = 1,
    CurrentDepth   = 1,
    MaxQComp       = 1,
    NumPos         = 1,
    Families       = [F_True, F_Rand],
    find_pruning_tree(TotalNumQs, CurrentDepth, MaxQComp, NumPos, Families, _Tree).

test('pruning_tree: FAILS for [True, False, Random] with 2 questions (impossible split)', [fail]) :-
    build_uniform_family(1, truly, F_True),
    build_uniform_family(1, falsely, F_False),
    build_uniform_family(1, random, F_Rand),
    TotalNumQs     = 2,
    CurrentDepth   = 2,
    MaxQComp       = 2,
    NumPos         = 1,
    Families       = [F_True, F_False, F_Rand],
    find_pruning_tree(TotalNumQs, CurrentDepth, MaxQComp, NumPos, Families, _Tree).

test('pruning_tree: SUCCEEDS for a 2-position [Truly,Falsely] vs [Falsely,Truly] world') :-
    % This is a more complex scenario with 2 positions.
    % We need to distinguish two families:
    % Family 1: pos 1 is True, pos 2 is False
    % Family 2: pos 1 is False, pos 2 is True
    F1 = [pos(1, truly, _), pos(2, falsely, _)],
    F2 = [pos(1, falsely, _), pos(2, truly, _)],
    Families = [F1, F2],
    
    % We check if a 1-question tree with complexity 0 can find a solution.
    % (The question "true" asked at position 1 will work)
    find_pruning_tree(
        1, % TotalNumQs / Max Depth
        1, % CurrentDepth
        0, % MaxQComplexity
        2, % NumPos
        Families,
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

    % --- 2. The Test: Call the solver ---
    % We ask it to solve this 3-family problem with only 1 question.
    % The algorithm *should* fail because of Pruning Check 2.
    
    find_pruning_tree(
        TotalNumQs,   % TotalNumQs = 1
        TotalNumQs,   % CurrentDepth = 1
        0,            % MaxQComplexity = 0 (simple questions only)
        NumPos,       % NumPos = 1
        Families,
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

    % 4. Call the pruning solver directly with the debug statements active
    find_pruning_tree(
        NumQs,   % TotalNumQs
        NumQs,   % CurrentDepth
        QComplexity,
        NumPos,
        UniqueFamilies,
        _Tree).

:- end_tests(complex_pruning_scenario).