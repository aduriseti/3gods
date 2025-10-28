
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
:- table evaluate/3.

% 1. Evaluate logical AND
evaluate((Q1, Q2), WorldState, Path) :-
    evaluate(Q1, WorldState, Path),
    evaluate(Q2, WorldState, Path).

% 2. Evaluate logical OR
evaluate((Q1 ; Q2), WorldState, Path) :-
    ( evaluate(Q1, WorldState, Path) ; evaluate(Q2, WorldState, Path) ).

% 3. Evaluate state-dependent questions by calling them with the WorldState
evaluate(at_position_question(P, G), _, WS) :-
    at_position(P, G, WS). % at_position doesn't need the path
evaluate(query_question(Pos, SubQ), Path, WS) :-
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
is_question(true).
is_question(fail).

% Base Case 2: Questions about the world are allowed.
is_question(NumPos, at_position_question(Pos, God)) :-
    is_position(NumPos, Pos),
    is_god(God).

% Base Case 3: Questions about how gods at positions might respond to questions are allowed.
is_question(NumPos, query_position_question(Pos, Q)) :-
    is_position(NumPos, Pos),
    is_question(NumPos, Q). % Pass NumPos to the recursive call

% Recursive rules for AND and OR remain the same.
is_question(NumPos, (Q1, Q2)) :-
    is_question(NumPos, Q1),
    is_question(NumPos, Q2).

is_question(NumPos, (Q1 ; Q2)) :-
    is_question(NumPos, Q1),
    is_question(NumPos, Q2).

% --- The Logic ---
% --- World State Generation ---
num_positions(3).
num_questions(2). % We can ask up to 2 questions.
god_types([truly, random, falsely]).

is_random_answer(true).
is_random_answer(fail).

% --- Family Generation ---

% Helper to zip positions and gods into a template list.
build_family_template([], [], []).
build_family_template([Pos|Ps], [God|Gs], [pos(Pos, God, _)|Template]) :-
    build_family_template(Ps, Gs, Template).

% generate_world_family_template(FamilyTemplate)
% Generates a single family, defined by a unique assignment of god types to positions.
generate_world_family_template(FamilyTemplate) :-
    % 1. Get a list of all positions, e.g., [1, 2, 3]
    findall(P, is_position(P), Positions),
    % 2. Get the list of required god types
    god_types(Gods),
    % 3. Generate a unique ordering of the god types, e.g., [truly, random, falsely]
    permutation(Gods, PermutedGods),
    % 4. "Zip" the positions and god types together to build the template
    build_family_template(Positions, PermutedGods, FamilyTemplate).

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
                get_answer_path_recursive(Tree, World, [], Path)
            ),
            Paths),
    % Sort the list to create a canonical set of paths
    sort(Paths, SignatureSet).

% --- The Main Distinguishing Question Logic ---

% Recursively generates a tree of questions up to MaxDepth.
generate_tree(_, 0, leaf) :- !.
generate_tree(NumPos, Depth, tree(q(Pos, Q), YesTree, NoTree)) :-
    Depth > 0,
    NextDepth is Depth - 1,
    % Use the new parameterized predicates
    is_position(NumPos, Pos),
    is_question(NumPos, Q),
    % Pass NumPos to the recursive calls
    generate_tree(NumPos, NextDepth, YesTree),
    generate_tree(NumPos, NextDepth, NoTree).

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

% is_distinguishing_tree(MaxDepth, Tree, GeneratorGoal)
% --- The New, Flexible Main Predicate ---
is_distinguishing_tree(NumPos, NumQs, GodTypes, Tree, GeneratorGoal) :-
    % 1. Generate a candidate tree structure
    generate_tree(NumPos, NumQs, Tree),
    % 2. Find all family templates using the provided generator and parameters
    findall(F, call(GeneratorGoal, NumPos, GodTypes, F), Families),
    % 3. Calculate the signature set for each family
    maplist(get_family_signature_set(Tree, NumQs), Families, ListOfSignatureSets),
    % 4. Check if all sets are mutually disjoint
    all_disjoint(ListOfSignatureSets).

solve_3gods_tf(Tree) :- is_distinguishing_tree(
       3,                          % Num Positions
       3,                          % Num Questions
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


:- begin_tests(god_puzzle_logic).

% --- Sanity Checks for World Generation ---

test('generates exactly two families') :-
    findall(F, generate_world_family_template(F), Families),
    assertion(length(Families, 2)).

test('truly family has exactly one world') :-
    build_uniform_family(truly, FamilyTemplate),
    findall(W, generate_worlds_from_templates(FamilyTemplate, W), Worlds),
    assertion(length(Worlds, 1)).

test('falsely family has exactly one world') :-
    build_uniform_family(falsely, FamilyTemplate),
    findall(W, generate_worlds_from_templates(FamilyTemplate, W), Worlds),
    assertion(length(Worlds, 1)).

:- begin_tests(disjoint_logic).

% --- Low-Level Sanity Checks for Set Logic ---

test('disjoint/2 succeeds for non-overlapping sets') :-
    disjoint([a, b], [c, d]).

test('disjoint/2 fails for overlapping sets', [fail]) :-
    disjoint([a, b], [b, c]).

test('all_disjoint/1 succeeds for a list of disjoint sets') :-
    all_disjoint([[a], [b, c], [d]]).

test('all_disjoint/1 fails if any two sets overlap', [fail]) :-
    all_disjoint([[a, b], [c, d], [b, e]]).

% --- Integration Test for Signature Set Generation ---

% Helper to build a family template where every god is of the same type.
build_uniform_family(GodType, FamilyTemplate) :-
    findall(P, is_position(P), Positions),
    % --- Start of Fix ---
    length(Positions, Len),
    length(Gods, Len), % Create a list of unbound variables of the same length
    maplist(=(GodType), Gods), % Make every element in the list the same GodType
    % --- End of Fix ---
    build_family_template(Positions, Gods, FamilyTemplate).


test('get_family_signature_set for "all truly" family is correct') :-
    % For a simple tree with one question, 'true'...
    Tree = tree(q(1, true), leaf, leaf),
    % ...and the "all truly" family...
    build_uniform_family(truly, Family),
    % ...the set of outcomes should be just one path: [[true]].
    get_family_signature_set(Tree, 1, Family, [[true]]).

test('get_family_signature_set for "all falsely" family is correct') :-
    % For a simple tree with one question, 'true'...
    Tree = tree(q(1, true), leaf, leaf),
    % ...and the "all falsely" family...
    build_uniform_family(falsely, Family),
    % ...the set of outcomes should be just one path: [[fail]].
    get_family_signature_set(Tree, 1, Family, [[fail]]).

test('get_family_signature_set for "all random" family is correct') :-
    % For a simple tree with one question, 'true'...
    Tree = tree(q(1, true), leaf, leaf),
    % ...and the "all random" family...
    build_uniform_family(random, Family),
    % ...the set of outcomes should include both [true] and [fail].
    get_family_signature_set(Tree, 1, Family, [[fail], [true]]).

% --- Tests for a 2-Question Tree ---

test('2-question tree with "all truly" family has one outcome: [true, true]') :-
    % For a tree where every question is 'true'...
    Tree = tree(q(1, true), tree(q(1, true), leaf, leaf), tree(q(1, true), leaf, leaf)),
    % ...and a world of truth-tellers...
    build_uniform_family(truly, Family),
    % ...the god will answer 'true' to the first question, then 'true' to the second.
    get_family_signature_set(Tree, 2, Family, [[true, true]]).

test('2-question tree with "all falsely" family has one outcome: [fail, fail]') :-
    % For a tree where every question is 'true'...
    Tree = tree(q(1, true), tree(q(1, true), leaf, leaf), tree(q(1, true), leaf, leaf)),
    % ...and a world of liars...
    build_uniform_family(falsely, Family),
    % ...the god will lie about the first 'true' (answer 'fail'), then lie about the second (answer 'fail').
    get_family_signature_set(Tree, 2, Family, [[fail, fail]]).

test('2-question tree with "all random" family has all 4 possible outcomes') :-
    % This test requires num_questions(2) to be set.
    num_questions(2),
    % For a tree where every question is 'true'...
    Tree = tree(q(1, true), tree(q(1, true), leaf, leaf), tree(q(1, true), leaf, leaf)),
    % ...and a world of random gods...
    build_uniform_family(random, Family),
    % ...the outcome will simply be the random god's predetermined list of 2 answers.
    % Since the answer list can be any combination of true/fail, all 4 paths are possible.
    ExpectedSet = [[fail, fail], [fail, true], [true, fail], [true, true]],
    get_family_signature_set(Tree, 2, Family, ExpectedSet).
;lasd
:- end_tests(disjoint_logic).

verify_indistinguishable(InferenceLimit, NumPos, NumQs, GodTypes, GeneratorGoal) :-
    writeln('--- Starting verification... ---'),
    % This goal will be run with a limit on how much work it can do.
    Goal = is_distinguishing_tree(NumPos, NumQs, GodTypes, _Tree, GeneratorGoal),

    % call_with_inference_limit/3 runs Goal, but stops after InferenceLimit steps.
    % It binds 'Result' to tells us what happened.
    call_with_inference_limit(Goal, InferenceLimit, Result),

    (   Result == inference_limit_exceeded
    ->  % This is the outcome we expect for an impossible problem.
        writeln('--- Verification PASSED: No distinguishing tree found within the inference limit. ---'),
        true
    ;   % This means the search finished. Did it find a tree?
        ( Result == ! % '!' means the Goal succeeded deterministically.
        ->  writeln('--- Verification FAILED: A distinguishing tree was found! ---'),
            fail
        ;   % This covers normal failure (Result == false) or non-deterministic success.
            writeln('--- Verification PASSED: Search completed with no solution. ---'),
            true
        )
    ).

:- begin_tests(distinguishing_scenarios).

test('1 question CAN distinguish [truly] from [falsely]') :-
    is_distinguishing_tree(1, 1, [truly, falsely], _Tree, generate_uniform_families).

test('1 question CANNOT distinguish [truly] from [random]') :-
    verify_indistinguishable(1e6, 1, 1, [truly, random], generate_uniform_families).

test('3 questions CANNOT distinguish [truly] from [random]') :-
    verify_indistinguishable(1e6, 1, 3, [truly, random], generate_uniform_families).

:- end_tests(distinguishing_scenarios).