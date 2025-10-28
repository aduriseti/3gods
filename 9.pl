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
:- table evaluate/3.

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
evaluate(false, _, _) :- false.

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
is_random_answer(false).

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

% --- The Final Heuristic Search Algorithm ---

find_best_question(Worlds, MaxQComplexity, NumPos, BestQuestion, BestScore) :-
    % FIX: The goal now generates both P and Q
    findall(q(P, Q),
            ( is_position(NumPos, P),
              is_question(NumPos, 0, Q)
            ),
            BaseQuestionNodes),
    find_best_in_list(Worlds, BaseQuestionNodes, BestBaseQ, BestBaseScore),
    improve_question(Worlds, MaxQComplexity, 1, NumPos, [BestBaseQ], BestBaseScore, BestQuestion, BestScore).

improve_question(_, MaxQ, CurrentComp, _, [BestQSoFar|_], BestScoreSoFar, BestQSoFar, BestScoreSoFar) :-
    CurrentComp > MaxQ, !.

improve_question(Worlds, MaxQComp, CurrentComp, NumPos, PromisingQs, BestScoreSoFar, FinalQ, FinalScore) :-
    % 1. Find all new, more complex questions by composing our current best ones.
    findall(q(Pos, NewQ),
            (   % Option A: Compose two promising questions
                member(q(_, Q1), PromisingQs),
                member(q(_, Q2), PromisingQs),
                compose(Q1, Q2, NewQ),
                is_position(NumPos, Pos) % Assign a position
            ;   % Option B: Compose a position and a promising question
                is_position(NumPos, Pos),
                member(q(_, Q1), PromisingQs),
                compose(Pos, Q1, NewQ)
            ),
            NewCompositions),
    
    % 2. Find the best of these new compositions.
    (   find_best_in_list(Worlds, NewCompositions, BestNewQ, BestNewScore)
    ->  % 3. Decide if the new composition is an improvement.
        (   BestNewScore < BestScoreSoFar
        ->  % It's an improvement! Continue searching from this new, better starting point.
            NextComp is CurrentComp + 1,
            improve_question(Worlds, MaxQComp, NextComp, NumPos, [BestNewQ|PromisingQs], BestNewScore, FinalQ, FinalScore)
        ;   % It's not an improvement. The best we found at the previous level is the winner.
            member(BestQSoFar, PromisingQs),
            FinalQ = BestQSoFar,
            FinalScore = BestScoreSoFar
        )
    ;   % No valid new compositions were found. The previous best is the winner.
        member(BestQSoFar, PromisingQs),
        FinalQ = BestQSoFar,
        FinalScore = BestScoreSoFar
    ).


% --- Helpers for the Heuristic Search (Corrected) ---

% The composition grammar
compose(Q1, Q2, (Q1, Q2)) :- Q1 @< Q2.
compose(Q1, Q2, (Q1 ; Q2)) :- Q1 @< Q2.
compose(Pos, Q, query_question(Pos, Q)).

% Scores a question based on how well it partitions a set of worlds.
% --- The Corrected, Smarter Scoring Predicate ---

score_question(Worlds, q(Pos, Q), Score) :-
    % 1. Partition the worlds as before.
    findall(W, (member(W, Worlds), answer_for_world(Q, Pos, W, true)),  YesWorlds),
    findall(W, (member(W, Worlds), answer_for_world(Q, Pos, W, false)), NoWorlds),

    % 2. Count the number of UNIQUE FAMILIES in each resulting group.
    family_count(YesWorlds, YesFamilyCount),
    family_count(NoWorlds, NoFamilyCount),

    % 3. FIX: The score is the size of the LARGER FAMILY group.
    Score is max(YesFamilyCount, NoFamilyCount).

% NEW: Helper predicate to create the Key-Value pair for keysort.
score_question_with_q(Worlds, QuestionNode, Score-QuestionNode) :-
    score_question(Worlds, QuestionNode, Score).

% Finds the best-scoring question in a list.
find_best_in_list(Worlds, QuestionNodes, BestQ, BestScore) :-
    % FIX: Call the new helper to create a list of Score-Question pairs.
    maplist(score_question_with_q(Worlds), QuestionNodes, ScoredQs),
    % keysort can now correctly sort the list of pairs.
    keysort(ScoredQs, [BestScore-BestQ | _]).

% This is the main logic, now using the bounded tree generator.
% --- The NEW, Efficient Pruning Algorithm ---

% --- Self-Contained nub/2 to replace the broken library version ---
my_nub([], []).
my_nub([H | T], [H | T2]) :-
    \+ memberchk(H, T),
    my_nub(T, T2).
my_nub([H | T], T2) :-
    memberchk(H, T),
    my_nub(T, T2).

% --- New Helper to Count Families in a Set of Worlds ---

% Converts a concrete world back to its family template by blanking out random answers.
world_to_family_template(World, FamilyTemplate) :-
    maplist(world_pos_to_template_pos, World, FamilyTemplate).
world_pos_to_template_pos(pos(P, G, _RndAns), pos(P, G, _)).

% Counts the number of unique families represented in a list of worlds.
family_count(Worlds, Count) :-
    maplist(world_to_family_template, Worlds, Templates),
    sort(Templates, UniqueTemplates), % sort/2 removes duplicates
    length(UniqueTemplates, Count).

% Add this complete predicate definition to your file:
answer_for_world(Question, Pos, World, Answer) :-
    ( query_position(Pos, Question, [], World) -> Answer = true ; Answer = false ).

% --- Configurable Logging ---

% Set the desired log level.
% 0 = off, 1 = minimal (commits), 2 = verbose (tries/partitions), 3 = debug (full details)
log_level(2).

% Helper predicate to print logs based on level.
log(Level, Message) :-
    log_level(CurrentLevel),
    Level =< CurrentLevel,
    !, % The cut prevents backtracking
    writeln(Message).
log(_, _). % This clause does nothing if the level is too low.

% --- The New Solver (Based on Your Logic) ---
% find_distinguishing_tree(CurrentDepth, MaxQComp, Worlds, Tree)
% --- The Heuristic Solver (with Configurable Logging) ---

% --- Base Cases (with logging) ---
find_distinguishing_tree(_, _, _, Worlds, leaf) :-
    family_count(Worlds, 1),
    log(1, success(reason:'Base case: 1 family left')),
    !.
find_distinguishing_tree(0, _, _, Worlds, leaf) :-
    family_count(Worlds, Count),
    Count > 1,
    log(1, failure(reason:'Base case: Ran out of depth')),
    !, fail.

% --- Recursive Step (with logging) ---
find_distinguishing_tree(Depth, MaxQComplexity, NumPos, Worlds, tree(q(Pos, Q), YesTree, NoTree)) :-
    length(Worlds, WorldCount),
    log(2, enter_solver(depth:Depth, world_count:WorldCount)),
    Depth > 0,
    NextDepth is Depth - 1,

    % 1. Find the single most promising question.
    log(3, '--> Finding best question...'),
    find_best_question(Worlds, MaxQComplexity, NumPos, q(Pos, Q), Score),
    log(2, found_best_question(score:Score, q:q(Pos,Q))),

    % 2. Partition the worlds using this best question.
    log(3, '--> Partitioning worlds...'),
    findall(W, (member(W, Worlds), answer_for_world(Q, Pos, W, true)),  YesWorlds),
    findall(W, (member(W, Worlds), answer_for_world(Q, Pos, W, false)), NoWorlds),

    % 3. Compute family counts for the resulting sets.
    family_count(YesWorlds, YesFamilyCount),
    family_count(NoWorlds, NoFamilyCount),
    family_count(Worlds, TotalFamilyCount),
    log(2, partition_counts(total:TotalFamilyCount, yes:YesFamilyCount, no:NoFamilyCount)),

    % --- Pruning Checks (with logging) ---
    (   \+ ( YesFamilyCount == TotalFamilyCount ; NoFamilyCount == TotalFamilyCount )
    ->  log(3, '--> Prune 1 (Useless Split): PASSED')
    ;   log(2, prune(reason:'Useless Split')), fail
    ),
    
    MaxSize is 2^NextDepth,
    (   \+ ( YesFamilyCount > MaxSize ; NoFamilyCount > MaxSize )
    ->  log(3, '--> Prune 2 (Sub-problem Too Large): PASSED')
    ;   log(2, prune(reason:'Sub-problem Too Large')), fail
    ),
    
    !, % Commit to this question
    log(1, commit(depth:Depth, q:q(Pos,Q))),

    % 4. Recurse.
    find_distinguishing_tree(NextDepth, MaxQComplexity, NumPos, YesWorlds, YesTree),
    find_distinguishing_tree(NextDepth, MaxQComplexity, NumPos, NoWorlds, NoTree).

solve_puzzle(NumPos, NumQs, QComplexity, GodTypes, Tree, Generator) :-
    % 1. Generate all families.
    findall(F, call(Generator, NumPos, GodTypes, F), Families),
    % 2. Expand all families into one giant list of all possible worlds.
    findall(W, (member(F, Families), generate_worlds_from_templates(F, NumQs, W)), AllWorlds),
    my_nub(AllWorlds, UniqueWorlds), % Ensure we start with a clean set
    % 3. Call the new, simpler solver.
    find_distinguishing_tree(NumQs, QComplexity, NumPos, UniqueWorlds, Tree).

solve_3gods_tf(Tree) :- solve_puzzle(
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

:- begin_tests(heuristic_search_logic).

% --- Test for `score_question/3` (The Foundation) ---

test('score_question: identifies a perfect split') :-
    % A world of [T] vs [F] should be perfectly split by q(1,true).
    % Total=2 worlds, Yes=1, No=1. Score = max(1,1) = 1.
    Worlds = [[pos(1,truly,[])], [pos(1,falsely,[])]],
    score_question(Worlds, q(1, true), 1).

test('score_question: identifies a useless split') :-
    % A world of [T] vs [T] cannot be split by q(1,true).
    % Total=2 worlds, Yes=2, No=0. Score = max(2,0) = 2.
    Worlds = [[pos(1,truly,[])], [pos(1,truly,[])]],
    score_question(Worlds, q(1, true), 2).

% --- Test for `find_best_in_list/4` (The Selection) ---

test('find_best_in_list: correctly selects the best of two questions') :-
    Worlds = [[pos(1,truly,[])], [pos(1,falsely,[])]],
    % q(1,true) gives a perfect score of 1.
    % q(1,falsely) is also a perfect split with a score of 1.
    % q(1,at_position_question(1,falsely)) is useless, score = 2.
    % keysort is stable, so it should pick the first best question.
    Questions = [q(1, at_position_question(1,falsely)), q(1, true)],
    find_best_in_list(Worlds, Questions, q(1, true), 1).

% --- Test for `improve_question/8` (The Heuristic Loop) ---

test('improve_question: correctly stops when it finds a perfect score') :-
    Worlds = [[pos(1,truly,[])], [pos(1,falsely,[])]],
    % Start with a perfect question.
    BestBaseQ = q(1, true),
    BestBaseScore = 1,
    % Call improve_question. Since no composition can be better than a
    % perfect score of 1, it should immediately return the same question.
    improve_question(
        Worlds,             % Context
        2,                  % MaxQComplexity
        1,                  % CurrentComplexity
        1,                  % NumPos
        [BestBaseQ],        % Promising Questions
        BestBaseScore,      % Best Score So Far
        FinalQuestion,      % Output Question
        FinalScore          % Output Score
    ),
    assertion(FinalQuestion == BestBaseQ),
    assertion(FinalScore == BestBaseScore).

:- end_tests(heuristic_search_logic).

:- begin_tests(find_distinguishing_tree).

% find_distinguishing_tree(Depth, MaxQComplexity, NumPos, Worlds, tree(q(Pos, Q), YesTree, NoTree))

test('find_dist_tree can distinguish [T,T] from [F,F]') :-
    % --- Manual Setup ---
    NumPos = 2,
    NumQs = 1,

    % 1. Define families
    build_uniform_family(NumPos, truly, F_True),
    build_uniform_family(NumPos, falsely, F_False),

    % 2. Generate concrete worlds
    generate_worlds_from_templates(F_True, NumQs, World_T),
    generate_worlds_from_templates(F_False, NumQs, World_F),

    % 3. Combine into the final list
    AllWorlds = [World_T, World_F],

    % --- The Test ---
    % Call the subcomponent directly with the prepared data
    find_distinguishing_tree(
        NumQs,         % Depth
        1,             % MaxQComplexity
        NumPos,
        AllWorlds,
        _Tree).



:- end_tests(find_distinguishing_tree).

:- begin_tests(distinguishing_scenarios).

test('1 question CAN distinguish [truly] from [falsely]') :-
    solve_puzzle(1, 1, 1, [truly, falsely], _Tree, generate_uniform_families).

test('truly with 2 positions is distinguishable by default even with 0 questions]') :-
    solve_puzzle(
           2, % Num Positions
           0, % Tree Depth (Num Questions)
           0, % Max Question Complexity
           [truly, truly], % The multiset of gods for the combination
           _Tree,
           generate_uniform_families 
       ).

test('Exhaustive search proves [truly] vs [random] is indistinguishable for 1 Q^1 question') :-
    % We are asserting that the following goal MUST FAIL.
    % The '\+' operator succeeds if its argument fails completely.
    \+ solve_puzzle(
           1, % Num Positions
           1, % Tree Depth (Num Questions)
           1, % Max Question Complexity
           [truly, random],
           _Tree,
           generate_uniform_families
       ).

test('Exhaustive search proves [truly] vs [random] is indistinguishable for 3 Q^0 question') :-
    % We are asserting that the following goal MUST FAIL.
    % The '\+' operator succeeds if its argument fails completely.
    \+ solve_puzzle(
           1, % Num Positions
           3, % Tree Depth (Num Questions)
           0, % Max Question Complexity
           [truly, random],
           _Tree,
           generate_uniform_families
       ).

test('Exhaustive search proves [truly] vs [random] is indistinguishable for 3 Q^3 question') :-
    % We are asserting that the following goal MUST FAIL.
    % The '\+' operator succeeds if its argument fails completely.
    \+ solve_puzzle(
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
    \+ solve_puzzle(
           3, % Num Positions
           1, % Tree Depth (Num Questions)
           1, % Max Question Complexity (e.g., nesting one level deep)
           [truly, falsely, random],
           _Tree,
           generate_permutation_families
       ).

test('[truly,truly,falsely] distinguishable w/ 2 Q^1 questions') :-
    solve_puzzle(
           3, % Num Positions
           2, % Tree Depth (Num Questions)
           1, % Max Question Complexity (e.g., nesting one level deep)
           [truly, truly, falsely],
           _Tree,
           generate_permutation_families
       ).

test('Exhaustive search proves [truly,truly,falsely] in-distinguishable w/ 1 Q^4 questions') :-
    % We are asserting that the following goal MUST FAIL.
    % The '\+' operator succeeds if its argument fails completely.
    \+ solve_puzzle(
           3, % Num Positions
           1, % Tree Depth (Num Questions)
           4, % Max Question Complexity (e.g., nesting one level deep)
           [truly, truly, falsely],
           _Tree,
           generate_permutation_families
       ).

test('[truly,falsely,random] distinguishable w/ 3 Q^6 questions') :-
    solve_puzzle(
           3, % Num Positions
           3, % Tree Depth (Num Questions)
           6, % Max Question Complexity (e.g., nesting one level deep)
           [truly, falsely, random],
           _Tree,
           generate_permutation_families
       ).

:- end_tests(distinguishing_scenarios).

:- begin_tests(pruning_logic).

% find_distinguishing_tree(Depth, MaxQComplexity, NumPos, Worlds, tree(q(Pos, Q), YesTree, NoTree))

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