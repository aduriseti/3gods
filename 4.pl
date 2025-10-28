
% --- The World ---
% --- Core Query Logic ---

% This rule now generates all positions from 1 to N.
is_position(P) :-
    num_positions(N),
    between(1, N, P).

is_god(truly).
is_god(falsely).
is_god(random).

query(truly, Question, WorldState, _) :-
    evaluate(Question, WorldState).
query(falsely, Question, WorldState, _) :-
    \+ evaluate(Question, WorldState).
% Random god ignores question and positions of other gods, its answer is predetermined by WorldState compound term
query(random, _, _, RandomAnswer) :- RandomAnswer.

% --- The Evaluator ---

% evaluate(Question, WorldState)
% This predicate handles each type of question from your grammar explicitly.

% 1. Evaluate logical AND
evaluate((Q1, Q2), WorldState) :-
    evaluate(Q1, WorldState),
    evaluate(Q2, WorldState).

% 2. Evaluate logical OR
evaluate((Q1 ; Q2), WorldState) :-
    ( evaluate(Q1, WorldState) ; evaluate(Q2, WorldState) ).

% 3. Evaluate state-dependent questions by calling them with the WorldState
evaluate(at_position_question(P, G), WS) :- at_position(P, G, WS).
evaluate(query_position_question(P, Q), WS) :- query_position(P, Q, WS).

% 4. Evaluate base cases (these don't depend on the WorldState)
evaluate(true, _) :- true.
evaluate(fail, _) :- fail.

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
query_position(Position, Question, WorldState) :-
    % Find the specific pos(...) term for the given Position.
    member(pos(Position, GodType, RandomAnswer), WorldState),
    % Call the updated query/4 predicate with all necessary info.
    query(GodType, Question, WorldState, RandomAnswer).

% --- The Grammar ---
% Base Case 1: Trivial questions are allowed.
is_question(true).
is_question(fail).

% Base Case 2: Questions about the world are allowed.
is_question(at_position_question(Pos, God)) :- is_position(Pos), is_god(God).

% Base Case 3: Questions about how gods at positions might respond to questions are allowed.
is_question(query_position_question(Pos, Q)) :- is_position(Pos), is_question(Q).

% Recursive rules for AND and OR remain the same.
is_question((Q1, Q2)) :- is_question(Q1), is_question(Q2).
is_question((Q1 ; Q2)) :- is_question(Q1), is_question(Q2).

% --- The Logic ---
% --- World State Generation ---

num_positions(3). % Let's use 3 positions for this example.
god_types([truly, random, falsely]).

is_random_answer(true).
is_random_answer(fail).

% --- Family Generation ---

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

% Helper to zip positions and gods into a template list.
build_family_template([], [], []).
build_family_template([Pos|Ps], [God|Gs], [pos(Pos, God, _)|Template]) :-
    build_family_template(Ps, Gs, Template).

% --- World Generation from a Family Template ---

% generate_worlds_from_templates(FamilyTemplate, ConcreteWorld)
% Takes a template and generates a concrete world by filling in random answers.
fill_random_answer(random, RndAns) :- is_random_answer(RndAns).
fill_random_answer(God, _) :- dif(God, random). % For non-random gods, the answer is unbound.
generate_worlds_from_templates([], []).
% [
%   pos(a, truly, _),
%   pos(b, random, true),
%   pos(c, falsely, _)
% ]
generate_worlds_from_templates([pos(P, God, _)|Template], [pos(P, God, RndAns)|World]) :-
    fill_random_answer(God, RndAns),
    generate_worlds_from_templates(Template, World).


% --- Helper Predicates ---

% Determines the answer for a given question in a single, concrete world.
answer_for_world(Question, Pos, World, Answer) :-
    ( query_position(Pos, Question, World) -> Answer = true ; Answer = fail ).

% Determines the consistent answer (Signature) for an entire family.
% It fails if the family is not internally consistent.
family_signature(Question, Pos, Family, Signature) :-
    % Get the answer for the first world to use as a reference.
    generate_worlds_from_templates(Family, FirstWorld),
    answer_for_world(Question, Pos, FirstWorld, Signature),
    % Now, verify that NO other world in the family has a different answer.
    \+ ( generate_worlds_from_templates(Family, OtherWorld),
         answer_for_world(Question, Pos, OtherWorld, OtherAnswer),
         OtherAnswer \= Signature ).


% A counterexample exists if any family is internally inconsistent OR
% if two different families have the same consistent answer.

counterexample_exists(Question, Position) :-
    % Fails if there is a family that is NOT internally consistent.
    generate_world_family_template(Family),
    \+ family_signature(Question, Position, Family, _).

counterexample_exists(Question, Position) :-
    % Fails if two different families produce the same signature.
    generate_world_family_template(Family1),
    generate_world_family_template(Family2),
    Family1 \= Family2, % Ensure they are not the same family template
    family_signature(Question, Position, Family1, Signature),
    family_signature(Question, Position, Family2, Signature). % Fails if they collide


% --- The Main Distinguishing Question Logic ---

is_distinguishing_question(Question, Position) :-
    % 1. Generate a candidate question and position.
    is_question(Question),
    is_position(Position),

    % 2. Test the candidate by trying to find a counterexample.
    % The question is distinguishing IFF no counterexample exists.
    \+ counterexample_exists(Question, Position).



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