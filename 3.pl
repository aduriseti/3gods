
% --- The World ---
% --- Core Query Logic ---

is_position(a).

is_god(truly).
is_god(falsely).
is_god(random).

query(truly, Question, WorldState) :-
    evaluate(Question, WorldState).
query(falsely, Question, WorldState) :-
    \+ evaluate(Question, WorldState).
% Random god ignores question and positions of other gods, its answer is predetermined by WorldState compound term
query(random, _, (_, RandomAnswer)) :- RandomAnswer.

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

% at_position(Pos, God, (God, RandomAnswer)) :- is_position(Pos), is_god(God).
at_position(_, God, (God, _)).

% a -> b ==== !a | b
query_position(Position, Question, WorldState) :- 
        (   at_position(Position, God, WorldState), query(God, Question, WorldState)).

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

is_random_answer(true).
is_random_answer(fail).

% Generates all worlds in the "truly" family. The random answer is irrelevant
% but included for consistent structure.
is_truly_world((truly, RandomAnswer)) :-
    is_random_answer(RandomAnswer).

% Generates all worlds in the "random" family.
is_random_world((random, RandomAnswer)) :-
    is_random_answer(RandomAnswer).


% --- The Logic to Find Distinguishing Questions ---

is_distinguishing_question(Question, TrulyFamilyAnswer, RandomFamilyAnswer) :-
    % 1. Generate a potential question
    is_question(Question),

    % 2. Find the consistent answer for the 'truly' family
    is_truly_world(FirstTrulyWorld),
    ( query_position(a, Question, FirstTrulyWorld) -> TrulyFamilyAnswer = true ; TrulyFamilyAnswer = fail ),
    % Verify this answer is the same for all other 'truly' worlds
    \+ ( is_truly_world(OtherTrulyWorld),
         ( query_position(a, Question, OtherTrulyWorld) -> OtherAnswer = true ; OtherAnswer = fail ),
         OtherAnswer \= TrulyFamilyAnswer ),

    % 3. Find the consistent answer for the 'random' family
    is_random_world(FirstRandomWorld),
    ( query_position(a, Question, FirstRandomWorld) -> RandomFamilyAnswer = true ; RandomFamilyAnswer = fail ),
    % Verify this answer is the same for all other 'random' worlds
    \+ ( is_random_world(OtherRandomWorld),
         ( query_position(a, Question, OtherRandomWorld) -> OtherAnswer = true ; OtherAnswer = fail ),
         OtherAnswer \= RandomFamilyAnswer ),

    % 4. Ensure the two families produce different answers
    TrulyFamilyAnswer \= RandomFamilyAnswer.



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