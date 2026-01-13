
% --- The World ---
% --- Core Query Logic ---
is_position(a).

is_god(truly).
is_god(falsely).

query(truly, Question, WorldState) :-
    evaluate(Question, WorldState).

% The 'falsely' clause is simply the negation of the 'truly' clause.
query(falsely, Question, WorldState) :-
    \+ evaluate(Question, WorldState).

% --- The Evaluator ---
% helpers
at_position(Pos, God, God) :- is_position(Pos), is_god(God).

query_position(Position, Question, WorldState) :- 
        (   at_position(Position, GodType, WorldState), query(GodType, Question, WorldState)).

% evaluate(Question, WorldState)
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
is_distinguishing_question(Question, AnswerForTruly, AnswerForFalsely) :-
    is_question(Question),
	(query_position(a, Question, truly)
    	-> AnswerForTruly = true ; AnswerForTruly = fail),
    (query_position(a, Question, falsely)
    	-> AnswerForFalsely = true ; AnswerForFalsely = fail),
    AnswerForTruly \= AnswerForFalsely,
    render_question(Question, RenderedQuestion),
    writeln("\n"), writeln(RenderedQuestion).

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
