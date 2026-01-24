% paradox/paradox_utils.pl

:- module(paradox_utils, [
    draw_tree/1,
    draw_tree/2,
    render_question/2,
    render_question_short/2,
    all_disjoint/1,
    disjoint/2
]).

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

% --- Visualization ---

draw_tree(Tree) :-
    draw_tree(Tree, human).

draw_tree(Tree, Mode) :-
    writeln(''),
    draw_tree_rec(Tree, "", Mode).

draw_tree_rec(leaf, _Prefix, _Mode) :-
    writeln('  -> SOLVED').

draw_tree_rec(tree(q(Pos, Q), DaTree, JaTree, SilentTree), Prefix, Mode) :-
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
    format('~w| (ja)~n', [Prefix]),
    draw_tree_rec(JaTree, NextPrefix, Mode),

    % Silent Branch
        format('~w\\ (silent)~n', [Prefix]), draw_tree_rec(SilentTree, NextPrefix, Mode).
    

% Catch-all for unexpected tree structures
draw_tree_rec(Other, Prefix, _Mode) :-
    format('~wERROR: Unrecognized tree structure: ~w~n', [Prefix, Other]).

% Compact question renderer for the ASCII tree
render_question_short(at_position_question(P, G), S) :-
    format(string(S), "Is G~w ~w", [P, G]).
render_question_short(query_position_question(P, SubQ), S) :-
    render_question_short(SubQ, SubS),
    format(string(S), "If G~w asked '~s', say da", [P, SubS]).
render_question_short((Q1, Q2), S) :-
    render_question_short(Q1, S1),
    render_question_short(Q2, S2),
    format(string(S), "(~s AND ~s)", [S1, S2]).
render_question_short((Q1 ; Q2), S) :-
    render_question_short(Q1, S1),
    render_question_short(Q2, S2),
    format(string(S), "(~s OR ~s)", [S1, S2]).
render_question_short((Q1 xor Q2), S) :-
    render_question_short(Q1, S1),
    render_question_short(Q2, S2),
    format(string(S), "(~s XOR ~s)", [S1, S2]).
render_question_short(true, "True").
render_question_short(fail, "False").

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