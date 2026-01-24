% paradox/paradox_utils.plt

:- use_module(library(plunit)).
:- use_module(paradox_utils).

:- begin_tests(paradox_utils).

test(render_question_short_true) :-
    render_question_short(true, "True").

test(render_question_short_fail) :-
    render_question_short(fail, "False").

test(render_question_short_at_position) :-
    render_question_short(at_position_question(1, truly), "Is G1 truly").

test(render_question_short_nested) :-
    render_question_short(query_position_question(2, true), "If G2 asked 'True', say da").

test(render_question_complex) :-
    render_question((at_position_question(1, truly), at_position_question(2, falsely)), Output),
    sub_string(Output, _, _, _, "Is the god at position `1` the `truly` god?"),
    sub_string(Output, _, _, _, "Is the god at position `2` the `falsely` god?").

:- end_tests(paradox_utils).
