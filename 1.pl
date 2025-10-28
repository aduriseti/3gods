
% --- The World ---
query(truly, Question) :- Question.
query(falsely, Question) :- \+ Question.

at_position(a, truly).
at_position(a, falsely).

% a -> b ==== !a | b
query_position(Question, Position) :- 
        (   at_position(Position, truly), query(truly, Question)).
query_position(Question, Position) :- 
        (   at_position(Position, falsely), query(falsely, Question)).

% --- The Grammar ---
% Base Case 1: Trivial questions are allowed.
is_question(true).
is_question(fail).

% Base Case 2: Questions about the world are allowed.
is_question(at_position(a, truly)).
is_question(at_position(a, falsely)).

% Base Case 3: Questions about how gods might respond to questions are allowed.
is_question(query_position(Q, a)) :- is_question(Q).

% Recursive rules for AND and OR remain the same.
is_question((Q1, Q2)) :- is_question(Q1), is_question(Q2).
is_question((Q1 ; Q2)) :- is_question(Q1), is_question(Q2).

% --- The Logic ---

is_distinguishing_question(Question) :-
    is_question(Question),
	((   query_position(Question, a, truly), at_position(a, truly)) 
    	-> AnswerForTruly = true ; AnswerForTruly = fail),
    ((   query_position(Question, a, falsely), at_position(a, falsely)) 
    	-> AnswerForFalsely = true ; AnswerForFalsely = fail),
    AnswerForTruly \= AnswerForFalsely.

