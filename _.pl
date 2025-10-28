% --- Small fix in your world generator ---
% The call to is_random_answer/1 should be separate.
fill_random_answer(random, RndAns) :- is_random_answer(RndAns).
fill_random_answer(God, _) :- dif(God, random). % For non-random gods, the answer is unbound.

generate_worlds_from_templates([], []).
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


% --- The Main Distinguishing Question Logic ---

is_distinguishing_question(Question, Position) :-
    % 1. Generate a candidate question and position.
    is_question(Question),
    is_position(Position),

    % 2. Test the candidate by trying to find a counterexample.
    % The question is distinguishing IFF no counterexample exists.
    \+ counterexample_exists(Question, Position).


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
    Family1 \== Family2, % Ensure they are not the same family template
    family_signature(Question, Position, Family1, Signature),
    family_signature(Question, Position, Family2, Signature). % Fails if they collide