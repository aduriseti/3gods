this_is_a_fact.

this_is_a_parameterized_fact(param).

% Let's specify some facts about the sky.
the_color_of_the_sky_is(blue, at_morning).
the_color_of_the_sky_is(red, at_sunset).
the_color_of_the_sky_is(black, at_night).

% We can trigger evaluation with the query operator `?-'.
% Uppercase variables are "unified" with values by Prolog's engine.
% `?- the_color_of_the_sky_is(Color, at_sunset).` - Unifies `red` with `Color`.

% Anything which isn't reducible to facts is considered false.
% `?- the_color_of_the_sky_is(red, at_dawn)` - this is false.
% `?- the_color_of_the_sky_is(purple, at_sunset)` - this is also false.

% The `:-` operator allows us to derive facts from other facts.
% We can query for a time of day that we need a flashlight (night) w/:
% `?- i_need_a_flashlight_at(T).` - this will unify `T` with `night`.
i_need_a_flashlight_at(Time) :- the_color_of_the_sky_is(black, Time).
    
% The OR operator is `;`.
i_can_see_at(Time) :- 
    the_color_of_the_sky_is(blue, Time) ; 
    the_color_of_the_sky_is(red, Time).

% The AND operator is `,`. The NOT operator is `\+`.
its_too_early_for_me(at_morning).
i_enjoy(Time) :-
    i_can_see_at(Time),
    \+ its_too_early_for_me(Time).