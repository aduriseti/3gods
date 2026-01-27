:- use_module(library(plunit)).
:- consult('paradox/paradox.pl').
:- assert(current_log_level(info)).

:- begin_tests(debug_distinct).

test('Generate universe for complexity 1 and count distinct signatures') :-
    findall(F, generate_permutation_families(3, [truly, falsely, random], F), Families),
    my_nub(Families, UniqueFamilyTemplates),
    maplist(wrap_family_in_candidate([da_yes, da_no]), UniqueFamilyTemplates, UniqueFamilies),
    
    call_with_time_limit(20, generate_universe(3, 1, UniqueFamilies, 3)),
    
    call_with_time_limit(10, predicate_property(distinct_q(_,_,_), number_of_clauses(Count))),
    writeln(distinct_count_comp1(Count)),
    
    writeln('--- ALL DISTINCT QUESTIONS ---'),
    forall(distinct_q(Q, Sig, C), format('C=~w Q=~w Sig=~w~n', [C, Q, Sig])),
    
    assertion(Count > 11).

:- end_tests(debug_distinct).
