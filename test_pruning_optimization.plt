
:- use_module(library(plunit)).
:- ensure_loaded('random_silent.pl').

:- begin_tests(pruning_optimization).

test('Optimization Verification: Cut prevents Ja branch if Da fails') :-
    % 1. Clear State
    init_stats,
    retractall(user:distinct_q(_, _, _)),
    retractall(user:seen_signature(_)),

    % 2. Mock the Universe with 2 Questions
    % Q1: "If asked G1 'Is G1 truly', say da" (Complex)
    % This question effectively splits candidates by language/god, so it passes the "Useless Split" check.
    % But we expect it to fail at depth 2 (exhausted).
    Q1 = query_position_question(1, at_position_question(1, truly)),
    assertz(user:distinct_q(Q1, sig([], []), 1)),

    % Q2: "True" (Simple) - Just to have a second option
    assertz(user:distinct_q(true, sig([], []), 0)),

    % 3. Setup Candidates (Standard 3 God problem)
    NumPos = 3,
    GodTypes = [truly, falsely, random],
    findall(F, generate_permutation_families(NumPos, GodTypes, F), FamiliesTemplates),
    % Ensure no duplicates
    sort(FamiliesTemplates, UniqueTemplates),
    maplist(wrap_family_in_candidate([da_yes, da_no]), UniqueTemplates, Candidates),

    % 4. Run Solver (Expect Failure/Exhaustion for Q1, then Q2)
    % We limit depth to 2 to ensure Q1 fails quickly at the leaves.
    % MaxComplexity 1 to allow Q1.
    
    (   find_pruning_tree(3, 3, 1, 3, Candidates, Candidates, _Tree)
    ->  true
    ;   true % Expected to fail or succeed, we just want the stats
    ),

    % 5. Verify Statistics
    % Check if we entered the [da] branch
    (user:path_stats(explored, [da], E_Da) -> true ; E_Da = 0),
    % Check if we entered the [ja] branch
    (user:path_stats(explored, [ja], E_Ja) -> true ; E_Ja = 0),

    format('~n--- Optimization Test Stats ---~n', []),
    format('Explored [da]: ~w~n', [E_Da]),
    format('Explored [ja]: ~w~n', [E_Ja]),

    % Assertion: We must have tried 'da' (Q1 passed root check)
    assertion(E_Da > 0),
    
    % Assertion: We must NOT have tried 'ja' (Cut should have prevented it after Da failed)
    % Note: If Q2 also tried 'da' and failed, we still wouldn't see 'ja'.
    % If Q2 *succeeded* 'da', it would try 'ja'.
    % But 'true' question fails "Useless Split" check immediately (everyone says da or ja).
    % So Q2 shouldn't even enter 'da'.
    
    assertion(E_Ja == 0).

:- end_tests(pruning_optimization).
