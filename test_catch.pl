test_catch_timeout(Result) :-
    catch(
        call_with_time_limit(1, loop),
        error(time_limit_exceeded, _),
        (Result = timeout_was_caught)
    ).

loop :- loop.