answer(da).
answer(ja).
answer(either).

valid_answers(Answers, QuestionsLeft) :-
    length(Answers,6),
    maplist(answer, Answers),
    count_occurrences(Answers, da, DaCount),
    count_occurrences(Answers, ja, JaCount),
    count_occurrences(Answers, either, EitherCount),
    DaCount + EitherCount =< 2^(QuestionsLeft-1),
    JaCount + EitherCount =< 2^(QuestionsLeft-1).

count_valid_answers(Count) :-
    findall(Answers, valid_answers(Answers, 3), AllAnswers),
    length(AllAnswers, Count).

count_occurrences(List, Element, Count) :-
    include(=(Element), List, SubList),
    length(SubList, Count).