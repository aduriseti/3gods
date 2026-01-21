# The Hardest Solution to the Hardest Logic Puzzle Ever

Here's an easy and very simple riddle:

> There are 3 gods in front of you, one who always answers Truly, one who always answers Falsely, and one who always answers Randomly. Your goal is to determine the identity of each god using 3 yes/no questions. Oh - and the gods answer in their own language although they understand English. So they will answer with "ja"/"da" - and you don't know which means "yes" and which means "no".

Go on - take a few minutes to solve it (if the answer isn't obvious already).

...

Ahh - don't feel too bad - I couldn't help myself. Figuring this one out took me longer than I care to admit.  Famously - this is the so-called "Hardest Logic Puzzle Ever" (<https://en.wikipedia.org/wiki/The_Hardest_Logic_Puzzle_Ever>). If you want to solve this puzzle on your own, don't worry - keep reading - I'm not going to discuss any solutions. Instead, I will talk about my formal method of solving this problem with logic programming - which AFAIK no one has done before.

I decided to go with Prolog. Prolog is a declarative programming language (like SQL, or I guess TensorFlow v1?) as opposed to imperative programming languages like Python/C++/go/rust/etc... (also TensorFlow v2). What you do is specify known facts about the world (e.g. sky is blue, I am looking at the sky) and let Prolog's engine solve for unknowns (The color of the thing I am looking as is \_\_\_\_ --- and in this case Prolog would fill in `blue` for the blank). Here, our unknown that Prolog solves for would be the sequence of questions we use to solve this riddle.

<blockquote>
<details id="prolog-details" open markdown="1">
<summary> <b> <em> Click if have never seen Prolog before or want a refresher on its syntax. </em> </b> </summary>

This Wikipedia section is a good intro to the language: <https://en.wikipedia.org/wiki/Prolog#Syntax_and_semantics>.

If you don't want to read all that - here's a 2-minute crash course:

<iframe id="swish-iframe"
        src="https://swish.swi-prolog.org/?code=https://raw.githubusercontent.com/aduriseti/3gods/main/crash_course.pl&q=i_enjoy(Time)." 
        width="100%" 
        height="600px">
</iframe>

</details>

<script>
  // Collapse this section once playground loads.
  document.getElementById('swish-iframe').onload = function() {
    setTimeout(function() {
      document.getElementById('prolog-details').removeAttribute('open');
    }, 1000);
    
  };
</script>
</blockquote>

In this write-up I'll guide you through how I wrote this solver, roughly retracing the evolution of my implementation and thought process. I'll start by describing how to use logic programming to solve a much simpler problem. Then, I'll discuss how to extend this approach to the full 3-gods problem. Finally, I'll explain necessary optimizations.

* TOC
{:toc}

## Knight or Knave
*<https://en.wikipedia.org/wiki/Knights_and_Knaves>*

Let's first see how we might use Prolog to solve a much simpler problem.

Say there is a single god in front of us, who may be either the Truly god (a Knight) or Falsely god (a Knave) - and we want to determine its identity in 1 question.  As a human - easy - just ask the god a trivially true question like "Is 1 == 1?". If they say yes - they are the Truly god - if they say no they are the Falsely god. 

For Prolog to discover this same question - we need:
1. Constraints defining the world (a single god, who may be of 2 different types, each of which who answer questions differently)
2. Constraints on questions - here we define a grammar. For this world we only need the atomic trivially true question mentioned earlier, but we can also add some other grammar rules. For example, composition via `AND` and `OR` operators, etc... 
3. Constraints on how questions are evaluated by our world. This is where we encode our objective - which is that our desired question can differentiate the possible worldstates (the god in front of us is Truly or Falsely).

What does this actually look like?

World constraints are very simple - we have a single position, and it may be inhabited by either the Truly or Falsely god. We can therefore represent a worldstate using a list with a single element indicating the type of this single god.

```prolog
is_position(a).

is_god(truly).
is_god(falsely).

% at_position(Pos, GodType, WorldState).
% We leverage Prolog's deconstruction feature to extract `GodType` from
% `WorldState`.
at_position(Pos, GodType, [GodType]) :-
  is_position(Pos),
  is_god(GodType).
```

Similarly, our question grammar can be very simple. We only need an "atomic" true question - which we can imagine as "Is 1==1?" or some other axiomatic statement. But that's a little boring - let's at least give our Prolog program a chance to fail and ask a useless question. For example - "Will the god at position `a` answer the question "1==1" with yes?". The Truly - god - truthfully - will say yes. The Falsely god - deceitfully - will also say yes.
```prolog
% Axiomatically true questions are allowed.
is_question(true).

% Questions about how gods at positions might respond to questions are allowed.
is_question(query_position_question(Pos, Question)) :-
  is_position(Pos),
  is_question(Question).
```

This question grammar requires a corresponding evaluation framework - for this we define an evaluation predicate `evaluate(Question, WorldState)`, which gives the correct answer to `Question` for the given `WorldState`. Here - `WorldState` 
```prolog
% Note that axiomatic questions do not require the worldstate for evaluation
evaluate(true, _) :- true.

% (`query_position` is the predicate which evaluates the result of posing a
% question to a God. It is explained in more detail below - because we allow
% self-referential questions explaining this code creates a bootstrapping
% problem :O)
evaluate(query_position_question(Position, Question), WorldState) :-
  query_position(Position, Question, WorldState).
```

We encode the different behavior of the gods by how they evaluate questions - the Truly god returns the logical value of the proposition 
represented by a question as evaluated by Prolog's engine - the Falsely god returns its negation:
```prolog
query(truly, Question, WorldState) :-
    evaluate(Question, WorldState).

% (\+ is the negation operator in Prolog)
query(falsely, Question, WorldState) :-
    \+ evaluate(Question, WorldState).
```

We also need a way to pose these questions to gods at a specific position:
```prolog
query_position(Position, Question, WorldState) :- 
        at_position(Position, GodType, WorldState),
        query(GodType, Question, WorldState).
```

Finally, we can synthesize the constraints above with a predicate that forces the 2 possible worldstates (the god in front of you is Truly or Falsely) to give different responses for a question. If invoked with an unconstrained question this predicate will produce all distinguishing questions (you can cycle through all results with `;`).
```prolog
is_distinguishing_question(Question, AnswerForTruly, AnswerForFalsely) :-
    is_question(Question),
	(query_position(a, Question, [truly])
    	-> AnswerForTruly = true ; AnswerForTruly = fail),
    (query_position(a, Question, [falsely])
    	-> AnswerForFalsely = true ; AnswerForFalsely = fail),
    AnswerForTruly \= AnswerForFalsely.
```

You can play around with this simplified problem and its solution in the embedded playground below ([full screen](https://swish.swi-prolog.org/?code=https://raw.githubusercontent.com/aduriseti/3gods/main/knight_or_knave.pl&q=is_distinguishing_question(Q,_,_).)):

<iframe src="https://swish.swi-prolog.org/?code=https://raw.githubusercontent.com/aduriseti/3gods/main/knight_or_knave.pl&q=is_distinguishing_question(Question,_,_)." 
        width="100%" 
        height="600px">
</iframe>

## Extending to the full 3-gods puzzle
Ok - so how do we extend the approach above to the full 3-gods problem? Retracing my steps - we need support for
1. The random god
2. Multiple gods (multiple positions) & multiple questions
3. The language of the gods ("ja"/"da")

### Random god
Implementing the random god I accomplished by encoding all possible future answers of the random god into the worldstate. When we pose a question to a random god, instead of evaluating that question, we look up a random answer from its set of predetermined random responses. 
```prolog
% Using the number prior questions posed (`Path`), we select the corresponding
% random answer from `RndAnsList`. So if we have asked 2 questions so far, we
% select the 3rd result. If we have asked no questions so far, we select the 1st
% result - etc... Also note that `_Question` and `_Worldstate` (which stores
% the arrangement of the gods) are ignored.
query(random, _Question, Path, _WorldState, RndAnsList) :-
    length(Path, NumPreviousAnswers),
    CurrentQuestionNum is NumPreviousAnswers + 1,
    nth1(CurrentQuestionNum, RndAnsList, Answer),
    Answer. % Succeeds if the Nth answer is 'true'
```

<blockquote>
<details markdown="1">
<summary> <b> <em> Click for a discussion of alternate interpretations of the Random god. </em> </b> </summary>

Note that this is not the only possible interpretation of how the random god answers questions. The original wording of this puzzle describes the behavior of the random god as:

> Three gods A, B, and C are called, in no particular order, True, False, and Random. True always speaks truly, False always speaks falsely, **but whether Random speaks truly or falsely is a completely random matter**.

In my implementation, I interpreted this as - the response of the random god is randomly "yes" or "no" (actually "ja" and "da"). However - another possible interpretation is that the Random god's response must be randomly true or false. The difference is subtle, but there are questions that can be posed for which is not possible to answer "no" either truly or falsely. An example would be the useless question earlier: "Would you answer the question "1==1" with yes?". For such questions, depending on your interpretation of the Random god, it may not be able to answer "no". In fact, if you assume that the Random gods answer must be either true or false, this puzzle is a lot easier and can even be answered in 2 questions (although you have to use paradoxical questions) - see [this paper](https://www.researchgate.net/publication/31366417_A_simple_solution_to_the_hardest_logic_puzzle_ever) for more details.

I decided to interpret this puzzle as:

> Three gods A, B, and C are called, in some order, ‘True’, ‘False’, and ‘Random’. True always speaks truly, False always speaks falsely, but **whether Random answers ‘ja’ or ‘da’ is a completely random matter**.

And therefore allow the random god to potentially answer questions neither truly nor falsely.

</details>
</blockquote>

### Multiple gods & questions
Multiple gods is easy - we can just parameterize our `is_position` clause and extend our `WorldState` from a list of a single element to a list of multiple (3) elements.
```prolog
is_position(P) :-
    num_positions(N),
    between(1, N, P).

num_positions(3).

% `WorldState` is a list of `pos` objects containing a position and a god type.
% For example, 
% ```
% [
%   pos(1, truly),
%   pos(2, falsely),
%   pos(3, random),
% ]
% ```
% We check if a god `GodType` is at a particular position `Position` by
% performing a membership check on `pos(Position, GodType)`.
at_position(Position, GodType, WorldState) :-
    member(pos(Position, GodType), WorldState).
```

The biggest change is that we go from 2 possible worldstates (where the god in front of you is either Truly or Falsely) to 6 - for all permutations of 3 distinct gods A, B, C. Clearly - we now need multiple questions to differentiate these possible worldstates.

Note that if we are allowed multiple questions (3 for this puzzle), we don't just have to find $N$ questions - we have to find $2^N-1$ questions to build up a tree (see diagram below). Nodes will represent the act of posing a question $Q$ to a god in a specific position $P$. Responses to these questions ("ja"/"da") will be edges.

Our goal in constructing this tree is that by the time we reach a leaf node (having asked our last question), only a single possible worldstate (permutation of gods) will have been able to give the sequence of answers that navigates to this leaf node (e.g. "ja, da, da"). This process is illustrated below for a hypothetical question tree which successfully solves the riddle.

<pre class="mermaid">
flowchart TD
    %% --- Initial State ---
    Root["`All 6 Permutations
    {TFR, TRF, FTR, FRT, RTF, RFT}`"]
    
    %% --- Question 1 Action ---
    Q1_Act[/"Ask Q1
    to P1?"/]
    
    Root --> Q1_Act

    %% --- Split 1 ---
    Q1_Act -->|da| Left["{TRF, FRT, RTF, RFT}"]
    Q1_Act -->|ja| Right["{TFR, FTR, RTF, RFT}"]


    %% --- Question 2 & 3 Actions ---
    Q2_Act[/"Ask Q2
    to P2?"/]
    Q3_Act[/"Ask Q3
    to P3?"/]

    Left --> Q2_Act
    Right --> Q3_Act

    %% --- Split 2 ---
    Q2_Act -->|da| L_Da["{TRF, RTF}"]
    Q2_Act -->|ja| L_Ja["{FRT, RFT}"]

    Q3_Act -->|da| R_Da["{FTR, RTF}"]
    Q3_Act -->|ja| R_Ja["{TFR, RFT}"]
    

    %% --- Final Question Actions ---
    Q4_Act[/"Ask Q4
    to P4?"/]
    Q5_Act[/"Ask Q5
    to P5?"/]
    Q6_Act[/"Ask Q6
    to P6?"/]
    Q7_Act[/"Ask Q7
    to P7?"/]

    L_Da --> Q4_Act
    L_Ja --> Q5_Act
    R_Da --> Q6_Act
    R_Ja --> Q7_Act


    %% --- Final Splits to Solutions ---
    Q4_Act -->|da| Sol1(Solved: TRF)
    Q4_Act -->|ja| Sol2(Solved: RTF)
    
    Q5_Act -->|da| Sol3(Solved: FRT)
    Q5_Act -->|ja| Sol4(Solved: RFT)

    Q6_Act -->|da| Sol5(Solved: FTR)
    Q6_Act -->|ja| Sol6(Solved: RTF)
    
    Q7_Act -->|da| Sol7(Solved: TFR)
    Q7_Act -->|ja| Sol8(Solved: RFT)
    
    %% --- Styling ---
    classDef leaf fill:#d4edda,stroke:#333,stroke-width:2px;
    class Sol1,Sol2,Sol3,Sol4,Sol5,Sol6,Sol7,Sol8 leaf;

    %% Styled the parallelograms
    classDef question fill:#fffacd,stroke:#daa520,stroke-width:1px,stroke-dasharray: 5 5;
    class Q1_Act,Q2_Act,Q3_Act,Q4_Act,Q5_Act,Q6_Act,Q7_Act question;
</pre>

### Language of the gods
Accommodating the language of the gods was the easiest extension - all it required was adding a translation layer to our predicate posing questions to gods where we map `fail/true` to "ja" and "da" based on a hidden language member of `WorldState`: 
```prolog
get_utterance(true, da_yes, da).
get_utterance(fail, da_yes, ja).
get_utterance(true, da_no, ja).
get_utterance(fail, da_no, da).

query_position(Position, Question, WorldState, Language, Utterance) :-
    ... % Otherwise unchanged.
    % Map Logical Result to Utterance based on Language
    get_utterance(LogicalAns, Language, Utterance).
```

## Necessary Optimizations

### Bounding question grammar complexity
Now - if we were willing to wait infinitely long for our program to run, we would be done - but if you tried to solve the three gods puzzle with the Prolog program as described so far you would find it just hangs (possibly) indefinitely. This shouldn't be too surprising, after all, Prolog's engine uses naive depth first search and backtracking to explore our question grammar, which is unconstrained and allows for questions of arbitrary complexity.

So our first step is to add complexity bounds to our grammar limiting how many times we can compose and translate questions into more complex questions. Here is an example for a couple of our grammar rules:
```prolog
% Questions about how gods at positions might respond to questions.
is_question(NumPos, MaxQDepth, query_position_question(Pos, Q)) :-
    MaxQDepth > 0, % Only recurse if we have budget
    NextQDepth is MaxQDepth - 1,
    is_position(NumPos, Pos),
    is_question(NumPos, NextQDepth, Q).

% Recursive rules for AND.
is_question(NumPos, MaxQDepth, (Q1, Q2)) :-
    MaxQDepth > 0,
    NextQDepth is MaxQDepth - 1,
    is_question(NumPos, NextQDepth, Q1),
    is_question(NumPos, NextQDepth, Q2).
```

### Question deduplication 
Unfortunately - although we've guaranteed our program will exhaustively search all possible solutions using questions under a given complexity bound in finite time - in practice this program does not terminate in reasonable amount of time (I didn't have a strict time limit, but I was aiming for about 1 minute). 

In fact, I was even unable to exhaustively expand my question grammar for **a single question even for complexity bound 2**. This test below would consistently timeout:
```prolog
test("Exhaustive search proves [truly] vs [random] is indistinguishable
     for one complexity 2 (Q^2) question.") :-
    % We are asserting that the following goal MUST FAIL.
    % The '\+' operator succeeds if its argument fails completely.
    call_with_time_limit(10, \+ is_distinguishing_tree_bounded(
           1, % Num Positions
           1, % Tree Depth (Num Questions)
           2, % Max Question Complexity
           [truly, random],
           _Tree,
           generate_uniform_families
       )).
```

I was a little surprised - but - upon some thought I realized this made sense.
- We have $11$ base cases for questions
  - $2$ atomic questions (axiomatically true and false)
  - $3 * 3$ cases for questions about specific god types being at specific questions 
- For a set of questions of complexity $C$ with size $N$ - we have $3 + 3*N^2$ ways to produce new questions of complexity $C+1$
  - For each question $Q$, we can make $3$ new ones with questions of the form "Would the god at position $p$ respond to $Q$ with "da"?". I guess technically we could also produce conjugate questions for "ja", but in practice my grammar didn't do this.
  - For each pair of questions $Q_i, Q_j$, we can produce new questions $Q_i \land Q_j$, $Q_i \lor Q_j$, $Q_i \oplus Q_j$. (I added a rule for XOR because my solution to the riddle used it as a conditional inversion operator, even though the standard solutions online don't use it)

So - putting this together we have
- $14$ questions of complexity $0$ ($Q^0$ questions)
- $3 + 3*11^2 = 336$ questions of complexity $1$ ($Q^1$ questions)
- $3 + 3*336^2 = 400k$ questions of complexity $2$ ($Q^2$ questions)

I'm still a little surprised Prolog can't evaluate half a million questions in 10 seconds, but - oh well - can't argue with a hanging terminal.

> **Question complexity notation:** From now on I denote a question $Q$ of complexity $C$ as $Q^C$.

Now - it turns out we don't need $Q^2$ questions to solve this puzzle - $Q^1$ questions are fine. But one of my goals here was to make a more general solver for these types of puzzles. As a followup - for example - I might be interested in adding a puzzle grammar to find puzzles that can't be solved using $Q^1$ questions and instead require $Q^2$ or even $Q^3$ questions. So I think this is a problem that needs to be solved.

This stumped me for a long time - I went down some truly labyrinthine rabbit holes. I didn't want to use heuristics to constrain my grammar (although I did try). If this solver doesn't find a solution for a puzzle, I want to be sure that a solution doesn't exist, as opposed to wondering if my heuristic just pruned it. I had an interesting idea about co-generating puzzles and questions - this would allow me to find questions that "reduce" a puzzle to a union of more simple sub-puzzles. The advantage here is that once we solve a sub-puzzle - we don't have to keep evaluating questions and running expensive grammar expansions over its set of worldstates.  I still think there's something useful there, but I couldn't make it work. And - anyways work got busy. So I gave up a for a couple months.

Eventually my girlfriend returned from visiting family and asked about whatever happened to my stupid little Prolog project. I was walking her through the approaches I tried and as we talked we began to think about what makes a useful question. And specifically how we could prevent our grammar from mindlessly composing useless questions with useless questions, creating a combinatorial explosion of useless questions. We realized (and I really do think we jointly came up with this idea) that the domain of "useful" questions for this puzzle world is fairly limited. We only have 6 possible permutations of gods, and each permutation can only answer a question one of 3 ways:
1. ja
2. da
3. or either - since the question could've been posed to the Random god

We can say a question is useful if the different permutations of gods answer give a unique sequence of answers. Let's call this sequence the "logical signature" of a question. If we deduplicated on logical signature as we expanded our question grammar - we would end up producing $3^6=729$ questions - way less than $1e6$!

Here are some logical signatures for a couple example questions when posed to the first god in front of us:

|  | **God Permutation:** | `TFR` | `TRF` | `FTR` | `FRT` | `RTF` | `RFT` |
| :---- | ----- | :---: | :---: | :---: | :---: | :---: | :---: |
| **Question:** |  |  |  |  |  |  |  |
| *Is 1==1?* |  | *either* | *either* | *either* | *either* | *either* | *either* |
| *Is 1==0?* |  | *either* | *either* | *either* | *either* | *either* | *either* |
| *If I asked "is 1==1" <br> would you answer da?* |  | da | da | da | da | *either* | *either*  |

<blockquote>
<details markdown="1">
<summary> <b> <em> Q: Why can the Truly god answer "Is 1==1?" with either "da" or "ja"? </em> </b> </summary>
A: The language of the gods is unconstrained. So either "da" or "ja" can mean "yes.
</details>
</blockquote>

Note that the signature for "Is 1==1?" is the same as the signature for "Is 1==0?" - and we would deduplicate these questions against each other when expanding our grammar.

I found that not only did this greatly speed up question generation (we could produce all useful $Q^2$ questions in less than 1 second) we could even produce every possible useful question using just $Q^3$ questions! (And do so in less than 30 seconds.) So - in a sense - we were able to exhaustively search our question grammar despite it being infinite.

### Early question tree pruning
Now - there's one more optimization I implemented. You may remember that a full question tree for 3 questions has 7 nodes. Even with our greatly reduced question domain - $729^7$ is waaaaayyy too many question trees. So we also implemented some tree pruning ensuring we stopped exploring trees when a subnode contained too many permutations to solve with our remaining questions. 

For example, let's say we pick *"If I asked "is 1==1" would you answer da?"* as our starting question and ask it to the god in the first position. What are the possible responses for each permutation of the gods?
- TFR: "da"
- TRF: "da"
- FTR: "da"
- FRT: "da"
- RTF: either "ja" or "da" - depending on the mental state of the Random god
- RFT: ditto

So now, while we only have 2 possible worldstates that can answer "ja", we have 6 worldstates that can answer "da" that we now have to distinguish with only 2 questions (a max of 4 is possible). Therefore, we can discard this question and all trees deriving from it. This scenario is illustrated in the diagram below:

<pre class="mermaid">
flowchart TD
    Root["`All 6 Permutations
    {TFR, TRF, FTR, FRT, RTF, RFT}`"]
    
    Q1_Act[/"Ask P1:
    'If I asked is 1==1, would you say da?'"/]
    
    Root --> Q1_Act

    Q1_Act -->|da| Left["`{TFR, TRF, FTR, FRT, RTF, RFT}
    UNSOLVABLE:
    6 Remaining > 4 Capacity`"]

    Q1_Act -->|ja| Right["`{RTF, RFT}
    SOLVABLE:
    2 Remaining <= 4 Capacity`"]

    %% --- Styling ---
    
    %% 1. Question Node (Yellow/Dashed Parallelogram)
    classDef question fill:#fffacd,stroke:#daa520,stroke-width:1px,stroke-dasharray: 5 5;
    class Q1_Act question;

    %% 2. Solvable Node (Green)
    classDef leaf fill:#d4edda,stroke:#333,stroke-width:2px;
    class Right leaf;

    %% 3. Unsolvable Node (Red/Error with Dashed Border)
    classDef prune fill:#ffcccc,stroke:#f00,stroke-width:2px,stroke-dasharray: 5, 5;
    class Left prune;
</pre>

How effective is this pruning? I added some telemetry to my Prolog puzzle to analyze exploration/pruning statistics when solving the full riddle and found that we pruned all but a couple of questions at each level our question tree. One thing that surprised me is how few questions we actually explore at each depth level. We don't get anywhere close to exploring all 729 possible questions (although in practice its 365 because I use conjugate symmetry to cut down the question space). Which - come to think of it - is probably why this problem is tractable at all in Prolog.
```txt
│ INFO: --- SEARCH STATISTICS ---                                                                                                                                                                                                                                                                                    │
│ INFO: Note: Tree Level 1 is the Root.                                                                                                                                                                                                                                                                              │
│ INFO: Level | Explored | Pruned | Ratio                                                                                                                                                                                                                                                                            │
│ INFO: -----------------------------------                                                                                                                                                                                                                                                                          │
│ INFO:   1   |    26   |   24   | 0.9230769230769231                                                                                                                                                                                                                                                                │
│ INFO:   2   |    351   |   347   | 0.9886039886039886                                                                                                                                                                                                                                                              │
│ INFO:   3   |    176   |   152   | 0.8636363636363636                                                                                                                                                                                                                                                              │
│ INFO: -----------------------------------                                                                                                                                                                                                                                                                          │
│ INFO: TOTAL | 553 | 523 | 0.945750452079566
```

I tried repeating this analysis for an unsolvable puzzle to get an idea of how many questions would be pruned if we fully explore our grammar - but what I found is that all questions would be instantly pruned for unsolvable problems. I do wonder if this optimization scales to different puzzles though - I could imagine an antagonistically constructed puzzle that causes the number of question trees passing this check to explode. Maybe an additional optimization or even an entirely different approach would be needed in this case.

## Solver Demo
***SPOILER WARNING!: Skip this section if you don't want the solution to the puzzle ruined!***

I think it's time to show off the solver.

You can execute it in the playground below or use this [direct link](https://swish.swi-prolog.org/?code=https://raw.githubusercontent.com/aduriseti/3gods/main/final_solution.pl&q=solve_and_print_riddle.) to go to SWISH.

*NOTE: sometimes the SWISH sandbox kills my solver because it is too heavy. If that happens just run it locally: <https://www.swi-prolog.org/download/stable.>*

<iframe src="https://swish.swi-prolog.org/?code=https://raw.githubusercontent.com/aduriseti/3gods/main/final_solution.pl&q=solve_and_print_riddle." 
        width="100%" 
        height="600px">
</iframe>

## Next Steps
I have few ideas for future work.

### Solving this puzzle in 2 questions
First, I think that it is probably possible to solve this puzzle in 2 questions if we allow paradoxical questions. These would be questions like - "Will you answer this question with something meaning "yes"?". Let's say the Falsely god answers with "no" (in the god's language) - well - then it told the truth and violated its nature. But if the god answers `yes` - it has also told the truth and violated its nature. So it is impossible for the Falsely god to answer this question and it must remain silent. 

Since it is now possible to get three different responses to a question ("da"/"ja"/silence), in 2 questions we can now differentiate 9 worldstates - more than the 6 possible permutations of the gods. Or at least that's the idea.

Someone explored this idea in [a paper](https://www.researchgate.net/publication/31366417_A_simple_solution_to_the_hardest_logic_puzzle_ever) and came to the same conclusion as me. But there was a [later paper](https://www.researchgate.net/publication/225580608_Why_the_Hardest_Logic_Puzzle_Ever_Cannot_Be_Solved_in_Less_than_Three_Questions) which provided a proof for why this puzzle is unsolvable with 2 questions even when using paradoxical questions. Having read the proof - I'm unconvinced - but since I have the ability to exhaustively search the question grammar for this puzzle - this is something I can easily and conclusively verify.

### Puzzle generation
Second, I think we could create a grammar to produce new puzzles. A simple such grammar would be one with 2 rules
1. Add a new position (or god). This would let us go from 3 to 4 gods. Or 4 to 5 etc...
2. Add a new allowed god type $T$ at a position $P$. Let's say previously only the Truly god was allowed at that position. This rule could then allow additionally the Falsely or Random gods.

Once we can search over the puzzle domain, we could look for and classify categories of puzzles. For example, we could find puzzles that require complexity 2 ($Q^2$) questions - or complexity 3 questions ($Q^3$) questions.

### SMT solving
Third, I think solving this puzzle really pushes the limits of what is possible in Prolog. If we want to solve much harder problems (as discussed above), we would probably need to switch to a more powerful version of logic programming - specifically a SMT solver.

I think that this could be as simple as converting the program to Datalog (<https://en.wikipedia.org/wiki/Datalog>), but it might be as involved as rewriting the solver in Z3 (<https://en.wikipedia.org/wiki/Z3_Theorem_Prover>). It also might not be possible - I don't think I'm using any SMT incompatible formulation (my question grammar is finite) but I might be wrong.

## Appendix
<details markdown="1">
<summary>Click if you are curious about how my solution evolved over time</summary>

Here are some rough notes of my progress along with links to my solver in intermediate states.

- [1.pl](https://github.com/aduriseti/3gods/blob/main/1.pl)
  - Establishes approach of "distinguishing question" - allows us to find questions that can distinguish a single god who may be of types truly or falsely (although this doesn't work b/c the questions are not evaluated in the context of worldstate)
- [2.pl](https://github.com/aduriseti/3gods/blob/main/2.pl)
  - Supplies worldstate to question evaluation - previously question evaluation unconstrained by worldstate
  - This is actually roughly the approach outlined in the "Knight or Knave" section above
- [3.pl](https://github.com/aduriseti/3gods/blob/main/3.pl)
  - Adds random god - random gods answers are modeled as predetermined states upon world construction
  - Of course - the truly and random gods are indistinguishable - so this program always hangs
- [4.pl](https://github.com/aduriseti/3gods/blob/main/4.pl)/[5.pl](https://github.com/aduriseti/3gods/blob/main/5.pl)
  - Adds multiple positions (3) to our world - previously there was only a single god in front of us
  - There are no distinguishing questions for 3 gods (we need multiple questions)
- [6.pl](https://github.com/aduriseti/3gods/blob/main/6.pl)
  - Establishes approach of generating trees to partition families - but hangs when solving full complexity 3 gods problem
- [7.pl](https://github.com/aduriseti/3gods/blob/main/7.pl)
  - Adds an early stopping condition to tree generation - where - if # of families we have to partition at any node in tree path is $> 2 ^ {\text{questions left}}$ - we early stop
  - Still hangs
- [8.pl](https://github.com/aduriseti/3gods/blob/main/8.pl)
  - Added a bunch of testing to debug 7
  - Eventually realized that fully evaluating a question of the required complexity for our grammar is too expensive by itself - some back of envelope math gives 1M possible questions of complexity 2 (we know golden question has complexity 2)
- [9.pl](https://github.com/aduriseti/3gods/blob/main/9.pl)
  - A heuristic approach to constraining grammar expansion
  - Tries a new approach were we only compose questions (make them more complex) - if it "improves" the way they partition our families
  - It prunes too aggressively - trivially true question "1==1" is the "ideal" question from a partitioning perspective - but we know it doesn't form part of the ultimate golden question
- [10.pl](https://github.com/aduriseti/3gods/blob/main/10.pl)/[11.pl](https://github.com/aduriseti/3gods/blob/main/11.pl)/[12.pl](https://github.com/aduriseti/3gods/blob/main/12.pl)
  - Explores idea of co-generating the problem alongside our questions
  - Focuses on the step of decomposing the full complexity problem into 2 subproblems - 1 where the Random god isn't allowed at position 2 and 1 where the Random god isn't allowed at position 3
  - These 2 subproblems can be solved in 2 questions each
  - The golden question (I had in mind when I solved this puzzle) "Is the Random god directly adjacent and to the right of the Truly god" - when posed to position 1 - differentiates these 2 subproblems
  - Wasn't able to get this to work - But I think it should be possible - just errors in code (we can exhaustively search the complexity 2 grammar in a few minutes)
- [8a.pl](https://github.com/aduriseti/3gods/blob/main/8a.pl)
  - The breakthrough step making grammar expansion efficient
  - Deduplicates questions by how they partition god permutations
- [8b.pl](https://github.com/aduriseti/3gods/blob/main/8b.pl)
  - Support for ja/da language - also optimizes question deduplication
  - Fully solves the riddle

</details>

<script>
  window.MathJax = {
    tex: {
      inlineMath: [['$', '$'], ['\\(', '\\)']],
      displayMath: [['$$', '$$'], ['\\[', '\\]']]
    }
  };
</script>
<script type="text/javascript" id="MathJax-script" async
  src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js">
</script>

<script type="module">
  import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';
  mermaid.initialize({ startOnLoad: true });
</script>