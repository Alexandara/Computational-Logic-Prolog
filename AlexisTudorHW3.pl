/*
 * Name:        Alexis Tudor
 * Class:       CS6374
 * Assignment:  Homework 3
 * Details:     Contains Prolog source code and unit test cases to test the code.
 *              SWISH link: https://swish.swi-prolog.org/p/kIUxzcVS.pl
 * Note:        
 * */

 :- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Example For Custom Operators %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
Question: How do you redefine an built-in operator?
Answer:   You can not! Using the docs you can find how to define 
          your own oeprators though! 
          https://www.swi-prolog.org/pldoc/doc_for?object=op/3

Example:  Defining the `univ` operator `=..`, using the operator `..=`

    Prolog Built-In:
        > Term =.. [one, two]. 
          Term = one(two).

    Our Operator:
       > Term ..= [one, two]. 
         Term = one(two).
*/

% redefine the operator, 700 is the precedence number of =.. so ours
%   should have the same
% xfx denotes the type of the operator
% `x` means the precedence of the argument must strictly be lower than
%   the functor
% `f` denotes the functor, in this case the operator we're defining.

:- op(700, xfx, ..=).

..=(Term, [FunctorNonZeroArity|Args]) :-
    length(Args, N),
    \+(N =:= 0),
    functor(Term, FunctorNonZeroArity, N),
    insert_args(Term, Args), !. % we cut since there should only be one solution.


insert_args(Term, Args) :-
    insert_args(Term, Args, 1).

insert_args(_, [], _).
insert_args(Term, [NextArg|Args], N) :-
    arg(N, Term, NextArg),
    N1 is N + 1,
    insert_args(Term, Args, N1).
    
%% If this is true, then when the file is loaded there should be no error.
%% Try to make it incorrect and the error will be displayed as a warning.
:- Term ..= [invented, kowalski, prolog], Term =.. TermToList, ==(TermToList, [invented, kowalski, prolog]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% Ex. 11.3.1 %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% 11.3 (i)
%%   \== using == and cut-fail
%%   You will have to define the \== op as /==
%%   Consult the example above on how to do it.
%%   Your operator /== will be tested using infix notation!
:- op(700, xfx, /==).
/==(X, Y) :- X == Y,!,fail.
/==(_, _).

%% 11.3 (ii)
%%   nonvar using var and cut-fail
%%   You will have to define the nonvar op as non+var
non_var(X) :- var(X),!,fail.
non_var(_).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Belgian Snake Problem %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

snake(Rings, Width, Height) :- snake(Rings, 0, Width, Height).
snake(_,_,_,[]).
snake(Rings, Flip, Width, [_|T]) :- 
    (Flip = 0, printrow(Rings, State, Width), Flip1 is 1; 
    printrowrev(Rings, State, Width), Flip1 is 0),
    nl,
    snake(State,Flip1,Width,T).

printrow(Rings, State, Width) :- printrow(Rings, [], State, Width).
printrow(_, State ,State,[]).
printrow([R|R1], Rev, State, [_|T]) :-
    print(R), append(Rev, [R], Rev1),
    (R1 = [], printrow(Rev1, [],_, T);
    printrow(R1, Rev1,_, T)), append(R1, Rev1,State).
    
printrowrev(Rings, State, Width) :- printrowrev(Rings, [], [], State, Width).
printrowrev(_, State, A, State,[]) :- reverse(A,A1),printlist(A1).
printrowrev([R|R1], Rev, A, State, [_|T]) :-
    append(A,[R],A1), append(Rev, [R], Rev1), 
    (R1 = [], printrowrev(Rev1, [],A1,_, T);
    printrowrev(R1, Rev1,A1,_, T)), append(R1, Rev1,State).

printlist([]).
printlist([H|T]) :- print(H), printlist(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% N-Queens Problem %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
 * data representation
 * 	Qs = [R1, R2, R3, R4].
 * 	Here R1 through R4 are unique row numbers, and their position
 * 	in the list is the column number, this ensures no two queens
 *  are in the same row or column.
 *
 * Thus, a queen can attack another iff they lie on the same
 * diagonal.
 * */


queens(N, Qs) :-
        length(Qs, N),
        Qs ins 1..N,
        safe(Qs),
        labeling([ff], Qs).

safe([]).
safe([Q|Qs]) :- notattack(Qs, Q, 1), safe(Qs).

notattack([], _, _).
notattack([Q|Qs], Q0, D0) :-
        Q0 #\= Q,
        abs(Q0 - Q) #\= D0,
        D1 #= D0 + 1,
        notattack(Qs, Q0, D1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Cryptarithmetic Puzzles %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rev(L, R) :- trev(L, [], R). % O(n) time
trev([], P, P).
trev([H|T], P, R) :- trev(T, [H|P], R).

solve(R1, R2, R3) :- rev(R1,R1r), rev(R2,R2r), rev(R3, R3r),
    solve(R1r, R2r, R3r,0, [0,1,2,3,4,5,6,7,8,9]).
solve([],[],[X],1,_) :- X is 1.
solve([],[],[_],0,_).
solve([],[X],[X],0,_).
solve([],[X],[X1],1,_) :- X1 is X+1.
solve([X],[],[X],0,_).
solve([X],[],[X1],1,_) :- X1 is X+1.
solve([H1|R1], [H2|R2], [H3|R3], C, D) :- 
    (var(H1) -> select(H1, D, D1); D1 = D),
    (var(H2) -> select(H2, D1, D2); D2 = D1),
    (var(H3) -> H3 is (H1+H2+C) mod 10, C1 is (H1+H2+C)//10, 
    select(H3, D2, D3);
    H3 is (H1+H2+C) mod 10, C1 is (H1+H2+C)//10, D3 = D2),
    solve(R1, R2, R3, C1, D3).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Stable Marriage Problem %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Men and Women are a list of atoms
%% Men = [m1, m2], Women = [w1, w2].

%% Men'sPrefsLists and Women'sPrefsLists are a list of list of atoms
%% MensPrefsLists = [[w1, w2], [w2, w1]], WomensPrefsLists = [[m1, m2], [m2, m1]].

%% Marriages are a list of lists of [man, woman] married pair.
%% Marriages = [[m1, w1], [m2, w2]]

/* Students Define These */
stable_marriages(Men, MensPrefsLists, Women, WomensPrefLists, Marriages) :-
    marriages([Men, MensPrefsLists], [Women, WomensPrefLists], Marriages).

marriages(MenR,WomenR, Marriages) :- 
    marriagecombine(MenR, Men),
    marriagecombine(WomenR,Women),
    genmarriages(Men,Women,Marriages1),
    formatm(Marriages1,Marriages).

formatm(L,M) :- formatm(L,[],M).
formatm([],M,M).
formatm([([Man,_],[Woman,_])|T], A, M) :- 
    append(A,[[Man,Woman]], A1),
    formatm(T,A1,M).

ranking(M,W,N) :- ranking(M,W,0,N).
ranking([],_,N,N).
ranking([_,[]],_,N,N).
ranking([Person1,[Pref1|Prefs]],[Person2,_],A,N) :- 
    A1 is A+1,
    (Pref1 = Person2, ranking([],Person2,A1,N); 
    ranking([Person1,Prefs],[Person2,_],A1,N)).

% P1 prefers P2 over P3 
preference(P1,P2,P3) :-
    ranking(P1,P2,N1),
    ranking(P1,P3,N2),!,
    N1 < N2.

unstablepair((Man1, Woman1), (Man2,Woman2)) :- 
    (preference(Man1,Woman2,Woman1),
    preference(Woman2, Man1, Man2);
    preference(Woman1, Man2, Man1),
    preference(Man2, Woman1, Woman2)).

checkinstability(_,[]) :- fail.
checkinstability(Marriage,[M|Ms]) :-
    (unstablepair(Marriage,M);
    checkinstability(Marriage,Ms)).

unstable([]) :- fail.
unstable([Marriage|Marriages]) :- 
    (checkinstability(Marriage,Marriages);
    unstable(Marriages)).

marriagecombine(L,Result) :- marriagecombine(L,[],Result).
marriagecombine([[],[]], Result,Result).
marriagecombine([[Person|People],[Pref|Prefs]], A, R) :-
    append(A,[[Person, Pref]], A1),
    marriagecombine([People,Prefs], A1, R).

list_pairs(List1, List2, Pair) :-
    findall((X,Y), (member(X, List1), member(Y, List2)), Pairs), 
    member(Pair,Pairs).

membermarriage(Man, [(Man,_)|_]).
membermarriage(Man, [_|T]) :- membermarriage(Man, T).
membermarriage(Woman, [(_,Woman)|_]).
membermarriage(Woman, [_|T]) :- membermarriage(Woman, T).

genmarriages(Men, Women, Marriages) :- genmarriages(Men, Women, [], Marriages).
genmarriages([], _, Marriages, Marriages) :- \+unstable(Marriages).
genmarriages([Man|Men], Women, A, Marriages) :-
    list_pairs([Man],Women,(MM,MW)),
    \+membermarriage(MM,A),
    append(A,[(MM,MW)],A2),
    \+unstable(A2),
    select(MW,Women,Women2),
    genmarriages(Men, Women2, A2, Marriages).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Blocks World Problem %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
block(a).
block(b).
block(c).
block(d).
block(e).

place(p).
place(q).
place(r).

transform(State1, State2, Plan) :- transform(State1, State2, [State1], Plan).
transform(State, State,_,[]).
transform(State1, State2, Visited, [Action|Actions]) :-
    choose_action(Action, State1, State2),
    update(Action, State1, State),
    \+member(State, Visited),
    transform(State, State2, [State|Visited], Actions).

legal_action(to_place(Block,Y,Place),State) :-
    on(Block,Y,State), clear(Block,State),
    place(Place), clear(Place,State).
legal_action(to_block(Block1,Y,Block2), State) :- 
    on(Block1,Y,State), clear(Block1,State), block(Block2),
    Block1 \= Block2, clear(Block2,State).
    
clear(X,State) :- \+member(on(_,X),State).

on(X,Y,State) :- member(on(X,Y), State).

update(to_block(X,Y,Z), State, State1) :- substitute(on(X,Y), on(X,Z), State, State1).
update(to_place(X,Y,Z),State,State1) :- substitute(on(X,Y), on(X,Z), State, State1).

substitute(X, Y, L1, L2) :- substitute(X, Y, L1, [], L2).
substitute(_, _, [], L2, L2r) :- rev(L2,L2r).
substitute(X, Y, [H|T], A, L2) :- (H = X, substitute(X, Y, T, [Y|A], L2); 
                                  substitute(X,Y,T,[H|A],L2)).

choose_action(Action, State1, State2) :- 
    suggest(Action, State2), legal_action(Action, State1).
choose_action(Action, State1, _) :- legal_action(Action, State1).
suggest(to_place(X,_,Z) ,State) :- member(on(X,Z), State), place(Z).
suggest(to_block(X,_,Z) ,State) :- member(on(X,Z) ,State), block(Z).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Missionary-Cannibal Problem %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Shore States
%   left_shore(NumMissionary, NumCannibal).
%   right_shore(NumMissionary, NumCannibal).

% Moves Should Be Given By
%   boat(NumMissionary, NumCannibal).

% Your input will be the initial shore states, and the output will be the sequence
% of moves; any valid sequence of moves should be admissible. And the final states.

/*
Challenge: Print out the states before and after each move
Note: It is not necessary to implement this!
    
    Initial State           Move        Final State
    left_shore(3, 3)      boat(0,2)     left_shore(3, 1)
    right_shore(0, 0)                   right_shore(0, 2)
*/

get_moves(Left1, Right1, Moves, Left2, Right2) :- %print("Initial State"), tab(10),
    %print("Move"), tab(10), print("Final State"), nl,
    get_moves([Left1, Right1], [Left2,Right2], l, [[Left1, Right1]], [], Moves).

get_moves(State, State, _, _, Moves, Moves).
get_moves(State1, State2, Side, PrevStates, PrevMoves, Moves) :- 
    %print(State1), tab(10),
    valid_move(State1, Side, Move),
    update(Move, State1, StateN, Side),
    flip(Side, SideN),
    \+member(StateN,PrevStates),
    %print(Move), print(Side), print(->), print(SideN), tab(10), print(StateN),nl,
    get_moves(StateN, State2, SideN, [StateN|PrevStates], [Move|PrevMoves], Moves). 

valid_move([left_shore(ML,CL),right_shore(_,_)], l, boat(M,C)) :-
    M in 0..2, C in 0..2,
    M #=< ML, C #=< CL, (ML-M #>= CL-C; ML-M #= 0), M+C #> 0, M+C #< 3, %(M+MR #>= C+CR; M #= 0, ML #=0),
    labeling([ff],[M,C]). 
valid_move([left_shore(_,_),right_shore(MR,CR)], r, boat(M,C)) :-
    M in 0..2, C in 0..2,
    M #=< MR, C #=< CR, (MR-M #>= CR-C; MR-M #= 0), M+C #> 0, M+C #< 3, %(M+ML #>= C+CL; M #= 0, ML #=0),
    labeling([ff],[M,C]). 

update(boat(M,C), [left_shore(ML,CL),right_shore(MR,CR)], 
       [left_shore(ML2,CL2),right_shore(MR2,CR2)], l) :-
    MR2 is MR + M, CR2 is CR + C, ML2 is ML - M, CL2 is CL - C.
update(boat(M,C), [left_shore(ML,CL),right_shore(MR,CR)], 
       [left_shore(ML2,CL2),right_shore(MR2,CR2)], r) :-
    MR2 is MR - M, CR2 is CR - C, ML2 is ML + M, CL2 is CL + C.

flip(r,l).
flip(l,r).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Unit Tests  %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Helper Code: Don't Remover or Modify %
dict_items(Dict, Keys, Values) :-
    dict_pairs(Dict, _, KVPairs),
    pairs_keys(KVPairs, Keys),
    pairs_values(KVPairs, Values).

% Helper Code: Don't Remover or Modify %
stable_marriages_test(MensPrefs, WomensPrefs, Marriages) :-
    dict_items(MensPrefs, Men, MensPrefsLists),
    dict_items(WomensPrefs, Women, WomensPrefLists),
    stable_marriages(Men, MensPrefsLists, Women, WomensPrefLists, Marriages).


:- begin_tests(home_work_3).

test(solve, nondet) :-
    solve([S,E,N,D], [M,O,R,E], [M,O,N,E,Y]),
    D is 7, E is 5, M is 1, N is 6, O is 0, R is 8,
    S is 9, Y is 2.

test(/==) :-
    /==(1, 2), \+(/==(1, 1)),
    current_op(_, _, /==),
    term_string(Term1, "1 /== 2"),
    call(Term1),
    term_string(Term2, "\\+(1 /== 1)"),
    call(Term2).
    

test(non_var) :-
    non_var(a), not(non_var(_)).

test(snake, Output='abcda\nbadcb\ncdabc\n') :-
    with_output_to(atom(Output), snake([a,b,c,d],[_,_,_,_,_],[_,_,_])).

test(queens, nondet) :-
    queens(4, [3, 1, 4, 2]),
    queens(10, [7, 4, 2, 9, 5, 10, 8, 6, 3, 1]).

test(stable_marriages, nondet) :-
    
    stable_marriages_test(
        mens_preference{
            david: [judy, paula],
            jeremy: [judy, paula]
        },
        womens_preference{
            paula: [david, jeremy],
            judy: [david, jeremy]
        },
        [[david, judy], [jeremy, paula]]
    ),

    stable_marriages_test(
        mens_preferences{
            avraham: [chana, tamar, zvia, ruth, sarah],
            binyamin: [zvia, chana, ruth, sarah, tamar],
            chaim: [chana, ruth, tamar, sarah, zvia],
            david: [zvia, ruth, chana, sarah, tamar],
            elazar: [tamar, ruth, chana, zvia, sarah]
        },
        womens_preferences{
            zvia: [elazar, avraham, david, binyamin, chaim],
            chana: [david, elazar, binyamin, avraham, chaim],
            ruth: [avraham, david, binyamin, chaim, elazar],
            sarah: [chaim, binyamin, david, avraham, elazar],
            tamar: [david, binyamin, chaim, elazar, avraham]
        },
        [[avraham, ruth], [binyamin, sarah], [chaim, tamar], [david, chana], [elazar, zvia]]
    ).
%Test modified for a looser definition of outnumbering (i.e. the missionaries are not outnumbered if they do not get off the boat on the destination shore)
test(get_moves, nondet) :-
  get_moves(
    left_shore(3, 3), 
    right_shore(0, 0),
    [
        boat(0,2),
        boat(0,1),
        boat(1,1),
        boat(0,1),
        boat(0,2),
        boat(1,0),
        boat(2,0),
        boat(0,1),
        boat(1,0),
        boat(0,1),
        boat(0,2)
    ],
    left_shore(0, 0),
    right_shore(3, 3)
).

test(transform, nondet) :-
  
  transform(
    [on(a, b), on(b, p), on(c, r)], 
    [on(a, b), on(b, c), on(c, r)],
    [
        to_place(a, b, q), 
        to_block(b, p, c), 
        to_block(a, q, b)
    ]
  ).

:- end_tests(home_work_3).
