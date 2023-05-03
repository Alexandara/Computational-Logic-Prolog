/*
 * Name:        Alexis Tudor
 * Class:       CS6374
 * Assignment:  Homework 5
 * Details:     Contains Prolog source code and unit test cases to test the code.
 *              SWISH link:
 * Note:        
 * */

:- use_module(library(scasp)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% Color Map %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
color_map([],_).
color_map([Region|Regions], Colors) :- 
    color_region(Region, Colors), color_map(Regions, Colors).
color_region(r(_,Color,Neighbors), Colors) :-
    select(Color, Colors, Colors1), 
    members(Neighbors, Colors1).
members([], _).
members([C|R], Colors) :- member(C, Colors), members(R, Colors).

map(_, [],[]).
map(F, [H1|T1],[H2|T2]) :- call(F,(H1,H2)), map(F,T1,T2).

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

:- begin_tests(home_work_3).

test(color_map,nondet) :-
  color_map([r(a,A,[B,C,D]), 
  			r(b,B,[A,C,E]), 
  			r(c,C,[A,B,D,E,F]),
  			r(d,D,[A,C,F]), 
  			r(e,E,[B,C,F]), 
  			r(f,F,[C,D,E])],
  		[y,r,b,t]),
  	A is y, 
  	B is r,
  	C is b,
  	D is r,
  	E is y,
  	F is t.

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

:- end_tests(home_work_3).




























