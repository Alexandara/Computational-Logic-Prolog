/*
 * Name:        Alexis Tudor
 * Class:       CS6374
 * Assignment:  Homework 4
 * Details:     Contains Prolog source code and unit test cases to test the code.
 *              SWISH link: https://swish.swi-prolog.org/p/hw3art.pl
 * Note:        
 * */

 :- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% N-Queens Problem %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
 * data representation
 *  Qs = [R1, R2, R3, R4].
 *  Here R1 through R4 are unique row numbers, and their position
 *  in the list is the column number, this ensures no two queens
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
    solve(R1r, R2r, R3r,0,[0,1,2,3,4,5,6,7,8,9]).
solve([],[],[X],1,_) :- X is 1.
solve([],[],[_],0,_).
solve([],[X],[X],0,_).
solve([],[X],[X1],1,_) :- X1 is X+1.
solve([X],[],[X],0,_).
solve([X],[],[X1],1,_) :- X1 is X+1.
solve([H1|R1], [H2|R2], [H3|R3], C,D) :- 
    [H1,H2,H3] ins 0..9,
    H3 #= (H1 + H2 + C) mod 10, C1 #= (H1 + H2 + C) // 10,
    labeling([ff], [H1,H2,H3]),
    solve(R1,R2,R3,C1, D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% Zebra Puzzle %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

zebra(Vars):-
Vars=[English,Spaniard,Ukrainian,Japanese,Norwegian,
Red,Green,Yellow,Blue,Ivory,
Dog,Snails,Fox,Horse,Zebra,
Tea,Coffee,Milk,OrangeJuice,Water,
Kools,Chesterfields,Parliaments,LuckyStrike,OldGold],
Vars ins 1..5,
English#=Red,
Spaniard#=Dog,
Coffee#=Green,
Ukrainian#=Tea,
Green#=Ivory+1,
OldGold#=Snails,
Kools#=Yellow,
Milk#=3,
Norwegian#=1,
Chesterfields#=Fox-1#\/Chesterfields#=Fox+1,
Kools#=Horse+1#\/Kools#=Horse-1,
LuckyStrike#=OrangeJuice,
Japanese#=Parliaments,
Norwegian#=Blue+1#\/NO#=Blue-1,
all_different([English,Spaniard,Ukrainian,Japanese,Norwegian]),
all_different([Red,Green,Yellow,Blue,Ivory]),
all_different([Coffee,Water,Milk,OrangeJuice,Tea]),
all_different([Dog,Fox,Horse,Snails,Zebra]),
all_different([Chesterfields,LuckyStrike,Parliaments,Kools,OldGold]),
label(Vars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% Sudoku Puzzle %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

readin(SudokuLines) :-
    open('input.pl', read, File),
    read_file(File,SudokuLines),
    close(File).

read_file(File,[]) :-
    at_end_of_stream(File).

read_file(File,[H|T]) :-
    \+ at_end_of_stream(File),
    read(File,H),
    read_file(File,T).

sudoku([Row1,Row2,Row3,Row4,Row5,Row6,Row7,Row8,Row9]) :- 
    swiprologsudokuhelper([Row1,Row2,Row3,Row4,Row5,Row6,Row7,Row8,Row9]), 
    labeling([ff], Row1),
    labeling([ff], Row2),
    labeling([ff], Row3),
    labeling([ff], Row4),
    labeling([ff], Row5),
    labeling([ff], Row6),
    labeling([ff], Row7),
    labeling([ff], Row8),
    labeling([ff], Row9).

% Inspired by the Sudoku example on the SWI Prolog documentation website
swiprologsudokuhelper(Rows) :-
        length(Rows, 9), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        blocks(As, Bs, Cs),
        blocks(Ds, Es, Fs),
        blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
        all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
        blocks(Ns1, Ns2, Ns3).

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

formats(L1, L2) :- formats(L1,[],L2).
formats([],L2,L2).
formats([H|T], A, L2) :- (H is 0, append([_],A,A1); append([H],A,A1)), formats(T, A1,L2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Unit Tests  %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(home_work_4).

test(solve, nondet) :-
    solve([S,E,N,D], [M,O,R,E], [M,O,N,E,Y]),
    D is 7, E is 5, M is 1, N is 6, O is 0, R is 8,
    S is 9, Y is 2,!,
    nl, print("SEND MORE MONEY Problem:"), nl,
    print("SEND:"), tab(1), print([S,E,N,D]), nl,
    print("MORE:"), tab(1), print([M,O,R,E]), nl,
    print("MONEY:"), tab(1), print([M,O,N,E,Y]), nl.

test(queens, nondet) :-
    queens(4, [3, 1, 4, 2]), 
    queens(10, [7, 4, 2, 9, 5, 10, 8, 6, 3, 1]),!,
    nl,print("Queens Test Answers"),nl,
    print("For 4 queens:"), tab(1), print([3, 1, 4, 2]), nl,
    print("For 10 queens:"), tab(1), print([7, 4, 2, 9, 5, 10, 8, 6, 3, 1]),nl.

test(zebra, nondet) :-
    zebra([English,Spaniard,Ukrainian,Japanese,Norwegian,
_,_,_,_,_,
_,_,_,_,Zebra,
_,_,_,_,Water,
_,_,_,_,_]), 
    Water is Norwegian, Zebra is Japanese,!,
    nl,
    print("Zebra Puzzle Test Answers"), nl,
    print("Englishman"), tab(1), print(English), nl,
    print("Spaniard"), tab(1),  print(Spaniard), nl,
    print("Ukrainian"), tab(1), print(Ukrainian), nl,
    print("Japanese"), tab(1),  print(Japanese),nl,
    print("Norwegian"), tab(1),  print(Norwegian),nl,
    print("Who drinks water?"), tab(1),  print(Water), nl,
    print("Who owns the Zebra?"), tab(1), print(Zebra), nl. 


test(sudoku, nondet) :-
    readin([f(1,1,S11),
        f(1,2,S12),
        f(1,3,S13),
        f(1,4,S14),
        f(1,5,S15),
        f(1,6,S16),
        f(1,7,S17),
        f(1,8,S18),
        f(1,9,S19),
        f(2,1,S21),
        f(2,2,S22),
        f(2,3,S23),
        f(2,4,S24),
        f(2,5,S25),
        f(2,6,S26),
        f(2,7,S27),
        f(2,8,S28),
        f(2,9,S29),
        f(3,1,S31),
        f(3,2,S32),
        f(3,3,S33),
        f(3,4,S34),
        f(3,5,S35),
        f(3,6,S36),
        f(3,7,S37),
        f(3,8,S38),
        f(3,9,S39),
        f(4,1,S41),
        f(4,2,S42),
        f(4,3,S43),
        f(4,4,S44),
        f(4,5,S45),
        f(4,6,S46),
        f(4,7,S47),
        f(4,8,S48),
        f(4,9,S49),
        f(5,1,S51),
        f(5,2,S52),
        f(5,3,S53),
        f(5,4,S54),
        f(5,5,S55),
        f(5,6,S56),
        f(5,7,S57),
        f(5,8,S58),
        f(5,9,S59),
        f(6,1,S61),
        f(6,2,S62),
        f(6,3,S63),
        f(6,4,S64),
        f(6,5,S65),
        f(6,6,S66),
        f(6,7,S67),
        f(6,8,S68),
        f(6,9,S69),
        f(7,1,S71),
        f(7,2,S72),
        f(7,3,S73),
        f(7,4,S74),
        f(7,5,S75),
        f(7,6,S76),
        f(7,7,S77),
        f(7,8,S78),
        f(7,9,S79),
        f(8,1,S81),
        f(8,2,S82),
        f(8,3,S83),
        f(8,4,S84),
        f(8,5,S85),
        f(8,6,S86),
        f(8,7,S87),
        f(8,8,S88),
        f(8,9,S89),
        f(9,1,S91),
        f(9,2,S92),
        f(9,3,S93),
        f(9,4,S94),
        f(9,5,S95),
        f(9,6,S96),
        f(9,7,S97),
        f(9,8,S98),
        f(9,9,S99)]),
    formats([S11,S12,S13,S14,S15,S16,S17,S18,S19],Row1),
    formats([S21,S22,S23,S24,S25,S26,S27,S28,S29],Row2),
    formats([S31,S32,S33,S34,S35,S36,S37,S38,S39],Row3),
    formats([S41,S42,S43,S44,S45,S46,S47,S48,S49],Row4),
    formats([S51,S52,S53,S54,S55,S56,S57,S58,S59],Row5),
    formats([S61,S62,S63,S64,S65,S66,S67,S68,S69],Row6),
    formats([S71,S72,S73,S74,S75,S76,S77,S78,S79],Row7),
    formats([S81,S82,S83,S84,S85,S86,S87,S88,S89],Row8),
    formats([S91,S92,S93,S94,S95,S96,S97,S98,S99],Row9),
    nl, print("Sudoku Problem Answers"), nl,
    print("Before Solving:"), nl,
    print([S11,S12,S13,S14,S15,S16,S17,S18,S19]), nl,
    print([S21,S22,S23,S24,S25,S26,S27,S28,S29]), nl,
    print([S31,S32,S33,S34,S35,S36,S37,S38,S39]), nl,
    print([S41,S42,S43,S44,S45,S46,S47,S48,S49]), nl,
    print([S51,S52,S53,S54,S55,S56,S57,S58,S59]), nl,
    print([S61,S62,S63,S64,S65,S66,S67,S68,S69]), nl,
    print([S71,S72,S73,S74,S75,S76,S77,S78,S79]), nl,
    print([S81,S82,S83,S84,S85,S86,S87,S88,S89]), nl,
    print([S91,S92,S93,S94,S95,S96,S97,S98,S99]), nl,
    sudoku([Row1,Row2,Row3,Row4,Row5,Row6,Row7,Row8,Row9]), !,
    nl, print("After Solving:"), nl,
    print(Row1), nl,
    print(Row2), nl,
    print(Row3), nl,
    print(Row4), nl,
    print(Row5), nl,
    print(Row6), nl,
    print(Row7), nl,
    print(Row8), nl,
    print(Row9), nl.

:- end_tests(home_work_4).




































