/*
 * Name:        <First Name> <Last Name>
 * Class:       CS6374
 * Assignment:  Homework 2
 * Details:     Contains Prolog source code and unit test cases to test the code.
 *              SWISH link: <Link to your file if there is a SWISH version>
 * Note:        <Additional Notes, e.g. filename for non-code questions.>
 * 
 * */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% PART 1: Ex. 3.2.1 %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% (i) Sublist v/s Subsequence
subsequence([X|Xs], [X|Ys]) :-
    subsequence(Xs, Ys).

subsequence(Xs, [_|Ys]) :-
    subsequence(Xs, Ys).

subsequence([], _).

/*
 * Answer: Subsequence is recursive and covers all cases.
*/




% (ii) Write recursive programs for adjacent and last that
% are semantically equiv. to the ones defined on pg. 62,
% WITHOUT using the append predicate.
adjacent(X,Y,[X,Y|_]).
adjacent(X,Y,[_|T]) :- adjacent(X, Y, T).

last(X, [X|[]]).
last(X, [_|T]) :- last(X, T).



% (iii) double(List, ListList) s.t.
% Every element in List appears twice in ListList
%     i.e. all permutations are admissible
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

member2(X, [X|T]) :- member(X, T).
member2(Num, [_|T]) :- member2(Num, T).

delete2(X, L, R) :- deletel(X,L,R1), deletel(X,R1,R).

double([],_).
double([L1|L1s], L2) :- member2(L1,L2), delete2(L1,L2,Ld), double(L1s, Ld).


% (iv) The size of proof tree as a function of the size 
% of the input list for prog. 3.16a and prog 3.16b.
% The exact size or an asymptotic bound, either is acceptable.

/*
 * Answer: The naive recursion takes O(n2) time due to having to 
 * call both reverse and append up to n times, and the reverse-accumulate 
 * takes O(n) time, iterating through the list once. 
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% PART 2: Ex. 3.3.1 %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% (i) substitute(X, Y, L1, L2) <-
%	  	L2 is a result of substituting all occurences of X in L1 with Y
% Suggestion: name your implementation substitute_helper, and call
%   it within substitute.
rev(L, R) :- trev(L, [], R). % O(n) time
trev([], P, P).
trev([H|T], P, R) :- trev(T, [H|P], R).

substitute(X, Y, L1, L2) :- substitute(X, Y, L1, [], L2).
substitute(_, _, [], L2, L2r) :- rev(L2,L2r).
substitute(X, Y, [H|T], A, L2) :- (H = X, substitute(X, Y, T, [Y|A], L2); 
                                  substitute(X,Y,T,[H|A],L2)).



% (ii) What is the meaning of the given variant of select?
sel(X, [X|Xs], Xs).
sel(X, [Y|Ys], [Y|Zs]) :- X \= Y, sel(X, Ys, Zs).

/*
 * Answer: For select(X, Lx, L), it is true if X is in Lx, whereas L is 
 * Lx without X in it. It works the same as a delete function.
*/



% (iii) no_doubles(L1, L2) <- 
% 	where L2 is a result of removing all duplicates from L1
member(X, [X|_]).
member(Num, [_|T]) :- member(Num, T).

no_doubles(L, L2) :- no_doubles(L, [], L2).
no_doubles([], L, Lr) :- rev(L,Lr).
no_doubles([H|T], A, L2) :- (member(H, A), no_doubles(T, A, L2); 
                            no_doubles(T, [H|A], L2)).



% (v) mergesort(Xs, Ys) <-
%	Ys is an ordered permutation of the list Xs
len([],0).
len(L,S) :- len(L, 0, S).
len([], S, S).
len([_|T], P, S) :- P1 is P+1, len(T, P1, S). 

insert_ordered(X, [], [X]).
insert_ordered(X, [H|T], [X, H | T]) :- X =< H.
insert_ordered(X, [H|T], [H|Lr]) :- X > H, insert_ordered(X, T, Lr).

merge([], L, L).
merge([H|T], L, Ls) :- insert_ordered(H, L, X), merge(T, X, Ls).

half([],[],[]).
half([X],[X],[]).
half([H1, H2| T], [H1|T1], [H2|T2]) :- half(T, T1, T2).

mergesort([],[]).
mergesort([X],[X]).
mergesort(L1, L2) :- half(L1, H1, H2), mergesort(H1, S1), mergesort(H2, S2), 
    merge(S1, S2, L2).



% (vi) kth_largest(Xs, K, V) <-
%	Finds the kth largest element of the list Xs.
fiveelements([],[],[],[],[],[]).
fiveelements([X],[X],[],[],[],[]).
fiveelements([X, Y],[X],[Y],[],[],[]).
fiveelements([X, Y,Z],[X],[Y],[Z],[],[]).
fiveelements([X, Y,Z,A],[X],[Y],[Z],[A],[]).
fiveelements([H1, H2, H3, H4, H5| T], [H1|T1], [H2|T2], [H3|T3], 
             [H4|T4], [H5|T5]) :- fiveelements(T, T1, T2, T3, T4, T5).

combine([],[],[],[],[],[]).
combine([[X]],[X],[],[],[],[]).
combine([[X, Y]],[X],[Y],[],[],[]).
combine([[X, Y,Z]],[X],[Y],[Z],[],[]).
combine([[X, Y,Z,A]],[X],[Y],[Z],[A],[]).
combine([[H1, H2, H3, H4, H5] | T], [H1|T1], [H2|T2], [H3|T3], 
        [H4|T4], [H5|T5])
           :- combine(T, T1, T2, T3, T4, T5).

both(OriginalList, ListofLists) :- fiveelements(OriginalList, L1,L2,L3,L4,L5), 
    combine(ListofLists, L1,L2,L3,L4,L5).

median([X],X).
median([X,_], X).
median([L1,L2,L3], X) :- mergesort([L1,L2,L3], [_,X,_]).
median([L1,L2,L3,L4], X) :- mergesort([L1,L2,L3,L4], [_,X,_,_]).
median([L1,L2,L3,L4,L5], X) :- mergesort([L1,L2,L3,L4,L5], [_,_,X,_,_]).

listofmedians([],[]).
listofmedians([L|T], [M|Lm]) :- median(L,M), listofmedians(T, Lm).

% partition(L, H, S, B) - partition list L on pivot H to 
% produce smaller elements and bigger elements
partition([], _, [], []).
partition([H|T], P, S, [H|B]) :- P =< H, partition(T, P, S, B). 
partition([H|T], P, [H|S], B) :- P > H, partition(T, P, S, B). 

kth_largest([[Y,X]],1,X) :- X >= Y.
kth_largest([[Y,X]],1,Y) :- Y >= X.
kth_largest([[Y,X]],2,X) :- X < Y.
kth_largest([[Y,X]],2,Y) :- Y < X.
kth_largest([[X]],_,X).
kth_largest(Xs, K, E) :- both(Xs, LoL), %Break the list into groups of 5
    listofmedians(LoL, LoM), % find the median of each group
    median(LoM, MoM), %find the median of the medians
    partition(Xs, MoM, S, B), % partition the list on the median of medians
    len(Xs,X1s),
    len(S,S1),
    len(B, B1),!,
    ((B1 = 0, kth_largest([S],K,E);S1=0, kth_largest([B],K,E));
    ((X1s-K) < S1, K1 is K-B1, kth_largest(S, K1, E); kth_largest(B,K,E))).



% (vii) better_poker_hand(Hand1, Hand2, Hand)
%	Where Hand is the better of the two hands.
% Conventions:
%     card(Suit, Value)
%     Suit ∈ {heart, diamond, spade, club}
%     Value ∈ {2,3,4,5,6,7,8,9,10,jack,queen,king,ace}
%     House Rule: no baby straight/5-high straight i.e. A 2 3 4 5
card(heart, 2).
card(heart, 3).
card(heart, 4).
card(heart, 5).
card(heart, 6).
card(heart, 7).
card(heart, 8).
card(heart, 9).
card(heart, 10).
card(heart, jack).
card(heart, queen).
card(heart, king).
card(heart, ace).

card(diamond, 2).
card(diamond, 3).
card(diamond, 4).
card(diamond, 5).
card(diamond, 6).
card(diamond, 7).
card(diamond, 8).
card(diamond, 9).
card(diamond, 10).
card(diamond, jack).
card(diamond, queen).
card(diamond, king).
card(diamond, ace).

card(spade, 2).
card(spade, 3).
card(spade, 4).
card(spade, 5).
card(spade, 6).
card(spade, 7).
card(spade, 8).
card(spade, 9).
card(spade, 10).
card(spade, jack).
card(spade, queen).
card(spade, king).
card(spade, ace).

card(club, 2).
card(club, 3).
card(club, 4).
card(club, 5).
card(club, 6).
card(club, 7).
card(club, 8).
card(club, 9).
card(club, 10).
card(club, jack).
card(club, queen).
card(club, king).
card(club, ace).

lenh([],0).
lenh(L,S) :- len(L, 0, S).
lenh([], S, S).
lenh([_|T], P, S) :- P1 is P+1, len(T, P1, S). 

lessorequalh(Small,Big) :- integer(Small), integer(Big), Small =< Big.
lessorequalh(Small,Big) :- integer(Small), atom(Big).
lessorequalh(jack, jack).
lessorequalh(jack, queen).
lessorequalh(jack, king).
lessorequalh(jack,ace).
lessorequalh(queen, queen).
lessorequalh(queen, king).
lessorequalh(queen, ace).
lessorequalh(king, king).
lessorequalh(king,ace).
lessorequalh(ace, ace).

     
insert_orderedh(X, [], [X]).
insert_orderedh(card(Suit1, Number1), [card(Suit2,Number2)|T], [card(Suit1, Number1), card(Suit2,Number2) | T]) :- 
    lessorequalh(Number1, Number2).
insert_orderedh(card(Suit1, Number1), [card(Suit2,Number2)|T], [card(Suit2,Number2)|Lr]) :- 
    not(lessorequalh(Number1, Number2)), insert_orderedh(card(Suit1, Number1), T, Lr).

% merge(L1,L2, Ls) - Ls is L1 and L2 merged into each other and sorted
mergeh([], L, L).
mergeh([H|T], L, Ls) :- insert_orderedh(H, L, X), mergeh(T, X, Ls).

halfh([],[],[]).
halfh([X],[X],[]).
halfh([H1, H2| T], [H1|T1], [H2|T2]) :- halfh(T, T1, T2).

msorthand([],[]).
msorthand([X],[X]).
msorthand(L1, L2) :- halfh(L1, H1, H2), msorthand(H1, S1), msorthand(H2, S2), 
    mergeh(S1, S2, L2).

% Below functions assume sorted hand
% Royal Flush > Straight Flush > Four of a Kind > Full House > Straight > Flush > Three of a Kind > Two Pairs > One Pair > Nothing
flush([card(X, _), card(X, _), card(X, _), card(X, _), card(X, Y)],Y).
straight([card(_, 2), card(_, 3), card(_, 4), card(_, 5), card(_, 6)],6).
straight([card(_, 3), card(_, 4), card(_, 5), card(_, 6), card(_, 7)],7).
straight([card(_, 4), card(_, 5), card(_, 6), card(_, 7), card(_, 8)],8).
straight([card(_, 5), card(_, 6), card(_, 7), card(_, 8), card(_, 9)],9).
straight([card(_, 6), card(_, 7), card(_, 8), card(_, 9), card(_, 10)],10).
straight([card(_, 7), card(_, 8), card(_, 9), card(_, 10), card(_, jack)], jack).
straight([card(_, 8), card(_, 9), card(_, 10), card(_, jack), card(_, queen)],queen).
straight([card(_, 9), card(_, 10), card(_, jack), card(_, queen), card(_, king)],king).
royalstraight([card(_, 10), card(_, jack), card(_, queen), card(_, king), card(_,ace)],ace).

royalflush(Hand, ace) :- royalstraight(Hand,_), flush(Hand,_).
straightflush(Hand,X) :- straight(Hand, X), flush(Hand,_).
fourofakind([card(_, X), card(_, X), card(_, X), card(_, X), card(_, Y)], X) :- X \= Y.
fourofakind([card(_, Y), card(_, X), card(_, X), card(_, X), card(_, X)], X) :- X \= Y.
fullhouse([card(_, X), card(_, X), card(_, X), card(_, Y), card(_, Y)], Y).
fullhouse([card(_, X), card(_, X), card(_, Y), card(_, Y), card(_, Y)], Y).
threeofakind([card(_, X), card(_, X), card(_, X), card(_, Z), card(_, Y)], X) :- X \= Y, X \= Z.
threeofakind([card(_, Z), card(_, X), card(_, X), card(_, X), card(_, Y)], X) :- X \= Y, X \= Z.
threeofakind([card(_, Z), card(_, Y), card(_, X), card(_, X), card(_, X)], X) :- X \= Y, X \= Z.
twopairs([card(_, X), card(_, X), card(_, Y), card(_, Y), card(_, Z)], Y) :- X \= Y, X \= Z, Y \= Z.
twopairs([card(_, X), card(_, X), card(_, Z), card(_, Y), card(_, Y)], Y) :- X \= Y, X \= Z, Y \= Z.
twopairs([card(_, Z), card(_, X), card(_, X), card(_, Y), card(_, Y)], Y) :- X \= Y, X \= Z, Y \= Z.
onepair([card(_, X), card(_, X), card(_, Q), card(_, Y), card(_, Z)], X) :- X \= Y, X \= Z, X\=Q.
onepair([card(_, Q), card(_, X), card(_, X), card(_, Y), card(_, Z)], X) :- X \= Y, X \= Z, X\=Q.
onepair([card(_, Q), card(_, Y), card(_, X), card(_, X), card(_, Z)], X) :- X \= Y, X \= Z, X\=Q.
onepair([card(_, Q), card(_, Y), card(_, Z), card(_, X), card(_, X)], X) :- X \= Y, X \= Z, X\=Q.
nothing(Hand) :- not(royalflush(Hand,_)), not(straightflush(Hand,_)), not(fourofakind(Hand,_)), not(fullhouse(Hand,_)), 
    not(straight(Hand,_)), not(flush(Hand,_)), not(threeofakind(Hand,_)), not(twopairs(Hand,_)), not(onepair(Hand,_)).

% True if Hand1 is better than Hand 2
% Royal Flush > Straight Flush > Four of a Kind > Full House > Straight 
% > Flush > Three of a Kind > Two Pairs > One Pair > Nothing
betterthan(Hand1, Hand2) :- royalflush(Hand1,X), (royalflush(Hand2,Y), not(lessorequalh(X,Y));straightflush(Hand2,_);fourofakind(Hand2,_);fullhouse(Hand2,_);straight(Hand2,_);
                                               flush(Hand2,_); threeofakind(Hand2,_);twopairs(Hand2,_);onepair(Hand2,_);nothing(Hand2)).
betterthan(Hand1, Hand2) :- straightflush(Hand1,X), (straightflush(Hand2,Y), not(lessorequalh(X,Y));fourofakind(Hand2,_);fullhouse(Hand2,_);straight(Hand2,_);
                                               flush(Hand2,_); threeofakind(Hand2,_);twopairs(Hand2,_);onepair(Hand2,_);nothing(Hand2)).
betterthan(Hand1, Hand2) :- fourofakind(Hand1,X), (fourofakind(Hand2,Y), not(lessorequalh(X,Y));fullhouse(Hand2,_);straight(Hand2,_);
                                               flush(Hand2,_); threeofakind(Hand2,_);twopairs(Hand2,_);onepair(Hand2,_);nothing(Hand2)).
betterthan(Hand1, Hand2) :- fullhouse(Hand1,X), (fullhouse(Hand2,Y), not(lessorequalh(X,Y));straight(Hand2,_);
                                               flush(Hand2,_); threeofakind(Hand2,_);twopairs(Hand2,_);onepair(Hand2,_);nothing(Hand2)).
betterthan(Hand1, Hand2) :- straight(Hand1,X), (straight(Hand1,Y), not(lessorequalh(X,Y));flush(Hand2,_); threeofakind(Hand2,_);twopairs(Hand2,_);onepair(Hand2,_);nothing(Hand2)).
betterthan(Hand1, Hand2) :- flush(Hand1,X), (flush(Hand2,Y), not(lessorequalh(X,Y));threeofakind(Hand2,_);twopairs(Hand2,_);onepair(Hand2,_);nothing(Hand2)).
betterthan(Hand1, Hand2) :- threeofakind(Hand1,X), (threeofakind(Hand2,Y), not(lessorequalh(X,Y));twopairs(Hand2,_);onepair(Hand2,_);nothing(Hand2)).
betterthan(Hand1, Hand2) :- twopairs(Hand1,X), (twopairs(Hand2,Y), not(lessorequalh(X,Y));onepair(Hand2,_);nothing(Hand2)).
betterthan(Hand1, Hand2) :- onepair(Hand1,X), (onepair(Hand2,Y), not(lessorequalh(X,Y));nothing(Hand2)).
betterthan(_,_) :- false. 

better_poker_hand(Hand1, Hand2, Hand) :- msorthand(Hand1, Hand1s),
   msorthand(Hand2, Hand2s), (betterthan(Hand1s,Hand2s), Hand = Hand1;betterthan(Hand2s,Hand1s), Hand = Hand2; Hand=Hand1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Start of Helper Functions %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper func, converts decimal repr of peano num and back.
dec_peano(0, 0).
dec_peano(X, s(Y)) :- dec_peano(N, Y), X is N + 1.

peano_dec(P, D) :- once(dec_peano(D, P)).

rangedec(0, [0]).
rangedec(s(N), [s(N)|L]) :-
    rangedec(N, L).

rangedec(N, N, [H]) :-
    rangedec(N, [H|_]).

rangedec(s(N), Stop, [s(N)|L]) :-
    rangedec(N, Stop, L).

% Numbers
num(0).
num(s(X)) :- num(X).

% Comparison

% less than
lt(0, s(X)) :- num(X).
lt(s(X), s(Y)) :- lt(X, Y).

% greater than
gt(s(X), s(Y)) :- lt(s(Y), s(X)).

% equal
eq(X, X).

% less than or equal
lteq(N1, N2) :-
    lt(N1, N2);
    eq(N1, N2).

% plus
add(0, Y, Y) :- num(Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

% times
times(0, Y, 0) :- num(Y).
times(s(X), Y, Z) :- times(X, Y, Z1), add(Y, Z1, Z).

%%%%%%%%%%%%%%%% Don't Remove Any Helper Functions %%%%%%%%%%%%%%%%%

% add a number to sbt
insert(N, nil, node(N, nil, nil)).
insert(N, node(V, A, B), node(V, AA, B)) :- lteq(N, V), insert(N, A, AA).
insert(N, node(V, A, B), node(V, A, BB)) :- gt(N, V), insert(N, B, BB).


%  applies a predicate which will modify the tree
%  at a node with value V in list of values
tree_map_mutate(_, [], Old, Old).
tree_map_mutate(F, [X|Xs], Old, New) :-
    call(F, X, Old, Mid),
    tree_map_mutate(F, Xs, Mid, New).


% insert a list of numbers
inslist(Xs, Old, New) :-
    tree_map_mutate(insert, Xs, Old, New).


% delete a list of numbers 
% Note: this function will work
%    only if your implementation of
%    delete is correct.
dellist(Xs, Old, New) :-
    tree_map_mutate(delete, Xs, Old, New).


% get max value from tree
get_max(node(V, _, node), V).
get_max(node(_, _, R), M) :- get_max(R, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% End of Helper Functions %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% PART 3: SBT Functions %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (i) sumtree(T, N): N is the sum of elements in SBT T (use succ arithmetic).
tree(nil).
tree(node(V, L, R)) :- integer(V), tree(L), tree(R).

treemem(X, node(X, _, _)).
treemem(X, node(Y, L, _)) :- lteq(X, Y), treemem(X,L).
treemem(X, node(Y, _, R)) :- gt(X, Y), treemem(X,R). 

treeins(X, nil, node(X, nil, nil)).
treeins(X, node(Y, L, R), node(Y, L1, R)) :- lteq(X, Y), treeins(X, L, L1).
treeins(X, node(Y, L, R), node(Y, L, R1)) :- gt(X, Y), treeins(X, R, R1).

preorder(nil, []).
preorder(node(X,L,R) ,Xs) :- preorder(L,Ls), preorder(R,Rs), append([X|Ls],Rs,Xs).

sum([], 0).
sum(L, S) :- sum(L, 0, S).
sum([], S, S).
sum([H|T], P, S) :- add(P,H,P1), sum(T, P1, S).

sumtree(T,N) :- preorder(T, Np), sum(Np, N).


% (ii) delete(E, T, Tn): delete the element E from SBT T to obtain SBT Tn.
deletel(X, [X|T], T).
deletel(X, [H|T], [H|R]) :- deletel(X, T, R). 

treetolistremover(E, T, Ls) :- preorder(T, L), deletel(E, L, Ls).  

listtotree(L, T) :- listtotree(L, nil, T).
listtotree([], T, T).
listtotree([H|T],A,Tree) :- treeins(H,A, A1), listtotree(T,A1,Tree).

delete(E, T, Tn) :- treetolistremover(E,T,Ls), listtotree(Ls, Tn).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% PART 4: Ex. 8.3.1 %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (i) triangular_num(N, T) <-
% 		T is the Nth Triangular Number.
triangular_num(N,T) :- N > 0, N1 is N-1, triangular_num(N1, T1), T is N+T1.
triangular_num(1,1).
triangular_num(0,0).


% (iii) reverse_between(I, J, K)
% 	K is an integer between J and I inclusive, J > I.
reverse_between(I,J,J) :- I =< J.
reverse_between(I,J,K) :- I =< J, J1 is J-1, reverse_between(I, J1, K).


% (vi) min_list(List, Min)
%	Min is the minimum int in the List of integers.
min_list(L, N) :- minimum(L, inf, N).
minimum([],A,A).
minimum([H|T],A,N) :- (A > H, minimum(T,H,N); minimum(T,A,N)).


% (vii) length_iter(List, Len)
%	Len of list List using an iterative function.
length_iter(L,N) :- len2(L, 0, N).
len2([_|Xs],A,N) :- A1 is A+1, len2(Xs,A1,N).
len2([],A,A).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% Unit Tests  %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(home_work_2).

    /* Test 1: Part 1::(ii) a. */
    test(adjacent) :- 
        adjacent(1, 2, [1, 2, 3]),
        not(adjacent(1, 2, [1, 3, 2])).

    /* Test 2: Part 1::(ii) b. */
    test(last) :- 
        last(9, [9, 1, 1, 9]),
        not(last(1, [9, 1, 1, 9])).

    /* Test 3: Part 1::(iii) */
    test(double) :-
        double([1, 2], [1, 1, 2, 2]),
        double([1, 2], [1, 1, 2, 2]),
        double([1, 2], [1, 2, 1, 2]),
        double([1, 2], [1, 2, 2, 1]),
        double([1, 2], [1, 2, 1, 2]),
        double([1, 2], [1, 2, 2, 1]),
        double([1, 2], [1, 1, 2, 2]),
        double([1, 2], [1, 1, 2, 2]),
        double([1, 2], [1, 2, 1, 2]),
        double([1, 2], [1, 2, 2, 1]),
        double([1, 2], [1, 2, 1, 2]),
        double([1, 2], [1, 2, 2, 1]),
        double([1, 2], [2, 1, 1, 2]),
        double([1, 2], [2, 1, 2, 1]),
        double([1, 2], [2, 1, 1, 2]),
        double([1, 2], [2, 1, 2, 1]),
        double([1, 2], [2, 2, 1, 1]),
        double([1, 2], [2, 2, 1, 1]),
        double([1, 2], [2, 1, 1, 2]),
        double([1, 2], [2, 1, 2, 1]),
        double([1, 2], [2, 1, 1, 2]),
        double([1, 2], [2, 1, 2, 1]),
        double([1, 2], [2, 2, 1, 1]),
        double([1, 2], [2, 2, 1, 1]),
        double([1, 2], [1, 2, 3, 4, 5, 6, 1, 2]).

    /* Test 4: Part 2::(i) */
    test(substitute) :- 
        substitute(0, 5, [0, 1, 0, 3, 0, 5, 0], [5, 1, 5, 3, 5, 5, 5]).

    /* Test 5: Part 2::(iii) */
    test(no_doubles) :- 
        no_doubles([1, 3, 2, 2, 3, 2], [1, 3, 2]). % Changed unit test to be unordered non-doubled list instead of ordered

    /* Test 6: Part 2::(v) */
    test(mergesort) :- 
        mergesort([1, 9, 8, 5, 3, 6, 7, 2, 4], [1, 2, 3, 4, 5, 6, 7, 8, 9]),
        mergesort([1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9]),
        mergesort([9, 8, 7, 6, 5, 4, 3, 2, 1], [1, 2, 3, 4, 5, 6, 7, 8, 9]).
    

    /* Test 7: Part 2::(vi)  */
    test(kth_largest) :-
        kth_largest([4, 1, 6, 2, 5, 3], 1, 6),
        kth_largest([5, 4, 6, 3, 1, 2], 2, 5),
        kth_largest([2, 1, 5, 3, 4, 6], 3, 4),
        kth_largest([4, 2, 6, 3, 5, 1], 4, 3),
        kth_largest([5, 3, 4, 2, 1, 6], 5, 2),
        kth_largest([1, 6, 4, 5, 3, 2], 6, 1).
        
    /* Test 8: Part 2::(vii) */
    test(better_poker_hand) :- 
        better_poker_hand(
            [card(diamond, 10), card(club, 2), card(heart, ace), card(spade, ace), card(diamond, king)], 
            [card(heart, 2), card(heart, 3), card(heart, 4), card(heart, 5), card(heart, 6)], 
            [card(heart, 2), card(heart, 3), card(heart, 4), card(heart, 5), card(heart, 6)]
        ),
        better_poker_hand(
            [card(heart, 5), card(spade, queen), card(heart, king), card(diamond, king), card(spade, 7)],
            [card(diamond, 5), card(spade, king), card(diamond, queen), card(club, queen), card(club, 7)],
            [card(heart, 5), card(spade, queen), card(heart, king), card(diamond, king), card(spade, 7)]
        ),
        better_poker_hand(
            [card(heart, 5), card(spade, queen), card(heart, ace), card(diamond, 4), card(spade, 7)],
            [card(diamond, 5), card(spade, king), card(diamond, queen), card(club, jack), card(club, 7)],
            [card(heart, 5), card(spade, queen), card(heart, ace), card(diamond, 4), card(spade, 7)]
        ).

    /* Test 9: Part 3::(i) */
    test(sumtree) :- 
        % sum of all numbers in tree, the numbers being
        %    from 1 to 10.
        once(dec_peano(10, TenPeano)),
        once(rangedec(TenPeano, OneToTenPeano)),
        once(inslist(OneToTenPeano, nil, New)),
        once(sumtree(New, FiftyFivePeano)),
        once(peano_dec(FiftyFivePeano, 55)),
        % sum of numbers from 6 to 10
        %     limited list generation method.
        once(dec_peano(6, SixPeano)),
        once(rangedec(TenPeano, SixPeano, TenToSixPeano)),
        once(inslist(TenToSixPeano, nil, Tree2)),
        once(sumtree(Tree2, FortyPeano)),
        once(peano_dec(FortyPeano, 40)).

    /* Test 10: Part 3::(ii) */
    test(delete) :- 
        % sum of numbers from 6 to 10
        %     delete 1 to 5 from tree.
        once(dec_peano(10, TenPeano)),
        once(rangedec(TenPeano, OneToTenPeano)),
        once(inslist(OneToTenPeano, nil, Tree1)),
        once(dec_peano(5, FivePeano)),
        once(rangedec(FivePeano, OneToFivePeano)),
        once(dellist(OneToFivePeano, Tree1, Tree2)),
        once(sumtree(Tree2, FortyPeano)),
        once(peano_dec(FortyPeano, 40)),
        % sum of numbers from 1 to 9 (delete 10 from tree)
        once(delete(TenPeano, Tree1, Tree3)),
        once(sumtree(Tree3, FortyFivePeano)),
        once(peano_dec(FortyFivePeano, 45)).


    /* Test 11: Part 4::(i) */
    test(triangular_num) :-
        triangular_num(0, 0),
        triangular_num(1, 1),
        triangular_num(2, 3),
        triangular_num(3, 6),
        triangular_num(11, 66),
        triangular_num(8, X),
        triangular_num(X, 666).

    /* Test 12: Part 4::(iii) */
    test(reverse_between) :- 
        findall(X, reverse_between(1, 4, X), Ls),
        msort(Ls, [1, 2, 3, 4]),
        findall(X, reverse_between(10, 24, X), Ls2),
        msort(Ls2, [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]).

    /* Test 13: Part 4::(vi) */
    test(min_list) :- 
        min_list([10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24], 10),
        min_list([6, 3, 4, 10, 5, 8, 9, 7], 3).

    /* Test 14: Part 4::(vii) */
    test(length_iter) :- 
        length_iter([6, 3, 4, 10, 5, 8, 9, 7], 8),
        length_iter([6, 4, 10, 5, 8], 5),
        length_iter([], 0).

    
:- end_tests(home_work_2).