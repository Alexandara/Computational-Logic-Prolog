/*
 * Name: 		Alexis Tudor
 * Class: 		CS6374
 * Assignment: Homework 1
 * Details:		Contains Prolog source code and unit test cases to test the code.
 * 				SWISH link: https://swish.swi-prolog.org/p/oyYLgtWD.pl
 * Note:		   <Additional Notes, e.g. filename for non-code questions.>
 * 
 * */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q1      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For gender use - male(NAME) or female(NAME).
% For parents use - father(FATHER_NAME, CHILD_NAME) or mother(MOTHER_NAME, CHILD_NAME)
% For married partners use - married(WIFE_NAME, HUSBAND_NAME)

% START Family Facts Enumeration
mother(lisa, abe).
mother(lisa, sarah).
mother(sarah, susan).
mother(susan, jack).
mother(susan, phil).
mother(nancy, john).
mother(mary, jill).

father(tony, abe).
father(tony, sarah).
father(abe, john).
father(john, jill).
father(bill, susan).
father(rob, phil).
father(rob, jack).
father(jack, jim).

% For HWQ 2
female(lisa).
female(sarah).
female(susan).
female(ann).
female(kim).
female(martha).
female(nancy).
female(mary).
female(jill).

male(tony).
male(abe).
male(john).
male(rick).
male(bill).
male(rob).
male(jack).
male(phil).
male(jim).

married(jill, rick).
married(kim, jack).
married(ann, phil).
married(martha, jim).
married(WIFE_NAME, HUSBAND_NAME) :- father(HUSBAND_NAME, Child), mother(WIFE_NAME, Child).
% END of Family Facts


parent(Parent, Child) :- mother(Parent, Child).
parent(Parent, Child) :- father(Parent, Child).

grandparent(Grandparent, Child) :- parent(Grandparent, Parent), parent(Parent, Child).
sibling(X,Y) :- parent(Parent, X), parent(Parent, Y), X\=Y.
auntuncle(AU,N) :- sibling(AU, Parent), parent(Parent, N). %Aunt or uncle
ancestor(Ancestor, Person) :- parent(Ancestor, Person).
ancestor(Ancestor, Person) :- parent(Ancestor, Z), ancestor(Z, Person).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q2      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q2(a)
fcousin(X,Y) :- parent(Parent, X), parent(Grandparent, Parent), parent(Parent2, Y), parent(Grandparent, Parent2), X\=Y, Parent\=Parent2. 

% Q2(b)
scousin(X,Y) :- fcousin(Parent1, Parent2), parent(Parent1, X), parent(Parent2, Y), X\=Y. 

% Q2(c)
grnephew(GreatNephew, GreatAU) :- auntuncle(GreatAU, Parent), parent(Parent, GreatNephew), male(GreatNephew). % Great nephew is the son of one's nephew or niece

% Q2(d)
niece(Niece, AU) :- auntuncle(AU, Niece), female(Niece).

% Q2(e)
manc(X,Y) :- male(X), ancestor(X,Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q3      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q3:
generation_cousin(X,Y) :- fcousin(X,Y).
generation_cousin(X,Y) :- parent(Parent1, X), parent(Parent2, Y), generation_cousin(Parent1, Parent2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q4      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q4(a)
grnephew_mar(GreatNephew, GreatAU) :- auntuncle(GreatAU, Parent), parent(Parent, GreatNephew), male(GreatNephew).
grnephew_mar(GreatNephew, GreatAU) :- married(GreatAU, BloodAU), auntuncle(BloodAU, Parent), parent(Parent, GreatNephew), male(GreatNephew).
grnephew_mar(GreatNephew, GreatAU) :- married(BloodAU, GreatAU), auntuncle(BloodAU, Parent), parent(Parent, GreatNephew), male(GreatNephew).

% Q4(b)
niece_mar(Niece, AU) :- auntuncle(AU, Niece), female(Niece).
niece_mar(Niece, AU) :- married(AU, BloodAU), auntuncle(BloodAU, Niece), female(Niece).  
niece_mar(Niece, AU) :- married(BloodAU, AU), auntuncle(BloodAU, Niece), female(Niece).  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q5      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Hw1.pdf


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q6      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q6: Peano Arithmetic

% Peano Numbers
num(0).
num(s(X)) :- num(X).

% Helper func, converts decimal repr of peano num and back.
peano_to_decimal_rec(0, 0).
peano_to_decimal_rec(s(X), Y) :- peano_to_decimal_rec(X, N), Y is N + 1.

peano_to_decimal(X, Y) :- once(peano_to_decimal_rec(X, Y)).

% Comparison

% equal
equal(X, X).

% less than (add base cases when needed)
less_than(0, s(N)) :- num(N).
less_than(s(SmallNum), s(BigNum)) :- less_than(SmallNum, BigNum).

% greater than (add base cases when needed)
greater_than(s(N), 0) :- num(N).
greater_than(s(BigNum), s(SmallNum)) :- greater_than(BigNum, SmallNum).

% addition (add base cases when needed)
add(0, Y, Y) :- num(Y).
add(s(Num1), Num2, s(Num3)) :- add(Num1, Num2, Num3).

% multiplication (add base cases when needed)
multiply(0, Y, 0) :- num(Y).
multiply(s(Num1), Num2, Num3) :- add(I, Num2, Num3), multiply(Num2, Num1, I).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q7      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q7: Peano Factorial
% Try to make it reversible. 
% HINT: See the implementation of `peano_to_decimal`.
% There are multiple ways of making the function reversible, the hint is one way.

factorial(0, s(0)).
factorial(s(Number), Answer) :- factorial(Number, P), multiply(s(Number), P, Answer). % This function works both ways



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q8      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q8: Quotient And Remainder

%remainder(X, X, 0) :- num(X).
remainder(Top, Bottom, Top) :- less_than(Top, Bottom).
remainder(Top, Bottom, Remainder) :- add(Qr, Bottom, Top), remainder(Qr, Bottom, Remainder).

%quotient(Top, Bottom, Quotient) :- multiply(Qr, Bottom, Top), add(Quotient, Remainder, Qr), remainder(Top, Bottom, Remainder).
quotient(Top, Bottom, Quo) :- remainder(Top, Bottom, Remainder), add(X, Remainder, Top), multiply(Bottom, Quo, X).
divide(Dividend, Divisor, Quo, Rem) :- remainder(Dividend, Divisor, Rem), quotient(Dividend, Divisor, Quo).
%divide(Dividend, Divisor, _, _) :- less_than(Dividend, Divisor).
%divide(Dividend, Divisor, Quo, Rem) :- add(Qr, Divisor, Dividend), multiply(Qr2, Divisor, Dividend), divide(Qr, Divisor, _, Rem), add(Quo, Rem, Qr2).

%divide(Top, Bottom) :- less_than(Top, Bottom).
%divide(_,_,_,_) :- false.

%divide(Top, Bottom, _, _) :- less_than(Top, Bottom).
%divide(Top, Bottom, Rem, Quo) :- multiply(Bottom, Quo, X), add(X, Rem, Top).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%      Q9      %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q9: Nth Fibonacci Number

fib(0, 0).
fib(s(0), s(0)).
fib(s(s(Number)), FibNum) :- fib(s(Number), P), fib(Number, P2), add(P, P2, FibNum). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% Unit Tests  %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(home_work_1).

    /* Test 1: Q2(a) */
    test(fcousin) :-
        setof(X-Y, (fcousin(X, Y), X @< Y), [john-susan]), print("test1").

    /* Test 2: Q2(b) */
    test(scousin) :-
        setof(X-Y, (scousin(X, Y), X @< Y), [jack-jill, jill-phil]).

    /* Test 3: Q2(c) */
    test(grnephew) :-
        setof(X-Y, (grnephew(X, Y)), Ls),
        msort(Ls, Sorted),
        msort([jack-abe, phil-abe], Sorted). % Strict interpretation of grand nephew, only son of one's nephew or niece by sibling

    /* Test 4: Q2(d) */
    test(niece) :-
        setof(X-Y, (niece(X, Y)), Ls), 
        msort(Ls, Sorted),
        msort([susan-abe], Sorted). % Strict interpretation of niece (only the sibling of a parent)

    /* Test 5: Q2(e) */
    test(manc) :-
        setof(X-jim, (manc(X, jim)), Ancestors),
        msort(Ancestors, Sorted),
        msort([bill-jim, jack-jim, rob-jim, tony-jim], Sorted).
        
    /* Test 6: Q3 */
	test(generation_cousin) :-
	    setof(X-Y, (generation_cousin(X, Y), X @< Y), [jack-jill, jill-phil, john-susan]).

    /* Test 7: Q4(a) */
    test(grnephew_mar) :-
        setof(X-Y, (grnephew_mar(X, Y)), Relatives),
        msort(Relatives, Sorted),
        msort(
            [jack-abe, jack-nancy, phil-abe, phil-nancy],
            Sorted
        ). % Strict interpretation of grand nephew, only son of one's nephew or niece by sibling

    /* Test 8: Q4(b) */
    test(niece_mar) :-
        setof(X-Y, (niece_mar(X, Y)), Relatives),
        msort(Relatives, Sorted),
        msort(
            [susan-nancy, susan-abe],
            Sorted
        ). % Strict interpretation of niece (only the sibling of a parent)


    /* Test 9: Q6 */
    test(equal) :-
      peano_to_decimal(X, 1), equal(s(0), X).

    /* Test 10: Q6 */
    test(less_than) :-
      peano_to_decimal(Ten, 10), 
      peano_to_decimal(Eleven, 11),
      less_than(Ten, Eleven).
      

    /* Test 11: Q6 */
    test(greater_than) :-
        peano_to_decimal(Ten, 10), 
        peano_to_decimal(Eleven, 11),
        greater_than(Eleven, Ten).

    /* Test 12: Q6 */
    test(add) :-
        peano_to_decimal(Ten, 10),
        peano_to_decimal(Eleven, 11),
        peano_to_decimal(TwentyOne, 21),
        add(Ten, Eleven, TwentyOne).

    /* Test 13: Q6 */
    test(multiply) :-
        peano_to_decimal(Ten, 10),
        peano_to_decimal(Eleven, 11),
        peano_to_decimal(OneHundredTen, 110),
        multiply(Ten, Eleven, OneHundredTen).

    /* Test 14: Q7 */
    test(factorial) :-
        peano_to_decimal(SevenTwenty, 720),
        peano_to_decimal(Six, 6),
        factorial(Six, SevenTwenty).

    /* Test 15: Q8 */
    test(divide) :-
        peano_to_decimal(Five, 5),
        peano_to_decimal(SeventyNine, 79),
        peano_to_decimal(Fifteen, 15),
        peano_to_decimal(Four, 4),
        divide(SeventyNine, Five, Fifteen, Four).

    /* Test 16: Q9 */
    test(fib) :-
        peano_to_decimal(Twelve, 12),
        peano_to_decimal(OneFourFour, 144),
        fib(Twelve, OneFourFour).


:- end_tests(home_work_1).