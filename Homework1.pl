% s(CASP) Programming
:- use_module(library(scasp)).

% Your program goes here
% Homework Question (HWQ) 1
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

% For HWQ 4
married(rick, jill).
married(jack, kim).
married(phil, ann).
married(jim, martha).
married(Husband, Wife) :- father(Husband, Child), mother(Wife, Child).

% Rules
parent(Parent, Child) :- mother(Parent, Child).
parent(Parent, Child) :- father(Parent, Child).

sibling(X,Y) :- parent(Parent, X), parent(Parent, Y), X\=Y.
auntuncle(AU,N) :- sibling(AU, Parent), parent(Parent, N). %Aunt or uncle


grandparent(Grandparent, Child) :- parent(Grandparent, Parent), parent(Parent, Child).
ancestor(Ancestor, Person) :- parent(Ancestor, Person).
ancestor(Ancestor, Person) :- parent(Ancestor, Z), ancestor(Z, Person).

% HWQ 2
fcousin(X,Y) :- parent(Parent, X), parent(Grandparent, Parent), parent(Parent2, Y), parent(Grandparent, Parent2), X\=Y, Parent\=Parent2. 
scousin(X,Y) :- fcousin(Parent1, Parent2), parent(Parent1, X), parent(Parent2, Y), X\=Y.     
% grnephew(GreatNephew, GreatAU) :- auntuncle(GreatAU, Parent), parent(Parent, GreatNephew), male(GreatNephew). % Great nephew is the son of one's nephew or niece
% niece(Niece, AU) :- auntuncle(AU, Niece), female(Niece).
manc(X,Y) :- ancestor(X,Y), male(X).

% HWQ 3
gencousin(X,Y) :- fcousin(X,Y).
gencousin(X,Y) :- parent(Parent1, X), parent(Parent2, Y), gencousin(Parent1, Parent2).

% HWQ 4 
grnephew(GreatNephew, GreatAU) :- auntuncle(GreatAU, Parent), parent(Parent, GreatNephew), male(GreatNephew).
grnephew(GreatNephew, GreatAU) :- married(GreatAU, BloodAU), auntuncle(BloodAU, Parent), parent(Parent, GreatNephew), male(GreatNephew).
grnephew(GreatNephew, GreatAU) :- married(BloodAU, GreatAU), auntuncle(BloodAU, Parent), parent(Parent, GreatNephew), male(GreatNephew).
niece(Niece, AU) :- auntuncle(AU, Niece), female(Niece).
niece(Niece, AU) :- married(AU, BloodAU), auntuncle(BloodAU, Niece), female(Niece).  
niece(Niece, AU) :- married(BloodAU, AU), auntuncle(BloodAU, Niece), female(Niece).  

% HWQ 6
num(0).
num(s(X)) :- num(X).
           
plus(0, Y, Y).
plus(s(X), Y, s(Z)) :- plus(X, Y, Z).

times(s(0), Y, Y).
times(s(X), Y, Z) :- plus(Y, I, Z), times(Y, X, I).











    