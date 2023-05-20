:-dynamic word/2.
build_kb:-write('Please enter a word and its category on separate lines:'),nl,read(X),
(
X=done;
read(Y),
assert((word(X,Y))),
build_kb
).
is_category(C):-word(_,C).
categories(H):-setof(L,find(L),H).
find(H):-word(_,H).
correct_positions([],[],[]).
correct_positions([H|T],[H1|T1],[H|T2]):-H==H1,correct_positions(T,T1,T2).
correct_positions([H|T],[H1|T1],T2):-H\==H1,correct_positions(T,T1,T2).
correct_letters([X|Y],M,[X|Z]) :- member(X,M), correct_letters(Y,M,Z).
correct_letters([X|Y],M,Z) :- \+ member(X,M), correct_letters(Y,M,Z).
correct_letters([],M,[]).
available_length(L):-word(X,_),atom_chars(X, L1),length(L1,L).
checkcat(L,C):-word(X,C),atom_chars(X, L1),length(L1,L).
pick_word(X,L,C):-word(X,C),atom_chars(X, L1),length(L1,L).
findlength(Z,L):-atom_chars(Z, L1),length(L1,L).
choosecategory(X):-write('Choose a category:'),nl,(read(X),is_category(X);write('This category does not exist.'),nl,choosecategory(X)).
tolist(X,L1):-atom_chars(X, L1).
equal([],[]).
equal([H|T],[H1|T1]):-H==H1,equal(T,T1).
chooselength(Y):-write('Choose a length:'),nl,(read(Y),available_length(Y);write('There are no words of this length.'),nl,chooselength(Y)).
won(Z,S):-tolist(S,S1),tolist(Z,Z1),equal(Z1,S1),write('You Won!').
incorrect(S,Y,X,Z,Y1):-findlength(S,L1),L1\==Y,write('Word is not composed of 5 letters. Try again.'),nl,write('Remaining Guesses are '),write(Y1),nl,game(Y,X,Z,Y1).
check(Y1):-Y1==1,write('You lost!').
correct(S,Y,X,Z,Y1):-(word(S,_),tolist(S,S1),tolist(Z,Z1),write('Correct letters are: '),correct_letters(S1,Z1,A1),write(A1),nl,write('Correct letters in correct positions are:'),correct_positions(Z1,S1,B1),write(B1),nl,write('Remaining Guesses are '),Y2 is Y1-1,write(Y2),nl,game(Y,X,Z,Y2));write('Enter a valid word'),nl,write('Remaining Guesses are '),write(Y1),nl,game(Y,X,Z,Y1).
game(Y,X,Z,Y1):-write('Enter a word composed of '),write(Y),write(' letters:'),nl,read(S),
(
won(Z,S);
check(Y1);
incorrect(S,Y,X,Z,Y1);
correct(S,Y,X,Z,Y1)
).
play:-write('The available categories are: '), categories(H),write(H),nl,choosecategory(X),chooselength(Y),write('Game started. You have '),Y1 is Y+1,write(Y1),write(' guesses.'),nl,pick_word(Z,Y,X),game(Y,X,Z,Y1).
main:-build_kb,play.




