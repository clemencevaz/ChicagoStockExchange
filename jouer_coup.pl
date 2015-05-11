
test(X):- integer(X),!, X>0,X<4, ! , write('ok'),!.

test(_) :- write('entre 1 et 3!!').

lire(X) :- nl,
	write('De combien voulez vous avancer ? (1,2,3)'), nl,
	read(X),nl,test(X),
	X=1,  nl.

boucle_lire(X):-
	repeat,lire(X), !.

%NB_PILES <= 2
coup_possible([MARCHE|Q], Coup):-
length(MARCHE,Res), Res> 2,!,
boucle_lire(Coup), write(Coup).

nouvellePositionTrader(Deplacement,Plateau, New_pos):-
getMarche(Plateau, M),
length(M,L),
getTrader(Plateau, T),
Res is (Deplacement+T) mod L.