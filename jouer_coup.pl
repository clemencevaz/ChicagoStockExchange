%SECURITE DEPLACEMENT (1,2 ou 3)
test(X):- integer(X),!, X>0,X<4,!.
test(_) :- write('entre 1 et 3!!').
lire(X) :- nl,
	write('De combien voulez vous avancer ? (1,2,3)'), nl,
	read(X),nl,test(X),
	X=1,  nl.
boucle_lire(X):-
	repeat,lire(X), !.

%NB_PILES <= 2
coup_possible(P, Coup):-
length(MARCHE,Res), Res>2,!,
boucle_lire(Coup),
newPosTrader(Coup,P, New_pos),
write(new_pos).

%RENVOIE LA NOUVELLE POSITION TRADER
newPosTrader(C,P, New_pos):-
getMarche(P, M),

%L=nombre de piles restantes
length(M,L),

getTrader(P, T),
Res is (C+T) mod L.