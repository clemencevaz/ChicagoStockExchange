
/*____________________________MENU DE DEPART___________________________ */
boucle_menu_depart:- repeat( menu_depart), !.
menu_depart:-nl, write('1. Partie à 2 joueurs'),nl,
write('2. Partie contre un bot'),nl,
write('3. Partie bot contre bot'),nl,
write('4. Quitter'),nl,
write('Entrer un choix : '),
read(Choix),integer(Choix),Choix>0,Choix=<4, appel(Choix),nl.

appel(1):- plateau_depart(P), affiche_plateau(P), jouer_coup(P, 1),!.
appel(2):- plateau_depart(P), write(P), !.
appel(4):-write('Au revoir!'), abort.
appel(_):-write('Vous avez mal choisi').

/*_______________________________INTERFACE _____________________________*/

%SECURITE DEPLACEMENT (1,2 ou 3)
test(X) :- integer(X),X>0,X<4,!.
test(_) :- write('entre 1 et 3!!').
lire(X) :- nl,
write('De combien voulez vous avancer ? (1,2,3)'), nl,
read(X), test(X),nl.

boucle_lire(X):-
repeat,lire(X), !.

/*_____________________________ JOUER COUP _____________________________*/
jouer_coup([Marche,Bourse,Trader,ResJ1,ResJ2],JoueurenCours):-
	length(Marche,Res), Res>2,!,
	boucle_lire(Deplacement),
	getPosTrader(Deplacement,[Marche,Bourse,Trader,ResJ1,ResJ2],NewPos),
	newMarche([Marche,Bourse,NewPos,ResJ1,ResJ2],NewPlateau, JoueurenCours),
	write(ResJ1),nl,
	write(ResJ2),nl,
	write(NewPlateau),
	change(JoueurenCours,NewJoueur),
	jouer_coup(NewPlateau,NewJoueur).

%Changement Joueur
change(1,2).
change(2,1).

% addReserve(Jou, R, X, Res) ajoute � la Reserve R du joueur en cours
addReserve(1, J1, X, [X|J1]).
addReserve(2, J2, X, [X|J2]).

%RENVOIE LA NOUVELLE POSITION TRADER
getPosTrader(D,[M,_,T,_,_],NewT):-
	length(M,Len),
	NewT is (D+T) mod Len.

%RENVOIE NOUVEAU MARCHE,MET A JOUR RESERVE
newMarche([M,B,T,J1,J2], [NewM,B,T,NewJ1,J2], 1):-
	length(M,Len),
	VoisinG	is (T-1) mod Len,
	VoisinD is(T+1) mod Len,
	pop(VoisinG, VoisinD, M, NewM,M1,M2),
	write('Lequel voulez vous garder ? (1 ou 2)'),
	read(Choix),
	newReserve(Choix,M1,M2,J1,NewJ1).

newMarche([M,B,T,J1,J2], [NewM,B,T,J1,NewJ2], 2):-
	length(M,Len),
	VoisinG	is (T-1) mod Len,
	VoisinD is(T+1) mod Len,
	pop(VoisinG, VoisinD, M, NewM,M1,M2),
	write('Lequel voulez vous garder ? (1 ou 2)'),
	read(Choix),
	newReservebis(Choix,M1,M2,J2,NewJ2,Vendue).

%Ajout à la réserve du Joueur2
newReserve(1,M1,_,J1,[M1|J1]).
newReserve(2,_,M2,J1,[M2|J1]).

%Ajout à la reserve du Joueur1
newReservebis(1,M1,M2,J2,[M1|J2],M2).
newReservebis(2,_,M2,J2,[M2|J2],M2).



%Récupère ou modifie la valeur d'une marchandise dans la bourse
valeurBourse(M, [[M|[Q]]|Q2], Q, VN):- Q is VN.
valeurBourse(M, [[M|[Q]]|Q2], Q):-!.
valeurBourse(M, [_|Q], V):- valeurBourse(M, Q, V).
valeurBourse(M, [_|Q], V, VN):- valeurBourse(M, Q, V, VN).

/*____________________ AFFICHAGE PLATEAU DE JEU _______________________*/
affiche_pile([], _,_).
affiche_pile([P|L], Trader, Trader) :-
	write(Trader),
	write(' '),
	write(P),
	writeln(' <= Trader'),
	Tmp is Trader+1,
	affiche_pile(L, Trader, Tmp).

affiche_pile([P|L], Trader, Ct) :-
	write(Ct),
	write(' '),
	writeln(P),
	Tmp is Ct+1,
	affiche_pile(L, Trader, Tmp).

affiche_plateau([Marche,Bourse,Trader,ResJ1,ResJ2]) :-
	affiche_pile(Marche, Trader, 1).



/*_______________________FONCTIONS DE SERVICE__________________________*/

%supprime les listes vides du march�
flatten([],[]).
flatten([T|Q],Res):-flatten(T,TF),!,flatten(Q,QF),concat(TF,QF,Res).
flatten([T|Q],[T|Res]):-flatten(Q,Res).
concat([], L,L).
concat([T|Q], L, [T|Res]):-concat(Q,L,Res).


%retire la tete de la pile M de rang N1 et N2 et l'affiche
pop(N1,N2,M,NewM,T1,T2):-
	nth1(N1, M, [T1|Q1]),
	write('1)'),
	write(T1),
	nl,
	nth1(N2, M, [T2|Q2]),
	write('2)'),
	write(T2),
	nl,
	replace(M, N1, Q1, Tmp),
	replace(Tmp, N2, Q2, Tmp2),
	delete(Tmp2, [], NewM),!.


%replace(L,I,X,Res) : remplace l'�lement de rang I de la liste L par X
replace([_|Q], 1, X, [X|Q]).
replace([T|Q], I, X, [T|R]):- I > 0, NI is I-1, replace(Q, NI, X, R), !.
replace(L, _, _, L).



/*_____________INITIALISATION PLATEAU _____________*/
%Choisit un élément aléatoire dans une liste
nth1Rand([X|R], Y):-
	length([X|R], Long),
	random(1, Long, Tmp),
	nth1(Tmp,[X|R], Y).

%Choisit un élément aléatoire dans une liste puis le supprimer de la liste
nth1RandDelete(Y, [X|R], Z):-
	length([X|R], Long),
	%Random entre deux entiers compris
	random_between(1, Long, Tmp),
	%Récupère l'élément à l'index Tmp, et récupère l'élement dans Y
	nth1(Tmp, [X|R], Y),
	%Supprime l'élément à l'index Tmp, et mets la liste finale dans Z
	nth1(Tmp, [X|R], _, Z).

%Initialisation de la bourse
initBourse([[ble,7],[riz,6],[cacao,6],[cafe,6],[sucre,6],[mais,6]]).

%X = position du trader initiale
initTrader(X) :-
	random(1,9, X).

%Chaque marchandise est là 6 fois
initLimites([ble, ble, ble, ble, ble, ble, riz, riz, riz, riz, riz, riz, cacao, cacao, cacao, cacao, cacao, cacao,
			cafe, cafe, cafe, cafe, cafe, cafe, sucre, sucre, sucre, sucre, sucre, sucre, mais, mais, mais, mais, mais, mais]).

%M = marchandise, [Y|R] = Liste marchandise modif, Z = liste marchandise après modif
initMarchandise(M, [Y|R], Z) :-
	nth1RandDelete(M, [Y|R], Z).

%Initialisation de la Pile, Final est la liste modifiée (sans les marchandises ajoutées)
%Une Pile est liste de 4 marchandises
initPile([A, B, C, D], X, Final):-
	initMarchandise(A, X, Z),
	initMarchandise(B, Z, Y),
	initMarchandise(C, Y, M),
	initMarchandise(D, M, Final).

%Initilisation du marché (Une liste qui représente 9 piles))
initMarche([A, B, C, D, E, F, G, H, I]) :-
	initLimites(X),
	initPile(A, X, J),
	initPile(B, J, K),
	initPile(C, K, L),
	initPile(D, L, M),
	initPile(E, M, N),
	initPile(F, N, O),
	initPile(G, O, P),
	initPile(H, P, Q),
	initPile(I, Q, _).

%Initialisation du plateau de type [Marche, Bourse, Trader, [Reserve J1], [Reserve J2]])
plateau_depart([Marche, Bourse, Trader,[],[]]) :-
initMarche(Marche),
initBourse(Bourse),
initTrader(Trader).
