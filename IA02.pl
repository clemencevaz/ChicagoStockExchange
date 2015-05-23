/*_________________MAIN_________________*/
main:-
boucle_menu_depart,
coup_possible([Marche, Bourse, Trader,_,_], Coup).

/*_________________MENU DE DEPART_________________ */
boucle_menu_depart:- menu_depart, !.
menu_depart:-nl, write('1. Partie à 2 joueurs'),nl,
write('2. Partie contre un bot'),nl,
write('3. Partie bot contre bot'),nl,
write('4. Quitter'),nl,
write('Entrer un choix : '),
read(Choix),Choix>0,Choix=<4, appel(Choix),nl.

appel(1):- plateau_depart(P), affiche_plateau(P), coup_possible(P, C), write(P), !.
appel(2):- plateau_depart(P), write(P), !.
appel(4):-write('Au revoir!'), abort.
appel(_):-write('Vous avez mal choisi').


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

/*_____________ ACCESSEURS _____________*/

%Récupère le marché, P = Plateau, +M = marché
getMarche(P, M) :-
	nth1(1, P, M).

%Récupère la bourse, P = Plateau, +B = bourse
getBourse(P, B) :-
	nth1(2, P, B).

%Récupère le trader, P = Plateau, +T = trader
getTrader(P, T) :-
	nth1(3, P, T).

%Récupère la réserve du joueur 1, P = plateau, +R = réserve
getReserveJ1(P, R) :-
	nth1(4, P, R).

%Récupère la réserve du joueur 2, P = plateau, +R = réserve
getReserveJ2(P, R) :-
	nth1(5, P, R).


/*_____________ JOUER COUP _____________*/

%SECURITE DEPLACEMENT (1,2 ou 3)

test(X) :- integer(X),X>0,X<4,!.
test(_) :- write('entre 1 et 3!!').
lire(X) :- nl,
write('De combien voulez vous avancer ? (1,2,3)'), nl,
read(X), test(X),nl.

boucle_lire(X):-
repeat,lire(X), !.


%NB_PILES <= 2
coup_possible([Marche,Bourse,Trader,Joueur1,Joueur2],Coup):-
length(Marche,Res), Res>2,!,
boucle_lire(Coup),
newPosTrader(Coup,[Marche,Bourse,Trader,Joueur1,Joueur2],NewP).

%RENVOIE LA NOUVELLE POSITION TRADER
newPosTrader(C,[M,B,T,J1,J2],[M,B,NewT,J1,J2]):-
	length(M,Len),
	NewT is (C+T) mod Len.

%AFFICHE VOISINS
retournevoisins([M,B,T,J1,J2], [NewM,B,T,J1,J2]):-
	length(M,Len),
	VoisinG	is (T-1) mod Len,
	VoisinD is(T+1) mod Len,
	pop(VoisinG,M,NewM),
	pop(VoisinD,M,NewM),
	%flatten(M,newM),
	write('Lequel voulez vous vendre ? (1 ou 2)'),
	read(Choix).

% Affichage du plateau de jeu %
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

affiche_plateau(P) :-
	getMarche(P, M),
	getTrader(P, T),
	affiche_pile(M, T, 1).



/*FONCTIONS DE SERVICE*/

%supprime les listes vides du march�
flatten([],[]).
flatten([T|Q],Res):-flatten(T,TF),!,flatten(Q,QF),concatener(TF,QF,Res).
flatten([T|Q],[T|Res]):-flatten(Q,Res).

%retire la tete de la pile de rang n et l'affiche
pop(n,M,NewM):-
	nth1(n, M, [T|Q]),
	write(T),
	NewM is Q.
