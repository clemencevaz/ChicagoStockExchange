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

%Initialisation du plateau de type [Marche, Bourse, Trader, [Reserve J1], [Reserve J2]])
initPlateau([Marche, Bourse, Trader, [], []]) :-
	initMarche(Marche),
	initBourse(Bourse),
	initTrader(Trader).
	
plateau_depart(P) :-
	initPlateau(P).
	
boucle_menu:-menu, !.
menu:-nl, repeat, write('1. Afficher plateau'),nl,
		  write('2. Initialiser plateau'),nl,
		  write('3. Commencer partie'),nl,
		  write('4. Quitter'),nl,
		  write('Entrer un choix : '),
		  read(Choix),Choix>0,Choix<4,appel(Choix),
		  Choix=4, !.
appel(1):- afficher_plateau(p), !.
appel(2):- plateau_depart(p),!.
appel(4):-write('Au revoir!'), abort.
appel(_):-write('Vous avez mal choisi').