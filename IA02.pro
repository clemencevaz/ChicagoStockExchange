
/* ______________MENU _____________ */
boucle_menu:-menu, !.
menu:-nl, write('1. Afficher plateau'),nl,
		  write('2. Initialiser plateau'),nl,
		  write('3. Commencer partie'),nl,
		  write('4. Quitter'),nl,
		  write('Entrer un choix : '),
		  read(Choix),nl, appel(Choix),
		  Choix=4,nl.
appel(1):- afficher_plateau(p),!.
appel(2):- plateau_depart(p),!.
appel(4):-write('Au revoir!')!.
appel(_):-write('Vous avez mal choisi').


/* __________COUP POSSIBLE__________ */
boucle_lire:-
	repeat,lire, !.

lire :- nl,
	write('De combien voulez vous avancer ? (1,2,3)'), nl,
	read(X),nl,test(X),
	X=1,  nl.

test(X):- X>0,X<4 , ! , write('ok'),!.

test(_) :- write('entre 1 et 3!!').


/* ___________JOUER COUP ___________ */