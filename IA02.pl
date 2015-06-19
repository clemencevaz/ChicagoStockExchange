
/*____________________________MENU DE DEPART___________________________ */
liredepart:-nl, write('1. Partie Ã  2 joueurs'),nl,
write('2. Partie contre un bot'),nl,
write('3. Partie bot contre bot'),nl,
write('4. Quitter'),nl,
write('Entrer un choix : '), nl,
read(Choix),appel(Choix),!.
liredepart:-nl,write('Entre 1 et 4 !!'), nl,liredepart.

appel(1):- plateau_depart(P), affiche_plateau(P), jeuHumainHumain(P, 1),nl,!.
appel(2):- plateau_depart(P), affiche_plateau(P), jeuHumainOrdi(P, 1),!.
appel(3):- plateau_depart(P), jeuOrdi(P, 1),!.
appel(4):-write('Au revoir!'), abort.

%jeuOrdi(+Plateau, +Joueur) 
%Ce prédicat est utilisé lorsque deux IA s'affrontent
jeuOrdi([Marche,Bourse,Trader,ResJ1,ResJ2],JoueurenCours) :-
	length(Marche, Res),
	(Res > 2 -> 
		iafinal([Marche,Bourse,Trader,ResJ1,ResJ2],JoueurenCours, NewP), change(JoueurenCours, NewJoueur), 
		jeuOrdi(NewP, NewJoueur); gagnant([Marche,Bourse,Trader,ResJ1,ResJ2], G), nl, write('Le gagnant est le joueur '), write(G)).

%jeuHumainHumain(+Plateau, +Joueur)
%Ce prédicat est utilisé lorsque deux joueurs humains s'affrontent
jeuHumainHumain([Marche,Bourse,Trader,ResJ1,ResJ2],JoueurenCours):-
	length(Marche, Res),
	(Res > 2 -> jouer_coup([Marche,Bourse,Trader,ResJ1,ResJ2], JoueurenCours, NewP),
	 change(JoueurenCours, NewJ), jeuHumainHumain(NewP, NewJ); gagnant([Marche,Bourse,Trader,ResJ1,ResJ2], G), nl, write('Le gagnant est le joueur '), write(G)).

%jeuHumainOrdi(+Plateau, +Joueur)
%Ce prédicat est utilisé lorsque un joueur humain et une IA s'affrontent
jeuHumainOrdi([Marche,Bourse,Trader,ResJ1,ResJ2],JoueurenCours):-
	length(Marche, Res),
	(Res>2 -> 
		(JoueurenCours == 1 -> 
			jouer_coup([Marche,Bourse,Trader,ResJ1,ResJ2], 1, NewP); 
			iafinal([Marche,Bourse,Trader,ResJ1,ResJ2], 2, NewP)), 
			change(JoueurenCours, NewJoueur), 
			jeuHumainOrdi(NewP, NewJoueur);  

			gagnant([Marche,Bourse,Trader,ResJ1,ResJ2], G), nl, write('Le gagnant est le joueur '), write(G)).

/*_______________________________INTERFACE _____________________________*/

%SECURITE DEPLACEMENT (1,2 ou 3)
%liredeplacement(?Deplacement)
%Ce prédicat est utilisé pour afficher le menu du déplacement du trader
liredeplacement(X) :-
	write('De combien voulez vous avancer ? (1,2,3)'), nl,
	read(X),
	liredep(X), !.
liredeplacement(X) :-
	nl,write('ENTRE 1, 2 ET 3 !!!!!'),nl,
	liredeplacement(X).

liredep(1):-!.
liredep(2):-!.
liredep(3):-!.

%SECURITE CHOIX MARCHANDISE (1 ou 2)
%liremarchandise(?Deplacement)
%Ce prédicat est utilisé pour afficher le menu de choix de la marchandise
liremarchandise(X) :-
	write('Lequel voulez vous garder ? (1 ou 2)'),
	nl,read(X),
	liremarch(X),!.
liremarchandise(X) :- nl,write('ENTRE 1 ET 2 !!!!!'),nl,liremarchandise(X).

liremarch(1):-!.
liremarch(2):-!.


/*_____________________________ JOUER COUP _____________________________*/
%jouer_coup(+Plateau, +Joueur, ?NouveauPlateau)
%Ce prédicat s'occupe de la gestion d'un seul coup pour un joueur humain
jouer_coup([Marche,Bourse,Trader,ResJ1,ResJ2],JoueurenCours, [NewM, NewB, TmpPos, NewJ1, NewJ2]):-
	liredeplacement(Deplacement),
	getPosTrader(Deplacement,[Marche,Bourse,Trader,ResJ1,ResJ2],NewPos),
	affiche_pile(Marche, NewPos),
	newMarcheBourseRes([Marche,Bourse,NewPos,ResJ1,ResJ2],[NewM,NewB,TmpPos,NewJ1,NewJ2], JoueurenCours),
	affiche_plateau([NewM, NewB, TmpPos, NewJ1, NewJ2]).

%Changement Joueur
%change(+Joueur, ?AutreJoueur)
%Ce prédicat renvoie la valeur de l'autre joueur
change(1,2).
change(2,1).

%RENVOIE LA NOUVELLE POSITION TRADER
%getPosTrader(+Deplacement, +Plateau, ?NouveauTrader)
%Ce prédicat renvoie la nouvelle position du trader selon son emplacement et la taille du marché
getPosTrader(D,[M,_,T,_,_],NewT):-
	length(M,Len),
	TmpT is D+T,
	modulo(TmpT, Len, NewT).

%RENVOIE NOUVEAU MARCHE,MET A JOUR RESERVE
%newMarcheBourseRes(+Plateau, ?NouveauPlateau, +JoueurEnCours)
%Ce prédicat récupère les voisins du trader, met à jour le marché, permet au joueur de choisir la marchandise, 
%met à jour sa réserve ainsi que la bourse
newMarcheBourseRes([M,B,T,J1,J2], [NewM,NewB,NewT,NewJ1,J2], 1):-
	getvoisins(M,T,VoisinG,VoisinD),
	pop(VoisinG, VoisinD, M, NewM,M1,M2, Vide),
	liremarchandise(Choix),
	(Vide==1 -> NewT is T-1; NewT is T),
	addReserve(Choix,M1,M2,J1,NewJ1,Vendue),
	setValeurMarchandise(Vendue, B, NewB).

%newMarcheBourseRes(+Plateau, ?NouveauPlateau, +JoueurEnCours)
%Identique que le prédicat ci-dessus
newMarcheBourseRes([M,B,T,J1,J2], [NewM,NewB,NewT,J1,NewJ2], 2):-
	getvoisins(M,T,VoisinG,VoisinD),
	pop(VoisinG, VoisinD, M, NewM,M1,M2, Vide),
	liremarchandise(Choix),
	(Vide==1 -> NewT is T-1; NewT is T),
	addReserve(Choix,M1,M2,J2,NewJ2,Vendue),
	setValeurMarchandise(Vendue,B,NewB).

%getvoisins(+Marche, +Trader, ?IndexGauche, ?IndexDroit)
%Ce prédicat renvoie l'index de la pile à gauche du trader ainsi que l'index de la pile à sa droite
getvoisins(M,T,TG,TD):-
	length(M,Len),
	(T==1 -> TmpGauche is Len;
	TmpGauche is T-1),
	modulo(TmpGauche, Len, TG),
	TmpDroite is T+1,
	modulo(TmpDroite, Len, TD).

%Ajout a  la reserve du Joueur
%addReserve(+Choix, +Marchandise1, +Marchandise2, +ReserveJoueurEnCours, ?NouvelleReserveJoueur, ?MarchandiseJetee)
%Ce prédicat ajoute à la réserve du joueur en cours la marchandise associée à son choix (1 ou 2) et renvoie la marchandise jetée
addReserve(1,M1,M2,ResenCours,[M1|ResenCours],M2).
addReserve(2,M1,M2,ResenCours,[M2|ResenCours],M1).

%getValeurMarchandise(+Marchandise, +Bourse, ?ValeurMarchandise)
%Recupere la valeur d'une marchandise M dans la bourse B
getValeurMarchandise(M, [[M|[Q]]|_], Q):-!.
getValeurMarchandise(M, [_|Q], V):- getValeurMarchandise(M, Q, V).

%setValeurMarchandise(+Marchandise, +BourseActuelle, +Valeur, ?NouvelleBourse)
%Modifie la valeur d'une marchandise dans la bourse
setValeurMarchandise(M, [[M|[OldV]]|Q2], [[M|[V]]|Q2]):- V is OldV-1,!.
setValeurMarchandise(M, [T|Q],[T|B]):- setValeurMarchandise(M, Q, B),!.

%modulo(+X, +Y, ?Z)
% Réimplémentation du modulo de SWI-Prolog pour ne pas avoir un modulo = 0
modulo(6,3,1).
module(4,2,1).
modulo(X,Y,Z):- X > Y, Z is X mod Y,!.
modulo(X,_,X).



/*____________________ AFFICHAGE PLATEAU DE JEU _______________________*/
%affiche_bourse(+Bourse)
%Affichage de la bourse en cours
affiche_bourse(Bourse) :-
	nl,
	nl, write('---------------- Bourse ----------------'),
	affiche_marchandise(Bourse).

%affiche_marchandise(+Bourse)
%Affichage de la marchandise en cours ainsi que sa valeur
affiche_marchandise([]).
affiche_marchandise([[M|[V]]|Q]) :-
	nl,
	write(M),
	write(' : '),
	write(V),
	affiche_marchandise(Q).

%affiche_pile(+Marche, +Trader)
%Affichage du marché avec la position du trader
affiche_pile(Marche, Trader) :-
	nl,
	write('----------------- Piles de jeu -----------------'),nl,
	affiche_pile(Marche, Trader, 1),nl,!.
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

%affiche_reserve(+Joueur, +Reserve)
%Affichage d'une réserve d'un joueur
affiche_reserve(X,L):-nl,write('Reserve Joueur'),write(X),write(' : '), write(L),nl.

%affiche_plateau(+Plateau)
%Affichage du plateau
affiche_plateau([Marche,Bourse,Trader,Res1,Res2]) :-
	nl,write('============== PLATEAU DE JEU ==================='),nl, write('================================================='),nl,
	affiche_pile(Marche, Trader),

	affiche_bourse(Bourse),

	nl, write('--------------- Reserves ---------------'),
	affiche_reserve(1,Res1),
	affiche_reserve(2,Res2),
	 write('================================================'),nl,nl.



/*_______________________FONCTIONS DE SERVICE__________________________*/


%retire la tete de la pile M de rang N1 et N2 et l'affiche
%pop(+IndexGauche, +IndexDroite, +Marche, ?NouveauMarche, ?MarchandiseGauche, ?MarchandiseDroite, ?Vide)
%Enlève les marchandises des piles à l'index IndexGauche et IndexDroite du marché, renvoie le nouveau marché,
% renvoie les marchandises enlevées
%Vide = 1 si une pile a été supprimée (car vide)
%Vide = 0 sinon
pop(N1,N2,M,NewM,T1,T2, Vide):-
	write('Marchandises enlevées:'),nl,
	nth1(N1, M, [T1|Q1]),
	write('1)'),
	write(T1),
	nl,
	nth1(N2, M, [T2|Q2]),
	write('2)'),
	write(T2),
	nl,
	replace(M, N1, Q1, Tmp),
	delete(Tmp, [], TmpM),
	length(TmpM, TmpL),
	length(M, OldL),
	replace(TmpM, N2, Q2, Tmp2),
	delete(Tmp2, [], NewM),
	(TmpL == OldL -> Vide is 0; Vide is 1),
	!.

%replace(+Liste,+Index,+NouvelleValeur, ?NouvelleListe)
remplace l'éement de rang Index de la liste Liste par NouvelleValeur
replace([_|Q], 1, X, [X|Q]).
replace([T|Q], I, X, [T|R]):- I > 0, NI is I-1, replace(Q, NI, X, R), !.
replace(L, _, _, L).

/*_____________INITIALISATION PLATEAU _____________*/

%nth1RandDelete(?Element, +Liste, ?NouvelleListe)
%Le prédicat choisit un élement aléatoire dans la liste Liste, renvoie dans Element et renvoie la nouvelle liste dans
%NouvelleListe
nth1RandDelete(Y, [X|R], Z):-
	length([X|R], Long),
	%Random entre deux entiers compris
	random_between(1, Long, Tmp),
	%RÃ©cupÃ¨re l'Ã©lÃ©ment Ã  l'index Tmp, et rÃ©cupÃ¨re l'Ã©lement dans Y
	nth1(Tmp, [X|R], Y),
	%Supprime l'Ã©lÃ©ment Ã  l'index Tmp, et mets la liste finale dans Z
	nth1(Tmp, [X|R], _, Z).

%Initialisation de la bourse
initBourse([[ble,7],[riz,6],[cacao,6],[cafe,6],[sucre,6],[mais,6]]).

%initTrader(?Position)
%Initialise la position du trader
initTrader(X) :-
	random(1,9, X).

%Initialise la liste de marchandises disponibles (chaque marchandise est répétée 6 fois)
initLimites([ble, ble, ble, ble, ble, ble, riz, riz, riz, riz, riz, riz, cacao, cacao, cacao, cacao, cacao, cacao,
			cafe, cafe, cafe, cafe, cafe, cafe, sucre, sucre, sucre, sucre, sucre, sucre, mais, mais, mais, mais, mais, mais]).

%initMarchandise(?Marchandise, +ListeMarchandises, ?NouvelleListe)
%M = marchandise, [Y|R] = Liste marchandise modif, Z = liste marchandise aprÃ¨s modif
initMarchandise(M, [Y|R], Z) :-
	nth1RandDelete(M, [Y|R], Z).

%initPile(?Pile, +ListeMarchandises, ?NouvelleListeMarchandise)
%Initialisation de la Pile, Final est la liste modifiÃ©e (sans les marchandises ajoutÃ©es)
%Une Pile est liste de 4 marchandises
initPile([A, B, C, D], X, Final):-
	initMarchandise(A, X, Z),
	initMarchandise(B, Z, Y),
	initMarchandise(C, Y, M),
	initMarchandise(D, M, Final).

%initMarche(?Marche)
%Initilisation du marchÃ© (Une liste qui reprÃ©sente 9 piles))
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

%plateau_depart(?Plateau)
%Initialisation du plateau de type [Marche, Bourse, Trader, [Reserve J1], [Reserve J2]])
plateau_depart([Marche, Bourse, Trader,[],[]]) :-
initMarche(Marche),
initBourse(Bourse),
initTrader(Trader).

/*_____________FIN DU JEU_____________*/
%Calcul somme J1
%somme(+Bourse, +ReserveJoueur, ?Score)
%Le prédicat calcule le score d'un joueur selon sa réserve et la bourse en cours
somme(_,[],0):-!.
somme(B,[T|Q],N):-getValeurMarchandise(T,B,R), somme(B,Q,N1),  N is N1+R.

%gagnant(+Plateau, ?JoueurGagnant)
%Le prédicat détermine le gagnant du jeu selon leur score
gagnant([_,B,_,J1,J2],1):-somme(B,J1,SJ1),somme(B,J2,SJ2),SJ1>SJ2,!.
gagnant([_,B,_,J1,J2],2):-somme(B,J1,SJ1),somme(B,J2,SJ2),SJ2>SJ1,!.

/*_____________INTEL ARTIFICIELLE_____________*/

%coups_possibles(+Plateau, +Deplacement, +JoueurEnCours, ?ListeCoupsPossibles)
%Ce prédicat détermine tous les coups possibles selon un déplacement et le plateau actuel
coups_possibles(_, 0, _, []).
coups_possibles([Marche,Bourse,Trader,ResJ1,ResJ2],Dep, Joueur,[[Joueur, Dep, MarchG, MarchD]|[[Joueur, Dep, MarchD, MarchG]|Liste]]):-
	getPosTrader(Dep, [Marche,Bourse,Trader,ResJ1,ResJ2],NewTrader),
	getvoisins(Marche,NewTrader,G,D),
	getMarchandise(G, D, Marche, MarchG, MarchD),
	NewDep is Dep-1,
	coups_possibles([Marche,Bourse,Trader,ResJ1,ResJ2],NewDep,Joueur,Liste),!.

%getMarchandise(+IndexGauche, +IndexDroite, +Marche, ?MarchandiseGauche, ?MarchandiseDroite)
%Ce prédicat récupère les marchandises à la tête des piles à l'index IndexGauche et IndexDroit
getMarchandise(IndexG, IndexD, Marche, MarchG, MarchD) :-
        nth1(IndexG, Marche, [MarchG|_]),
	nth1(IndexD, Marche, [MarchD|_]).

%pop_voisins(+Marche, ?NouveauMarche, +IndexVoisinGauche, +IndexVoisinDroite, ?Vide)
%Ce prédicat enlève la tête des piles IndexVoisinGauche et IndexVoisinDroite et renvoie le nouveau marché
%Vide = 1 si une pile est supprimée (car vide)
%Vide = 0 sinon
pop_voisins(Marche, NewM, Voisin1, Voisin2, Vide) :-
	nth1(Voisin1, Marche, [_|Q1]),
	nth1(Voisin2, Marche, [_|Q2]),
        replace(Marche, Voisin1, Q1, Tmp),
        delete(Tmp, [], TmpM),
        length(TmpM, TmpL),
        length(Marche, OldL),
        replace(TmpM, Voisin2, Q2, Tmp2),
        delete(Tmp2, [], NewM),
        (TmpL == OldL -> Vide is 0; Vide is 1),!.

%simuler_coup(+Plateau, +CoupEnCours, ?NouveauMarche)
%Ce prédicat simule le coup CoupEnCours et renvoie un NouveauMarche
simuler_coup([Marche,Bourse,Trader,J1,J2], [1, Deplacement, Gardee, Jetee], [NewM,NewB,NewT,NewJ1,J2]) :-
		%write('Trader: '), write(Trader), nl,
        getPosTrader(Deplacement, [Marche, Bourse, Trader, J1, J2], NewPos),
        getvoisins(Marche, NewPos, V1, V2),
        pop_voisins(Marche, NewM, V1, V2, Vide),
        (Vide==1 -> NewT is NewPos-1; NewT is NewPos),
        %nl,write('Nouvelle position: '),write(NewT),nl,
        setValeurMarchandise(Jetee, Bourse, NewB),
        ajouterReserve(Gardee,J1,NewJ1).
simuler_coup([Marche,Bourse,Trader,J1,J2], [2, Deplacement, Gardee, Jetee], [NewM,NewB,NewT,J1,NewJ2]) :-
        getPosTrader(Deplacement, [Marche, Bourse, Trader, J1, J2], NewPos),
        %nl,write('Nouvelle position: '),write(NewPos),nl,
        getvoisins(Marche, NewPos, V1, V2),
        pop_voisins(Marche, NewM, V1, V2, Vide),
        (Vide==1 -> NewT is NewPos-1; NewT is NewPos),
        setValeurMarchandise(Jetee, Bourse, NewB),
		ajouterReserve(Gardee,J2,NewJ2).

%ajouterReserve(+Marchandise, +ReserveduJoueur, ?NouvelleReserve)
%Ce prédicat ajoute la marchandise à la réserve
ajouterReserve(March, Res,[March|Res]).

%iafinal(+Plateau, +JoueurEnCours, ?NouveauPlateau)
%Ce prédicat est le début de l'IA et fait un coup avec l'IA
iafinal([Marche,Bourse,Trader,ResJ1,ResJ2], Joueur, NewPlateau):-
	ia([Marche,Bourse,Trader,ResJ1,ResJ2],Joueur,NewPlateau),
	affiche_plateau(NewPlateau).


%ia(+Plateau, +JoueurEnCours, ?NouveauPlateau)
ia([Marche,Bourse,Trader,ResJ1,ResJ2], Joueur, PlateauTmp):-
	coups_possibles([Marche,Bourse,Trader,ResJ1,ResJ2], 3, Joueur, ListeCoups),
	%write('Liste de coups: '), write(ListeCoups),nl,
	main([Marche,Bourse,Trader,ResJ1,ResJ2],Joueur,ListeCoups, MeilleurCoup),
	%write('Meilleur coup: '), write(MeilleurCoup),
	%on joue le meilleurcoup trouve par main
	simuler_coup([Marche,Bourse,Trader,ResJ1,ResJ2], MeilleurCoup,PlateauTmp).

%Déroulement d'un seul coup
%main(+Plateau, +JoueurEnCours, +ListeCoupsPossibles, ?MeilleurCoup, ?MeilleurScore)
%Ce prédicat déroule un seul coup et calcule le meilleur coup possible
main(_,_,[],[],0).
main(Plateau, Joueur, [T|Q],MeilleurCoup):- main(Plateau, Joueur, [T|Q], MeilleurCoup, MeilleurScore).
main(Plateau, Joueur, [T|Q], MeilleurCoup, MeilleurScore):-
	%nl,write('Coup en cours: '), write(T),
	main(Plateau,Joueur,Q,MeilleurCoupTmp,MeilleurScoreTmp),
	simuler_coup(Plateau,T,NewP),
	%affiche_plateau(NewP),
	%Score du coup en cours
	scoreTemporaire([Marche,Bourse,Trader,ResJ1,ResJ2],Joueur,Score),
	length(Marche, M),
	(M > 2 ->
		%Score du joueur adverse pour ce coup
		scoreAdverse(Joueur, NewP, Res),
		Tmp is Res-Score,
		Difference is abs(Tmp),nl,
		(Difference>=MeilleurScoreTmp ->
		     MeilleurScore is Difference, MeilleurCoup=T;
		     MeilleurScore is MeilleurScoreTmp,MeilleurCoup= MeilleurCoupTmp);
		MeilleurScore is Score, MeilleurCoup = T,!).

%scoreAdverse(+JoueurEnCours, +Plateau, ?ScoreAdverse)
%Ce prédicat retourne le score du meilleur coup du joueur adverse
scoreAdverse(Joueur,[Marche,Bourse,Trader,ResJ1,ResJ2],Res):-
	change(Joueur, NewJoueur),
	coups_possibles([Marche,Bourse,Trader,ResJ1,ResJ2],3, NewJoueur,Liste),
	meilleurscore([Marche,Bourse,Trader,ResJ1,ResJ2], NewJoueur, Liste, ListeMax),
	max_member(Res, ListeMax).

%meilleurscore(+Plateau, +JoueurEnCours, +ListeCoupsPossibles, ?ListeScores)
%Ce prédicat calcule le meilleur score du joueur adverse selon les coups possibles
meilleurscore(_, _, [], []).
meilleurscore([Marche,Bourse,Trader,ResJ1,ResJ2], Joueur, [T|Q],[Score|Liste]):-
	%write('Coup possible'), write(T),nl,
	meilleurscore([Marche,Bourse,Trader,ResJ1,ResJ2], Joueur, Q, Liste),
	simuler_coup([Marche,Bourse,Trader,ResJ1,ResJ2],T, NewPlateau),
	%nl,write('------ Adversaire ---------'),
	%affiche_plateau(NewPlateau),
	scoreTemporaire(NewPlateau, Joueur, Score).

%scoreTemporaire(+Plateau, +JoueurEnCours, ?Score)
%Ce prédicat calcule le score temporaire du joueur actuel selon sa réserve
scoreTemporaire([_,Bourse,_,J1,_], 1, N):-
        somme(Bourse, J1, N),!.
scoreTemporaire([_, Bourse, _, _, J2], 2, N):-
        somme(Bourse, J2, N),!.
