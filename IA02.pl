
/*____________________________MAIN MENU___________________________ */
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
%Game between 2 bots
jeuOrdi([Marche,Bourse,Trader,ResJ1,ResJ2],JoueurenCours) :-
	length(Marche, Res),
	(Res > 2 -> 
		iafinal([Marche,Bourse,Trader,ResJ1,ResJ2],JoueurenCours, NewP), change(JoueurenCours, NewJoueur), 
		jeuOrdi(NewP, NewJoueur); gagnant([Marche,Bourse,Trader,ResJ1,ResJ2], G), nl, write('Le gagnant est le joueur '), write(G)).

%jeuHumainHumain(+Plateau, +Joueur)
%Game between 2 human players.
jeuHumainHumain([Marche,Bourse,Trader,ResJ1,ResJ2],JoueurenCours):-
	length(Marche, Res),
	(Res > 2 -> jouer_coup([Marche,Bourse,Trader,ResJ1,ResJ2], JoueurenCours, NewP),
	 change(JoueurenCours, NewJ), jeuHumainHumain(NewP, NewJ); gagnant([Marche,Bourse,Trader,ResJ1,ResJ2], G), nl, write('Le gagnant est le joueur '), write(G)).

%jeuHumainOrdi(+Plateau, +Joueur)
%Game between a bot and a human player
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
%Display the menu to choose a movement
liredeplacement(X) :-
	write('Chose your movement (1,2,3)'), nl,
	read(X),
	liredep(X), !.
liredeplacement(X) :-
	nl,write('1, 2 OR 3 !!!!!'),nl,
	liredeplacement(X).

liredep(1):-!.
liredep(2):-!.
liredep(3):-!.

%SECURITE CHOIX MARCHANDISE (1 ou 2)
%liremarchandise(?Deplacement)
%Display the menu to choose the commodity
liremarchandise(X) :-
	write('Which one are you keeping ? (1 ou 2)'),
	nl,read(X),
	liremarch(X),!.
liremarchandise(X) :- nl,write('1 OR 2 !!!!!'),nl,liremarchandise(X).

liremarch(1):-!.
liremarch(2):-!.


/*_____________________________ JOUER COUP _____________________________*/
%jouer_coup(+Plateau, +Joueur, ?NouveauPlateau)
%Handle the move of a human player
jouer_coup([Marche,Bourse,Trader,ResJ1,ResJ2],JoueurenCours, [NewM, NewB, TmpPos, NewJ1, NewJ2]):-
	liredeplacement(Deplacement),
	getPosTrader(Deplacement,[Marche,Bourse,Trader,ResJ1,ResJ2],NewPos),
	affiche_pile(Marche, NewPos),
	newMarcheBourseRes([Marche,Bourse,NewPos,ResJ1,ResJ2],[NewM,NewB,TmpPos,NewJ1,NewJ2], JoueurenCours),
	affiche_plateau([NewM, NewB, TmpPos, NewJ1, NewJ2]).

%change(+Joueur, ?AutreJoueur)
%Change player
change(1,2).
change(2,1).

%RETURN THE NEW POSITION OF THE TRADER
%getPosTrader(+Deplacement, +Plateau, ?NouveauTrader)
%return the new position of trader depending on its position and the size of the board game
getPosTrader(D,[M,_,T,_,_],NewT):-
	length(M,Len),
	TmpT is D+T,
	modulo(TmpT, Len, NewT).

%RETURN NEW BOARD GAME, UPDATE THE STOCKPILE
%newMarcheBourseRes(+Plateau, ?NouveauPlateau, +JoueurEnCours)
%Get the trader's neighbors, update the Board Game 
%Update the Stockpile and the Stock Exchange
newMarcheBourseRes([M,B,T,J1,J2], [NewM,NewB,NewT,NewJ1,J2], 1):-
	getvoisins(M,T,VoisinG,VoisinD),
	pop(VoisinG, VoisinD, M, NewM,M1,M2, Vide),
	liremarchandise(Choix),
	(Vide==1 -> NewT is T-1; NewT is T),
	addReserve(Choix,M1,M2,J1,NewJ1,Vendue),
	setValeurMarchandise(Vendue, B, NewB).

%newMarcheBourseRes(+Plateau, ?NouveauPlateau, +JoueurEnCours)
newMarcheBourseRes([M,B,T,J1,J2], [NewM,NewB,NewT,J1,NewJ2], 2):-
	getvoisins(M,T,VoisinG,VoisinD),
	pop(VoisinG, VoisinD, M, NewM,M1,M2, Vide),
	liremarchandise(Choix),
	(Vide==1 -> NewT is T-1; NewT is T),
	addReserve(Choix,M1,M2,J2,NewJ2,Vendue),
	setValeurMarchandise(Vendue,B,NewB).

%getvoisins(+Marche, +Trader, ?IndexGauche, ?IndexDroit)
%Return the pile's index of the left and right side of the Trader's position
getvoisins(M,T,TG,TD):-
	length(M,Len),
	(T==1 -> TmpGauche is Len;
	TmpGauche is T-1),
	modulo(TmpGauche, Len, TG),
	TmpDroite is T+1,
	modulo(TmpDroite, Len, TD).

%Add to 
%addReserve(+Choix, +Marchandise1, +Marchandise2, +ReserveJoueurEnCours, ?NouvelleReserveJoueur, ?MarchandiseJetee)
%Update the current player's Stockpile and return the commodity thrown away
addReserve(1,M1,M2,ResenCours,[M1|ResenCours],M2).
addReserve(2,M1,M2,ResenCours,[M2|ResenCours],M1).

%getValeurMarchandise(+Marchandise, +Bourse, ?ValeurMarchandise)
%Get the commodity's value depending on the Stock Exchange
getValeurMarchandise(M, [[M|[Q]]|_], Q):-!.
getValeurMarchandise(M, [_|Q], V):- getValeurMarchandise(M, Q, V).

%setValeurMarchandise(+Marchandise, +BourseActuelle, +Valeur, ?NouvelleBourse)
%Decrease the value of a commodity in the Stock Exchange
setValeurMarchandise(M, [[M|[OldV]]|Q2], [[M|[V]]|Q2]):- V is OldV-1,!.
setValeurMarchandise(M, [T|Q],[T|B]):- setValeurMarchandise(M, Q, B),!.

%modulo(+X, +Y, ?Z)
% Implementation of an adapted modulo
modulo(6,3,1).
module(4,2,1).
modulo(X,Y,Z):- X > Y, Z is X mod Y,!.
modulo(X,_,X).



/*____________________ AFFICHAGE PLATEAU DE JEU _______________________*/
%affiche_bourse(+Bourse)
%Display the current Stock Exchange
affiche_bourse(Bourse) :-
	nl,
	nl, write('---------------- Bourse ----------------'),
	affiche_marchandise(Bourse).

%affiche_marchandise(+Bourse)
%Display the current commodity and its value
affiche_marchandise([]).
affiche_marchandise([[M|[V]]|Q]) :-
	nl,
	write(M),
	write(' : '),
	write(V),
	affiche_marchandise(Q).

%affiche_pile(+Marche, +Trader)
%Display the Board Game with the Trader's position
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
%Display a player's Stockpile
affiche_reserve(X,L):-nl,write('Reserve Joueur'),write(X),write(' : '), write(L),nl.

%affiche_plateau(+Plateau)
%Display the Boardgame
affiche_plateau([Marche,Bourse,Trader,Res1,Res2]) :-
	nl,write('============== PLATEAU DE JEU ==================='),nl, write('================================================='),nl,
	affiche_pile(Marche, Trader),

	affiche_bourse(Bourse),

	nl, write('--------------- Reserves ---------------'),
	affiche_reserve(1,Res1),
	affiche_reserve(2,Res2),
	 write('================================================'),nl,nl.



/*_______________________FONCTIONS DE SERVICE__________________________*/


%Pop the piles ranked N1 & N2 from the Boardgame M
%pop(+IndexGauche, +IndexDroite, +Marche, ?NouveauMarche, ?MarchandiseGauche, ?MarchandiseDroite, ?Vide)
%Return the new Boardgame and the commodity thrown away
%Vide = 1 if one of the pile was deleted (because empty)
%Vide = 0 otherwise
pop(N1,N2,M,NewM,T1,T2, Vide):-
	write('Commodities thrown away:'),nl,
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
Replace the "Index" ranked element of the list "Liste" by "NouvelleValeur"
replace([_|Q], 1, X, [X|Q]).
replace([T|Q], I, X, [T|R]):- I > 0, NI is I-1, replace(Q, NI, X, R), !.
replace(L, _, _, L).

/*_____________BOARDGAME INITIALISATION _____________*/

%nth1RandDelete(?Element, +Liste, ?NouvelleListe)
%Chooses a random element in the list "Liste", returns it in "Element" and return the new list in "NouvelleListe"
nth1RandDelete(Y, [X|R], Z):-
	length([X|R], Long),
	%Random entre deux entiers compris
	random_between(1, Long, Tmp),
	%RÃ©cupÃ¨re l'Ã©lÃ©ment Ã  l'index Tmp, et rÃ©cupÃ¨re l'Ã©lement dans Y
	nth1(Tmp, [X|R], Y),
	%Supprime l'Ã©lÃ©ment Ã  l'index Tmp, et mets la liste finale dans Z
	nth1(Tmp, [X|R], _, Z).

%Stock Exchange Initialisation
initBourse([[ble,7],[riz,6],[cacao,6],[cafe,6],[sucre,6],[mais,6]]).

%initTrader(?Position)
%Trader's position initialision
initTrader(X) :-
	random(1,9, X).

%Commodities' list initialision
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
%Calculated the player score depending on his StockPile and the Stock Exchange
somme(_,[],0):-!.
somme(B,[T|Q],N):-getValeurMarchandise(T,B,R), somme(B,Q,N1),  N is N1+R.

%gagnant(+Plateau, ?JoueurGagnant)
%Finds out the game's winner depending on their score
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
