%%%%%%%%%%%% miniMax-sample.pl %%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Example of minimax with just two branches, 				%%
%% to understand the mecanism and avoid burning your brain  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For test purpose, this module contains duplicate code.%%
%% do not remove it.									 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%
%% Constantes %%
%%%%%%%%%%%%%%%%

nbLignes(6).
nbColonnes(7).
infinitePos(10000).
infiniteNeg(-10000).



%%%%%%%%%%%%%%%%%%%%%%%
%% Pr�dicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% +J player qui doit jouer
% +Pmax prof maximale
% -R le coup a jouer
% -Value �valuation du noeud courant
parcoursArbre(J,Pmax,R,Value):- initTest,initCaseTest,infinitePos(InfP),infiniteNeg(InfN),assert(maximizer(J)), assert(joueurCourant(J)), parcours(1,1,Pmax,[1,0],InfP,InfN),setJoueur(1), feuille([1,0],X1), parcours(2,1,Pmax,[2,0],InfP,X1), feuille([2,0],X2), coupAJouerMaximizer([X1,X2],R,Value), clearTest,!. %the second call and the next ones are called with the result of the preceding (we take the max of all of them) on reset le joueur entre chaque call

%%%%%%%%%%%%%%%%%%%%%%
%% Pr�dicats priv�s %%
%%%%%%%%%%%%%%%%%%%%%%
initTest:-assert(case(-20,-20,jaune)). %juste pour definir case

initCaseTest:- case(X,Y,Z), assert(caseTest(X,Y,Z)),false. %on assert une caseTest pour toutes les cases.
initCaseTest.

clearTest:-retractall(caseTest(A,B,C)),retractall(feuille(D,E)),retract(maximizer(X)),retract(joueurCourant(J)),retract(case(-20,-20,jaune)). % on enleve tout ce que l'on a ajout�.



%Parcours
%+X la colonne a jouer
%+P la profondeur courante
%+L la liste de coups courante
%+Beta,+Alpha les valeurs courantes pour Alpha et Beta.
parcours(X, P, Pmax, L, Beta, Alpha):- nbLignes(MaxLignes),not(caseVideTest(X,MaxLignes)),write('feuille colonne pleine'), print(L), evaluate(Value), assert(feuille(L, Value)) .% on ne peut plus jouer, on met une feuille (on �value)
parcours(X, P, Pmax, L, Beta, Alpha):- P==Pmax,joueurCourant(Joue), placerJeton(X,Y,Joue),nl, write('feuille'), print(L), evaluate(Value),assert(feuille(L, Value)),write(" Value of node "),print(Value),nl, afficher,retract(caseTest(X,Y,Joue)). % on est � la prof max, on evalue et on met une feuille
parcours(X, P, Pmax, L, Beta, Alpha) :- incr(P, P1),joueurCourant(Joue), placerJeton(X,Y,Joue), %on incremente la profondeur, puis on joue un coup(qui r�ussit a tous les coups)
setJoueur(P1), %on set le joueur
attribueVal(ValeurPrec), % on initialise val
parcours(1, P1,Pmax, [1|L], Beta, Alpha),  %on joue colonne 1
feuille([1|L], Valeur1),%here is the value of first branch
setJoueur(P1), % on reset le joueur (il a chang� dans le premier parcours)
choixVal(Valeur1,ValeurPrec,Val1),%choisit si min ou max, renvoie la valeur pour le prochain coup.
joueCoupSuivant(Val1,2,P1,Pmax,L,Beta,Alpha,Valeur,Beta2,Alpha2),%on tente le coup suivant (ou pas si elaguage), avec la valeur retourn�e par le pr�c�dent
retract(caseTest(X,Y,Joue)), %on annule le coup pour poursuivre dans l'arbre
feuille([1|L], X1),feuille([2|L], X2), %on cherche les feuilles associ�es (elles ont �t� calcul�es plus bas dans l'arbre)
setJoueur(P1), %on change de joueur
assert(feuille(L,Valeur)),nl,joueurCourant(Joueur),write(Joueur), write(' feuille niv '),write(P),write( " "), print(L),write(" Value of Node "),print(Valeur),nl. %on met notre feuille calcul�e






%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Elaguage alpha beta%  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


choixVal(Valeur1,ValeurPrec,Val1):- joueurCourant(Joue), maximizer(Joue), Val1 is max(Valeur1, ValeurPrec),nl,write(" Maximizer between "),print(Valeur1), write(" and "),print(ValeurPrec),nl. % we choose after the first choice if we take max or min for val
choixVal(Valeur1,ValeurPrec,Val1):- Val1 is min(Valeur1, ValeurPrec),nl,write(" Minimizer between "),print(Valeur1), write(" and "),print(ValeurPrec),nl .

attribueVal(X):- infiniteNeg(InfN), joueurCourant(Joue), maximizer(Joue), X is InfN. % the initial value of a node (-inf if maximizer, +inf if minimizer)
attribueVal(X):-infinitePos(InfP), X is InfP.


%%%For the Minimizer
joueCoupSuivant(ValeurPrec,ColonneAJouer,P1,Pmax,L,Beta,Alpha,Val,Beta,Alpha):-joueurCourant(Joue), not(maximizer(Joue)), ValeurPrec =< Alpha, Val is ValeurPrec, assert(feuille([ColonneAJouer|L], Val)),write("coupure alpha because "),print(ValeurPrec),write("<="),print(Alpha).%coupure alpha !!
joueCoupSuivant(ValeurPrec,ColonneAJouer,P1,Pmax,L,Beta,Alpha,Val,BetaCalc,Alpha):-joueurCourant(Joue), not(maximizer(Joue)), BetaCalc is min(Beta, ValeurPrec),nl, parcours(ColonneAJouer, P1,Pmax,[ColonneAJouer|L],BetaCalc, Alpha), feuille([ColonneAJouer|L], ValeurFils), Val is min(ValeurFils, ValeurPrec), nl,write(" Minimizer between "),print(ValeurFils), write(" and "),print(ValeurPrec),nl . %pas de coupure!


%%For the Maximizer
joueCoupSuivant(ValeurPrec,ColonneAJouer,P1,Pmax,L, Beta, Alpha,Val,Beta,Alpha):-joueurCourant(Joue), maximizer(Joue), ValeurPrec >= Beta, Val is ValeurPrec,assert(feuille([ColonneAJouer|L], Val)),write("coupure beta because "),print(ValeurPrec),write(">="),print(Beta).%coupure beta !!
joueCoupSuivant(ValeurPrec,ColonneAJouer,P1,Pmax,L, Beta, Alpha,Val,Beta,AlphaCalc):-joueurCourant(Joue), maximizer(Joue), AlphaCalc is max(Alpha, ValeurPrec), parcours(ColonneAJouer, P1,Pmax,[ColonneAJouer|L],Beta, AlphaCalc),feuille([ColonneAJouer|L], ValeurFils),Val is max(ValeurFils, ValeurPrec),nl,write(" Maximizer between "),print(ValeurFils), write(" and "),print(ValeurPrec),nl. %pas de coupure!





%%%%%%%%%% Aide calcul. Redundant code, do not remove %%%%%%%%%%%%%
setJoueur(P):- parite(P), maximizer(jaune), joueurCourant(jaune),retract(joueurCourant(X)), assert(joueurCourant(rouge)),!. % si P pair, alors c'est au minimizer de jouer
setJoueur(P):- parite(P), maximizer(rouge), joueurCourant(rouge),retract(joueurCourant(X)), assert(joueurCourant(jaune)),!.
setJoueur(P):- not(parite(P)),maximizer(rouge), joueurCourant(jaune),retract(joueurCourant(X)), assert(joueurCourant(rouge)),!. % P impair, maximizer joue
setJoueur(P):- not(parite(P)),maximizer(jaune), joueurCourant(rouge),retract(joueurCourant(X)), assert(joueurCourant(jaune)).
setJoueur(P).

evaluate(X):- X is random(10). %fonction d'�valuation al�atoire

choixValeurNoeud(L,R,Value):- joueurCourant(X), maximizer(X), nl,write('maximizer algo '), write(X), coupAJouerMaximizer(L,R,Value),!. %on choisit val min ou max
choixValeurNoeud(L,R,Value):- nl,write('minimizer algo'),coupAJouerMinimizer(L,R,Value).


coupAJouerMaximizer(L, R,X):- membreMaxRank(X,L,R).
coupAJouerMinimizer(L, R,X):- membreMinRank(X,L,R).


parite(X):- divmod(X ,2, Q, R), R==0.

changerJoueur:- joueurCourant(jaune), retract(joueurCourant(X)), assert(joueurCourant(rouge)).
changerJoueur:- joueurCourant(rouge), retract(joueurCourant(X)), assert(joueurCourant(jaune)).

membreMaxRank(X, [Y|L], R):- membreMax(L, 1, 1, R, Y, X),!.
membreMax([], R, Rm, Rm, Max, Max).		   
membreMax([Y|L], R1, Rm, R, Max, X):- incr(R1,R2), maximum(Y, Max, Rm, R2, NewMax, NewRankMax), membreMax(L,R2,NewRankMax,R, NewMax, X).

membreMinRank(X, [Y|L], R):- membreMin(L, 1, 1, R, Y, X),!.
membreMin([], R, Rm, Rm, Max, Max).		   
membreMin([Y|L], R1, Rm, R, Max, X):- incr(R1,R2), minimum(Y, Max, Rm, R2, NewMax, NewRankMax), membreMin(L,R2,NewRankMax,R, NewMax, X).

minimum(Y, Max, OldRankMax, NewRankMax, Y, NewRankMax):- Y<Max.
minimum(Y, Max,OldRankMax, NewRankMax, Max, OldRankMax).  

maximum(Y, Max, OldRankMax, NewRankMax, Y, NewRankMax):- Y>Max.
maximum(Y, Max,OldRankMax, NewRankMax, Max, OldRankMax).  



%%% Place un jeton

% placerJeton/3(-Colonne, +Ligne, -Couleur) 
% ins�re si possible un jeton dans la colonne donn�e
% retourne la ligne d'insertion, ou no
placerJeton(X,Y,C) :- coupValide(X), insererJeton(X, Y, C).
%%%%% placerJeton %%%%%
% coupValide/1(-Colonne)
% V�rifie si un jeton est jouable dans cette colonne
% retourne yes ou no
coupValide(X) :- nbColonnes(NBCOLONNES), X=<NBCOLONNES, X>=1, nbLignes(NBLIGNES), caseVideTest(X,NBLIGNES).

% insererJeton/3(-Colonne, +Ligne, -Couleur)
% Insere, sans v�rification, un jeton de la couleur donn�e, dans la colonne donn�e
% retourne la ligne d'insertion, 
insererJeton(X,Y,C) :- calculPositionJeton(X, 1, Y), assert(caseTest(X,Y,C)).

% calculPositionJeton/3(+Colonne,+LigneToCheck,-Ligne)
% calcule la premiere ligne vide d'une colonne
% retourne l'indice de cette ligne vide
calculPositionJeton(X,YCheck,YCheck) :- caseVideTest(X,YCheck), !.
calculPositionJeton(X,YCheck,Y) :- incr(YCheck, YCheck1), calculPositionJeton(X,YCheck1,Y).

%%%%%%%%%%%%%%%



% incr/2(+X, -X1)
% unifie X1 � X+1
% vrai pour X1 = X+1
incr(X,X1):- X1 is X+1.

% decr/2(+X, -X1)
% unifie X1 � X-1
% vrai pour X1 = X-1
decr(X,X1):- X1 is X-1.

caseVideTest(X,Y) :- nonvar(X),nonvar(Y),not(caseTest(X,Y,_)).


%%%%%%%%%%%%%%%%%� ONLY for Testing%%%%

% afficher/0
% Affiche dans la console la partie actuelle.
% Tout le temps vrai.
afficher :-
	findall(_, afficherPlateau(_), _).

% principe : on parcourt la base de faits et pour chaque case on affiche une couleur (ou pas)
afficherPlateau(Y) :-
	nbLignes(NbLignes),
	between(1,NbLignes,Y1),
	Y is 7-Y1,
	findall(_, afficherLigne(_,Y), _),
	nl.

afficherLigne(X,Y) :-
	nbColonnes(NbColonnes),
	between(1,NbColonnes,X),
	afficherCase(X,Y).

afficherCase(X,Y) :- caseTest(X,Y,rouge), write(r), !.
afficherCase(X,Y) :- caseTest(X,Y,jaune), write(j), !.
afficherCase(_,_) :- write(.).