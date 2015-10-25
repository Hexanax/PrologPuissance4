%%%%%%%%%%%% jeu.pl %%%%%%%%%%%%

:- module(jeu, [nbLignes/1, nbColonnes/1, initJeu/0, gagne/3, placerJeton/3, coupPossible/0, case/3, caseVide/2]).

:- use_module(util).

:- dynamic case/3. % � tester

%%%%%%%%%%%%%%%%
%% Constantes %%
%%%%%%%%%%%%%%%%

nbLignes(6).
nbColonnes(7).

%%%%%%%%%%%%%%%%%%%%%%%
%% Pr�dicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

%%% Fonctions utiles

% caseVide/2(+X, +Y)
% verifie si la case est vide
% vrai si la case n'a pas �t� remplie
caseVide(X,Y) :- nonvar(X),nonvar(Y),not(case(X,Y,_)).

%%% Initialisation du plateau

% initJeu/0
% vide le plateau, initialise un nouveau plateau vide
% retourne yes
initJeu :- initClear, assert(case(_,_,_) :- fail).

% coupPossible/0
% verifie si l'on peut encore joueur
% vrai si il reste des coups valides, faux sinon
coupPossible :- nbColonnes(NBCOLLONNES), between(1,NBCOLLONNES,X), coupValide(X).

%%% V�rification de la victoire 

% gagne/3(+colonne, +ligne, +joueur)
% v�rifie si le coup est gagnant pour joueur
% retourne yes si gagnant ou no sinon
gagne(X,Y,J) :- gagneColonne(X,Y,J).
gagne(X,Y,J) :- gagneLigne(X,Y,J).
gagne(X,Y,J) :- gagneDiag1(X,Y,J).
gagne(X,Y,J) :- gagneDiag2(X,Y,J).


%%% Place un jeton

% placerJeton/3(-Colonne, +Ligne, -Couleur) 
% ins�re si possible un jeton dans la colonne donn�e
% retourne la ligne d'insertion, ou no
placerJeton(X,Y,C) :- coupValide(X), insererJeton(X, Y, C).

%%%%%%%%%%%%%%%%%%%%%%
%% Pr�dicats priv�s %%
%%%%%%%%%%%%%%%%%%%%%%


%%%%% init %%%%%


initClear :- retractall(case(_,_,_)). % pourrait fonctionner avec :- dynamic, � investiguer

initTest :- assert(case(4,1,rouge)), assert(case(3,2,rouge)), assert(case(2,3,rouge)), assert(case(1,4,rouge)). %initInterface, play


%%%%% gagne %%%%%


%%% En colonne %%%

gagneColonne(X,Y,J) :- case(X,Y,J), decr(Y,Y1), case(X,Y1,J), decr(Y1,Y2), case(X,Y2,J), decr(Y2,Y3), case(X,Y3,J). %ligne en bas

%%% En ligne %%%

gagneLigne(X,Y,J) :- gaucheVerif(X,Y,J,Rg), droiteVerif(X,Y,J,Rd),!, Rf is Rg+Rd, Rf>4.

gaucheVerif(X,Y,J,Rg):- gauche(X,Y,J,0,Rg).
gauche(X,Y,J,R,R) :- not(case(X,Y,J)). %Jusqu'� la case non J
gauche(X,Y,J,R,Rg) :- decr(X,X1), incr(R,R1), gauche(X1,Y,J,R1,Rg).

droiteVerif(X,Y,J,Rg):- droite(X,Y,J,0,Rg).
droite(X,Y,J,R,R) :- not(case(X,Y,J)). %Jusqu'� la case non J
droite(X,Y,J,R,Rg) :- incr(X,X1), incr(R,R1), droite(X1,Y,J,R1,Rg).

%%% En diagonale \ %%%

gagneDiag1(X,Y,J) :- gaucheHautVerif(X,Y,J,Rg), droiteBasVerif(X,Y,J,Rd),!, Rf is Rg+Rd, Rf>4.

gaucheHautVerif(X,Y,J,Rg):- gaucheHaut(X,Y,J,0,Rg).
gaucheHaut(X,Y,J,R,R) :- not(case(X,Y,J)). %Jusqu'� la case non J
gaucheHaut(X,Y,J,R,Rg) :- incr(Y,Y1), decr(X,X1), incr(R,R1), gaucheHaut(X1,Y1,J,R1,Rg).

droiteBasVerif(X,Y,J,Rg):- droiteBas(X,Y,J,0,Rg).
droiteBas(X,Y,J,R,R) :- not(case(X,Y,J)). %Jusqu'� la case non J
droiteBas(X,Y,J,R,Rg) :- decr(Y,Y1), incr(X,X1), incr(R,R1), droiteBas(X1,Y1,J,R1,Rg).

%%% En diagonale / %%%

gagneDiag2(X,Y,J) :- gaucheBasVerif(X,Y,J,Rg), droiteHautVerif(X,Y,J,Rd),!, Rf is Rg+Rd, Rf>4.

gaucheBasVerif(X,Y,J,Rg):- gaucheBas(X,Y,J,0,Rg).
gaucheBas(X,Y,J,R,R) :- not(case(X,Y,J)). %Jusqu'� la case non J
gaucheBas(X,Y,J,R,Rg) :- decr(Y,Y1), decr(X,X1), incr(R,R1), gaucheBas(X1,Y1,J,R1,Rg).

droiteHautVerif(X,Y,J,Rg):- droiteHaut(X,Y,J,0,Rg).
droiteHaut(X,Y,J,R,R) :- not(case(X,Y,J)). %Jusqu'� la case non J
droiteHaut(X,Y,J,R,Rg) :- incr(Y,Y1), incr(X,X1), incr(R,R1), droiteHaut(X1,Y1,J,R1,Rg).


%%%%% placerJeton %%%%%


% coupValide/1(-Colonne)
% V�rifie si un jeton est jouable dans cette colonne
% retourne yes ou no
coupValide(X) :- nbColonnes(NBCOLONNES), X=<NBCOLONNES, X>=1, nbLignes(NBLIGNES), caseVide(X,NBLIGNES).

% insererJeton/3(-Colonne, +Ligne, -Couleur)
% Insere, sans v�rification, un jeton de la couleur donn�e, dans la colonne donn�e
% retourne la ligne d'insertion, 
insererJeton(X,Y,C) :- calculPositionJeton(X, 1, Y), assert(case(X,Y,C)).

% calculPositionJeton/3(+Colonne,+LigneToCheck,-Ligne)
% calcule la premiere ligne vide d'une colonne
% retourne l'indice de cette ligne vide
calculPositionJeton(X,YCheck,YCheck) :- caseVide(X,YCheck), !.
calculPositionJeton(X,YCheck,Y) :- incr(YCheck, YCheck1), calculPositionJeton(X,YCheck1,Y).

