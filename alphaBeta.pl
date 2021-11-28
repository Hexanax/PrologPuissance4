%%%%%%%%%%%% miniMax.pl %%%%%%%%%%%%
% Implémentation de minimax avec diverses optimisations propres au Puissance 4.

:- module(alphaBeta, [alphaBeta/7]).

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- dynamic caseTest/3.

:- use_module(util).

couleurAdverse(jaune, rouge).
couleurAdverse(rouge, jaune).

alphaBeta(0,CouleurJoueur,Alpha,Beta , Move, Value,Maximizer):- 
    evaluate(CouleurJoueur, Value), !.

alphaBeta(Profondeur, CouleurJoueur, Alpha, Beta, Move, Value, Maximizer):- 
    Profondeur > 0,
    findall(X, (between(1,7,X),coupValide(X)), Moves),
    Profondeur1 is Profondeur -1,
    evaluate_and_choose(Moves, CouleurJoueur, Profondeur1, Alpha,Beta,nil,(Move,Value),Maximizer).

evaluate_and_choose([Move|Moves],CouleurJoueur,Profondeur,Alpha,Beta,Record,BestMove,Maximizer) :- 
	move(Move,CouleurJoueur,CouleurJoueurSuivant,Ligne),
    alphaBeta(D,CouleurJoueurSuivant,Alpha,Beta,MoveX,Value, Maximizer),
    undoMove(Move, Ligne, CouleurJoueur),
    cutoff(Move,Value,Profondeur,Alpha,Beta,Moves,CouleurJoueur,Record,BestMove, Maximizer).

evaluate_and_choose([],CouleurJoueur,Profondeur,Alpha,Beta,Move,(Move,Alpha),Maximizer).

cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Record,(Move,Value),Maximizer) :- 
    CouleurJoueur == Maximizer,
	Value >= Beta.
cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Record,(Move,Value),Maximizer) :- 
    couleurAdverse(CouleurJoueur,Maximizer),
	Value =< Alpha.

cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Record,BestMove,Maximizer) :- 
    CouleurJoueur == Maximizer,
    Value =< Alpha,
	evaluate_and_choose(Moves,CouleurJoueur,D,Alpha,Beta,Record,BestMove, Maximizer).
cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Record,BestMove,Maximizer) :- 
    CouleurJoueur == Maximizer,
    Value > Alpha,
	evaluate_and_choose(Moves,CouleurJoueur,D,Value,Beta,Move,BestMove, Maximizer).

cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Record,BestMove,Maximizer) :- 
    couleurAdverse(CouleurJoueur,Maximizer),
    Value >= Beta,
	evaluate_and_choose(Moves,CouleurJoueur,D,Alpha,Beta,Record,BestMove,Maximizer).
cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Record,BestMove,Maximizer) :- 
    couleurAdverse(CouleurJoueur,Maximizer),
    Value < Beta,
	evaluate_and_choose(Moves,CouleurJoueur,D,Alpha,Value,Move,BestMove,Maximizer).
    
move(Move, CouleurJoueur, CouleurJoueurSuivant,Ligne):-
    CouleurJoueur == jaune,
    CouleurJoueurSuivant = rouge,
    insererJeton(Move, Y, CouleurJoueur),
    Ligne is Y.

move(Move, CouleurJoueur, CouleurJoueurSuivant, Ligne):-
    CouleurJoueur == rouge,
    CouleurJoueurSuivant = jaune,
    insererJeton(Move, Y, CouleurJoueur),
    Ligne is Y.

undoMove(Move, Ligne, CouleurJoueur):-
    retract(caseTest(Move, Ligne, CouleurJoueur)).

evaluate(CouleurJoueur, Value):- 
    eval(CouleurJoueur, Score),
    Value is Score.

eval(CouleurJoueur, Score):- 
    dummyVictoire(CouleurJoueur, ScoreVictoire),
    random_between(-10, 10, Perturbation),
    Score is (ScoreVictoire + Perturbation).

%Forces the AI to play on the 2nd column because it gives a huge score to do so
forceColumnMove(CouleurJoueur, Score):-
    (caseTest(3, 6, _);
    caseTest(3, 5, _);
    caseTest(3, 4, _);
    caseTest(3, 3, _);
    caseTest(3, 2, _);
    caseTest(3, 1, _)),
    Score is 1000.

%Always a valid score of 0 if not true
forceColumnMove(_, 0).

dummyVictoire(CouleurJoueur, Score):-
    (caseTest(3, _, _)),
    Score is 1000.

dummyVictoire(_, 0).

%%%%% placerJeton %%%%%
% coupValide/1(-Colonne)
% Vérifie si un jeton est jouable dans cette colonne.
% Vrai si le coup est valide.
coupValide(X) :- nbColonnes(NBCOLONNES), X=<NBCOLONNES, X>=1, nbLignes(NBLIGNES), caseVideTest(X,NBLIGNES).

caseVideTest(X,Y) :- nonvar(X),nonvar(Y),not(caseTest(X,Y,_)).

% insererJeton/3(-Colonne, +Ligne, -Couleur)
% Insère, sans vérification, un jeton de la couleur donnée, dans la colonne donnée.
% Y s'unifie à la ligne d'insertion,
insererJeton(X,Y,C) :- calculPositionJeton(X, 1, Y), assert(caseTest(X,Y,C)).

% calculPositionJeton/3(+Colonne,+LigneToCheck,-Ligne)
% Calcule la première ligne vide d'une colonne.
% Ligne s'unfinie à l'indice de la première ligne vide de la colonne.
calculPositionJeton(X,YCheck,YCheck) :- caseVideTest(X,YCheck), !.
calculPositionJeton(X,YCheck,Y) :- incr(YCheck, YCheck1), calculPositionJeton(X,YCheck1,Y).

%%% Détection de la victoire des cases de test.

gagneTest(X,Y,J,V) :- %V=1 si victoire direct, 0 si indirect
	assert(caseTest(X,Y,J)),
	gagneColonneTest(X,Y,J,R1,A1),
	gagneLigneTest(X,Y,J,R2,P2,A2),
	gagneDiag1Test(X,Y,J,R3,P3,A3),
	gagneDiag2Test(X,Y,J,R4,P4,A4),
	Pf is P2+P3+P4,
	Af is A1+A2+A3+A4,
	testFinal(R1,R2,R3,R4,Pf,Af,V),
	retract(caseTest(X,Y,J)).

gagneTest(X,Y,J,0):-retract(caseTest(X,Y,J)), false. %ménage

testPotentielAccumulation(X,Y,J,P,A):-
	testPotentiel(X,Y,J,P), %Peut on la remplir au prochain coup?
	testAccumulation(X,Y,J,A). %As-t-on accumulation?

testPotentiel(_,1,_,1).	%case au niveau 1
testPotentiel(X,Y,_,1):-
	decr(Y,Y1),
	caseTest(X,Y1,_).  %On peut la remplir
testPotentiel(_,_,_,0). %On ne peut pas la remplir


testAccumulation(X,Y,J,1) :- incr(Y,Y1), caseTestValideVide(X,Y1), gagneTestDirect(X,Y1,J). %Case au dessus gagnante aussi
testAccumulation(X,Y,J,1) :- decr(Y,Y1), caseTestValideVide(X,Y1), gagneTestDirect(X,Y1,J). %Case en dessous gagnante aussi
testAccumulation(_,_,_,0). %Pas d'accumulation.

caseTestValideVide(X,Y):-
	nbColonnes(NBCOLONNES), X=<NBCOLONNES, X>=1,
	caseVideTest(X,Y). %Case vide

testFinal(R1,_,_,_,_,_,1):-
	R1 > 2.
testFinal(_,R2,_,_,_,_,1):-
	R2 > 2.
testFinal(_,_,R3,_,_,_,1):-
	R3 > 2.
testFinal(_,_,_,R4,_,_,1):-
	R4 > 2.
testFinal(_,_,_,_,P,_,0):-
	P>1.
testFinal(_,_,_,_,_,A,-5):-
	A >0.

%%%%% gagne %%%%%


%%% En colonne %%%

gagneColonneTest(X,Y,J,3,0) :-
	decr(Y,Y1),
	caseTest(X,Y1,J),
	decr(Y1,Y2),
	caseTest(X,Y2,J),
	decr(Y2,Y3),
	caseTest(X,Y3,J). %ligne en bas
gagneColonneTest(X,Y,J,0,1) :-
	decr(Y,Y1),
	caseTest(X,Y1,J),
	decr(Y1,Y2),
	caseTest(X,Y2,J),
	incr(Y,Ytemp),
	incr(Ytemp,Ydessus),
	gagneTestDirect(X,Ydessus,J).
gagneColonneTest(_,_,_,0,0).

%%% En ligne %%%

gagneLigneTest(X,Y,J,Rf,Pf,Af) :-
	decr(X,X1),
	gaucheTestVerif(X1,Y,J,Rg,Pg,Ag),
	incr(X,X2),
	droiteTestVerif(X2,Y,J,Rd,Pd,Ad),
	!,
	Rf is Rg+Rd, Pf is Pg+Pd, Af is Ag+Ad.

gaucheTestVerif(X,Y,J,Rg,Pg,Ag):-
	gaucheTest(X,Y,J,0,Rg,Pg,Ag).
gaucheTest(X,Y,J,R,R,Pg,Ag):-
	caseTestValideVide(X,Y),	%case dans le tableau et vide
	gagneTestDirectLigne(X,Y,J),	%gagnante
	testPotentielAccumulation(X,Y,J,Pg,Ag).		%Peut on la placer et a-t-on accumulation?
gaucheTest(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
gaucheTest(X,Y,J,R,Rg,Pg,Ag) :-
	decr(X,X1),
	incr(R,R1),
	gaucheTest(X1,Y,J,R1,Rg,Pg,Ag).

droiteTestVerif(X,Y,J,Rg,Pg,Ag):-
	droiteTest(X,Y,J,0,Rg,Pg,Ag).
droiteTest(X,Y,J,R,R,Pg,Ag):-
	caseTestValideVide(X,Y),	%case dans le tableau et vide
	gagneTestDirectLigne(X,Y,J),	%gagnante
	testPotentielAccumulation(X,Y,J,Pg,Ag).		%Peut on la placer et a-t-on accumulation?
droiteTest(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
droiteTest(X,Y,J,R,Rg,Pg,Ag) :-
	incr(X,X1),
	incr(R,R1),
	droiteTest(X1,Y,J,R1,Rg,Pg,Ag).

%%% En diagonale \ %%%

gagneDiag1Test(X,Y,J,Rf,Pf,Af) :-
	decr(X,X1),
	incr(Y,Y1),
	gaucheTestHautVerif(X1,Y1,J,Rg,Pg,Ag),
	incr(X,X2),
	decr(Y,Y2),
	droiteTestBasVerif(X2,Y2,J,Rd,Pd,Ad),
	!,
	Rf is Rg+Rd, Pf is Pg+Pd, Af is Ag+Ad.

gaucheTestHautVerif(X,Y,J,Rg,Pg,Ag):-
	gaucheTestHaut(X,Y,J,0,Rg,Pg,Ag).
gaucheTestHaut(X,Y,J,R,R,Pg,Ag):-
	caseTestValideVide(X,Y),	%case dans le tableau et vide
	gagneTestDirectDiag1(X,Y,J),	%gagnante
	testPotentielAccumulation(X,Y,J,Pg,Ag).		%Peut on la placer et a-t-on accumulation?
gaucheTestHaut(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
gaucheTestHaut(X,Y,J,R,Rg,Pg,Ag) :-
	incr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheTestHaut(X1,Y1,J,R1,Rg,Pg,Ag).

droiteTestBasVerif(X,Y,J,Rg,Pg,Ag):-
	droiteTestBas(X,Y,J,0,Rg,Pg,Ag).
droiteTestBas(X,Y,J,R,R,Pg,Ag):-
	caseTestValideVide(X,Y),	%case dans le tableau et vide
	gagneTestDirectDiag1(X,Y,J),	%gagnante
	testPotentielAccumulation(X,Y,J,Pg,Ag).		%Peut on la placer et a-t-on accumulation?
droiteTestBas(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
droiteTestBas(X,Y,J,R,Rg,Pg,Ag) :-
	decr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteTestBas(X1,Y1,J,R1,Rg,Pg,Ag).

%%% En diagonale / %%%

gagneDiag2Test(X,Y,J,Rf,Pf,Af) :-
	decr(X,X1),
	decr(Y,Y1),
	gaucheTestBasVerif(X1,Y1,J,Rg,Pg,Ag),
	incr(X,X2),
	incr(Y,Y2),
	droiteTestHautVerif(X2,Y2,J,Rd,Pd,Ad),
	!,
	Rf is Rg+Rd, Pf is Pg+Pd, Af is Ag+Ad.

gaucheTestBasVerif(X,Y,J,Rg,Pg,Ag) :-
	gaucheTestBas(X,Y,J,0,Rg,Pg,Ag).
gaucheTestBas(X,Y,J,R,R,Pg,Ag):-
	caseTestValideVide(X,Y),	%case dans le tableau et vide
	gagneTestDirectDiag2(X,Y,J),	%gagnante
	testPotentielAccumulation(X,Y,J,Pg,Ag).		%Peut on la placer et a-t-on accumulation?
gaucheTestBas(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
gaucheTestBas(X,Y,J,R,Rg,Pg,Ag) :-
	decr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheTestBas(X1,Y1,J,R1,Rg,Pg,Ag).

droiteTestHautVerif(X,Y,J,Rg,Pg,Ag) :-
	droiteTestHaut(X,Y,J,0,Rg,Pg,Ag).
droiteTestHaut(X,Y,J,R,R,Pg,Ag):-
	caseTestValideVide(X,Y),	%case dans le tableau et vide
	gagneTestDirectDiag2(X,Y,J),	%gagnante
	testPotentielAccumulation(X,Y,J,Pg,Ag).		%Peut on la placer et a-t-on accumulation?
droiteTestHaut(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
droiteTestHaut(X,Y,J,R,Rg,Pg,Ag) :-
	incr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteTestHaut(X1,Y1,J,R1,Rg,Pg,Ag).

%%%%% gagneTestDirect %%%%%


gagneTestDirect(X,Y,J) :-
	gagneTestDirectLigne(X,Y,J).
gagneTestDirect(X,Y,J) :-
	gagneTestDirectDiag1(X,Y,J).
gagneTestDirect(X,Y,J) :-
	gagneTestDirectDiag2(X,Y,J).
gagneTestDirect(X,Y,J) :-
    gagneTestDirectColonne(X,Y,J).

%%% En ligne %%%

gagneTestDirectLigne(X,Y,J) :-
	decr(X,X1),
	gaucheVerif(X1,Y,J,Rg),
	incr(X,X2),
	droiteVerif(X2,Y,J,Rd),
	!,
	Rf is Rg+Rd, Rf>2.

gaucheVerif(X,Y,J,Rg):-
	gauche(X,Y,J,0,Rg).
gauche(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gauche(X,Y,J,R,Rg) :-
	decr(X,X1),
	incr(R,R1),
	gauche(X1,Y,J,R1,Rg).

droiteVerif(X,Y,J,Rg):-
	droite(X,Y,J,0,Rg).
droite(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droite(X,Y,J,R,Rg) :-
	incr(X,X1),
	incr(R,R1),
	droite(X1,Y,J,R1,Rg).

%%% En diagonale \ %%%

gagneTestDirectDiag1(X,Y,J) :-
	decr(X,X1),
	incr(Y,Y1),
	gaucheHautVerif(X1,Y1,J,Rg),
	incr(X,X2),
	decr(Y,Y2),
	droiteBasVerif(X2,Y2,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>2.

gaucheHautVerif(X,Y,J,Rg):-
	gaucheHaut(X,Y,J,0,Rg).
gaucheHaut(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gaucheHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheHaut(X1,Y1,J,R1,Rg).

droiteBasVerif(X,Y,J,Rg):-
	droiteBas(X,Y,J,0,Rg).
droiteBas(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droiteBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteBas(X1,Y1,J,R1,Rg).

%%% En diagonale / %%%

gagneTestDirectDiag2(X,Y,J) :-
	decr(X,X1),
	decr(Y,Y1),
	gaucheBasVerif(X1,Y1,J,Rg),
	incr(X,X2),
	incr(Y,Y2),
	droiteHautVerif(X2,Y2,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>2.

gaucheBasVerif(X,Y,J,Rg) :-
	gaucheBas(X,Y,J,0,Rg).
gaucheBas(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gaucheBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheBas(X1,Y1,J,R1,Rg).

droiteHautVerif(X,Y,J,Rg) :-
	droiteHaut(X,Y,J,0,Rg).
droiteHaut(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droiteHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteHaut(X1,Y1,J,R1,Rg).

%%% En colonne %%%

gagneTestDirectColonne(X,Y,J) :-
    decr(Y,Y1),
    caseTest(X,Y1,J),
    decr(Y1,Y2),
    caseTest(X,Y2,J),
    decr(Y2,Y3),
    caseTest(X,Y3,J).