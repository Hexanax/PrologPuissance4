%%%%%%%%%%%% miniMax.pl %%%%%%%%%%%%
% Implémentation de minimax avec diverses optimisations propres au Puissance 4.

:- module(alphaBeta, [alphaBeta/7]).

%%%%%%%%%%%%%%%%
%% Inclusions %% https://github.com/PascalPons/connect4/blob/part4/solver.cpp
%%%%%%%%%%%%%%%%

:- dynamic caseTest/3.

:- use_module(util).
:- use_module(ia).

couleurAdverse(jaune, rouge).
couleurAdverse(rouge, jaune).

alphaBeta(0,CouleurJoueur,Alpha,Beta , Move, Value,Maximizer):- 
    evaluate(CouleurJoueur, Value),!.
alphaBeta(Profondeur, CouleurJoueur, Alpha, Beta, Move, Value, Maximizer):- 
    Profondeur > 0,
    findall(X, (between(1,7,X),coupValide(X)), Moves),
    taille(Moves,Size),
    Size > 0,
    Profondeur1 is Profondeur - 1,
    evaluate_and_choose(Moves, CouleurJoueur,Profondeur1,Alpha,Beta,nil, (Move,Value),Maximizer).
alphaBeta(Profondeur, CouleurJoueur, Alpha, Beta, Move, Value, Maximizer):- 
    Profondeur > 0,
    findall(X, (between(1,7,X),coupValide(X)), Moves),
    taille(Moves,0),
    alphaBeta(0,CouleurJoueur,Alpha,Beta , Move, Value,Maximizer).
taille([],0).

taille([H|T],L) :-
	taille(T,L1),
	L is 1 + L1.

evaluate_and_choose([Move|Moves],CouleurJoueur,Profondeur,Alpha,Beta,Record,BestMove,Maximizer) :- 
	move(Move,CouleurJoueur,CouleurJoueurSuivant,Ligne),
    Alpha1 is -Beta,
    Beta1 is -Alpha,
    alphaBeta(Profondeur,CouleurJoueurSuivant,Alpha1,Beta1,MoveX,Value, Maximizer),
    undoMove(Move, Ligne, CouleurJoueur),
    Value1 is Value,  % ici je sais pas si il faut faire fois -1 ou pas je te laisse test
    cutoff(Move,Value1,Profondeur,Alpha,Beta,Moves,CouleurJoueur,Record,BestMove, Maximizer).

evaluate_and_choose([],CouleurJoueur,Profondeur,Alpha,Beta,Move,(Move,Alpha),Maximizer).

cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Record,(Move,Value),Maximizer) :- 
    Value >= Beta.

cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Record,BestMove,Maximizer) :- 
    Alpha < Value,
	Value < Beta,
    evaluate_and_choose(Moves,CouleurJoueur,D,Value,Beta,Move,BestMove, Maximizer).

cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Record,BestMove,Maximizer) :- 
    Value =< Alpha,
    evaluate_and_choose(Moves,CouleurJoueur,D,Value,Beta,Record,BestMove, Maximizer).
    
    
move(Move, CouleurJoueur, CouleurJoueurSuivant,Ligne):-
    CouleurJoueur == jaune,
    CouleurJoueurSuivant = rouge,
    insererJeton(Move, Y, CouleurJoueur),
    Ligne is Y,
    !.

move(Move, CouleurJoueur, CouleurJoueurSuivant, Ligne):-
    CouleurJoueur == rouge,
    CouleurJoueurSuivant = jaune,
    insererJeton(Move, Y, CouleurJoueur),
    Ligne is Y,
    !.

undoMove(Move, Ligne, CouleurJoueur):-
    retract(caseTest(Move, Ligne, CouleurJoueur)).

evaluate(CouleurJoueur, Value):- 
    eval(CouleurJoueur, Score),
    Value is Score.

eval(CouleurJoueur, Score):- 
    %%%%%% Call heuristics %%%%%%
    poidsCaseTableau(PoidsCaseTableau),
    poidsDefensif(PoidsDefensif),
    poidsCaseOffensif(PoidsOffensif),
    poidsPiegeSept(PoidsPiege),
    poidsOpening(PoidsOpening),
    defensiveIA(CouleurJoueur, ScoreDefensif, PoidsDefensif),
    offensiveIA(CouleurJoueur, ScoreOffensif, PoidsOffensif),
    positionIA(CouleurJoueur, ScorePosition, PoidsCaseTableau),
    piege7IA(CouleurJoueur, ScorePiege, PoidsPiege),
    opening(CouleurJoueur, ScoreOpening, PoidsOpening),
    forceColumnMove(CouleurJoueur, ScoreVictoire),
    random_between(-4, 4, Perturbation),
    Score is ScoreDefensif * PoidsDefensif
            + ScorePosition * PoidsCaseTableau
            + ScoreOffensif * PoidsOffensif
            + ScorePiege * PoidsPiege
            + ScoreOpening * PoidsOpening
            + Perturbation.

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
    (caseTest(3, 1, _)),
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

%%%%%%%%%%% Commentaire heuristique de défense








%%%%%%%%%%%

defensiveIA(CouleurJoueur, ScoreDefensif, PoidsDefensif):- 
    PoidsDefensif > 0,
    findall(S, evalDangerAdverse(CouleurJoueur, S),Scores),
    sum(Scores, ScoreDefensifTot),
    ScoreDefensif is ScoreDefensifTot.
defensiveIA(_, 0, _).

offensiveIA(CouleurJoueur, ScoreOffensif, PoidsOffensif):- 
    PoidsOffensif > 0,
    findall(S, evalDangerJoueur(CouleurJoueur, S),Scores),
    sum(Scores, ScoreOffensiffTot),
    ScoreOffensif is ScoreOffensiffTot.
offensiveIA(_, 0, _).

evalDangerAdverse(CouleurJoueur, Score) :-
    couleurAdverse(CouleurJoueur, JoueurAdverse),
    caseTest(X,Y,JoueurAdverse),
    calculerScoreAlignement(X, Y, JoueurAdverse, S),
    Score is S * -1.

evalDangerJoueur(CouleurJoueur, Score) :-
    caseTest(X,Y,CouleurJoueur),
    calculerScoreAlignement(X, Y, JoueurAdverse, S),
    Score is S.

calculerScoreAlignement(X,Y, CouleurJoueur, Score ):-
    evaluerLigne(X,Y,CouleurJoueur,LigneGauche1, LigneGauche2, LigneGauche3, LigneDroite1, LigneDroite2, LigneDroite3),
    evaluerColonne(X,Y,CouleurJoueur,ColonneGauche1, ColonneGauche2, ColonneGauche3, ColonneDroite1, ColonneDroite2, ColonneDroite3),
    evaluerDiag1(X,Y,CouleurJoueur,Diag1Gauche1, Diag1Gauche2, Diag1Gauche3, Diag1Droite1, Diag1Droite2, Diag1Droite3),
    evaluerDiag2(X,Y,CouleurJoueur,Diag2Gauche1, Diag2Gauche2, Diag2Gauche3, Diag2Droite1, Diag2Droite2, Diag2Droite3),
    align4Pions(LigneGauche1, LigneGauche2, LigneGauche3, LigneDroite1, LigneDroite2, LigneDroite3, LigneScore4),
    align4Pions(ColonneGauche1, ColonneGauche2, ColonneGauche3, ColonneDroite1, ColonneDroite2, ColonneDroite3, ColonneScore4),
    align4Pions(Diag1Gauche1, Diag1Gauche2, Diag1Gauche3, Diag1Droite1, Diag1Droite2, Diag1Droite3, Diag1Score4),
    align4Pions(Diag2Gauche1, Diag2Gauche2, Diag2Gauche3, Diag2Droite1, Diag2Droite2, Diag2Droite3, Diag2Score4),
    findall(S, align3Pions(LigneGauche1, LigneGauche2, LigneGauche3, LigneDroite1, LigneDroite2, LigneDroite3, S),ListLigneScore3),
    sum(ListLigneScore3, LigneScore3),
    findall(S, align3Pions(ColonneGauche1, ColonneGauche2, ColonneGauche3, ColonneDroite1, ColonneDroite2, ColonneDroite3, S),ListColonneScore3),
    sum(ListColonneScore3, ColonneScore3),
    findall(S, align3Pions(Diag1Gauche1, Diag1Gauche2, Diag1Gauche3, Diag1Droite1, Diag1Droite2, Diag1Droite3, S),ListDiag1Score3),
    sum(ListDiag1Score3, Diag1Score3),
    findall(S, align3Pions(Diag2Gauche1, Diag2Gauche2, Diag2Gauche3, Diag2Droite1, Diag2Droite2, Diag2Droite3, S),ListDiag2Score3),
    sum(ListDiag2Score3, Diag2Score3),
    findall(S, align2Pions(LigneGauche1, LigneGauche2, LigneGauche3, LigneDroite1, LigneDroite2, LigneDroite3, S),ListLigneScore2),
    sum(ListLigneScore2, LigneScore2),
    findall(S, align2Pions(ColonneGauche1, ColonneGauche2, ColonneGauche3, ColonneDroite1, ColonneDroite2, ColonneDroite3, S),ListColonneScore2),
    sum(ListColonneScore2, ColonneScore2),
    findall(S, align2Pions(Diag1Gauche1, Diag1Gauche2, Diag1Gauche3, Diag1Droite1, Diag1Droite2, Diag1Droite3, S),ListDiag1Score2),
    sum(ListDiag1Score2, Diag1Score2),
    findall(S, align2Pions(Diag2Gauche1, Diag2Gauche2, Diag2Gauche3, Diag2Droite1, Diag2Droite2, Diag2Droite3, S),ListDiag2Score2),
    sum(ListDiag2Score2, Diag2Score2),
    Score is LigneScore4 + ColonneScore4 + Diag1Score4 + Diag2Score4 
            + LigneScore3 + ColonneScore3 + Diag1Score3 + Diag2Score3
            + LigneScore2 + ColonneScore2 + Diag1Score2 + Diag2Score2.


%x,x,x,x%
align4Pions(Gauche1, Gauche2, Gauche3, Droite1, Droite2, Droite3, Score):-
    (  
        1+Droite1+Droite2+Droite3 >= 4, Score is 1000,!;
        1+Gauche1+Droite1+Droite2 >= 4,Score is 1000,!;
        1+Gauche1+Gauche2+Droite1 >= 4,Score is 1000,!;
        1+Gauche1+Gauche2+Gauche3 >= 4,Score is 1000,!);
    (Score is 0).

%_,x,x,x%
align3Pions(Gauche1, Gauche2, Gauche3, Droite1, Droite2, _, Score):-
    (  
        Gauche1==0, Droite1==1, Droite2==1, Score is 100;
        Gauche1==1, Gauche2==0, Droite1==1, Score is 100;
        Gauche1==1, Gauche2==1, Gauche3==0, Score is 100);
    (Score is 0).

%x,_,x,x%
align3Pions(Gauche1, Gauche2, Gauche3, Droite1, Droite2, Droite3, Score):-
    (  
        Droite1==0, Droite2==1, Droite3==1, Score is 100;
        Gauche1==0, Gauche2==1, Droite1==1, Score is 100;
        Gauche1==1, Gauche2==0, Gauche3==1, Score is 100);
    (Score is 0).    

%x,x,_,x%
align3Pions(Gauche1, Gauche2, Gauche3, Droite1, Droite2, Droite3, Score):-
    (  
        Droite1==1, Droite2==0, Droite3==1, Score is 100;
        Gauche1==1, Droite1==0, Droite2==1, Score is 100;
        Gauche1==0, Gauche2==1, Gauche3==1, Score is 100);
    (Score is 0).

%x,x,x,_%
align3Pions(Gauche1, Gauche2, Gauche3, Droite1, Droite2, Droite3, Score):-
    (  
        Droite1==1, Droite2==1, Droite3==0, Score is 100;
        Gauche1==1, Droite1==1, Droite2==0, Score is 100;
        Gauche1==1, Gauche2==1, Droite1==0, Score is 100);
    (Score is 0).    

%_,_,x,x%
align2Pions(Gauche1, Gauche2, Gauche3, Droite1, _, _, Score):-
    (  
        Gauche1==0, Gauche2==0, Droite1==1, Score is 10;
        Gauche1==1, Gauche2==0, Gauche3==0, Score is 10);
    (Score is 0).    
  
%_,x,_,x%
align2Pions(Gauche1, Gauche2, Gauche3, Droite1, Droite2, _, Score):-
    (  
        Gauche1==0, Droite1==0, Droite2==1, Score is 10;
        Gauche1==0, Gauche2==1, Gauche3==0, Score is 10);
    (Score is 0).    

%_,x,x,_%
align2Pions(Gauche1, Gauche2, _, Droite1, Droite2, _, Score):-
    (  
        Gauche1==0, Droite1==1, Droite2==0, Score is 10;
        Gauche1==1, Gauche2==0, Droite1==0, Score is 10);
    (Score is 0).
 
%x,_,_,x%
align2Pions(Gauche1, Gauche2, Gauche3, Droite1, Droite2, Droite3, Score):-
    (  
        Droite1==0, Droite2==0, Droite3==1, Score is 10;
        Gauche1==0, Gauche2==0, Gauche3==1, Score is 10);
    (Score is 0).

%x,_,x,_%
align2Pions(Gauche1, Gauche2, _, Droite1, Droite2, Droite3, Score):-
    (  
        Droite1==0, Droite2==1, Droite3==0, Score is 10;
        Gauche1==0, Gauche2==1, Droite1==0, Score is 10);
    (Score is 0).  
   
%x,x,_,_%
align2Pions(Gauche1, _, _, Droite1, Droite2, Droite3, Score):-
    (  
        Droite1==1, Droite2==0, Droite3==0, Score is 10;
        Gauche1==1, Droite1==0, Droite2==0, Score is 10);
    (Score is 0).  
  
evaluerLigne(X,Y,Joueur,Gauche1,Gauche2,Gauche3,Droite1, Droite2, Droite3) :-
    verifierGaucheLigne(X,Y,Joueur,Gauche1,Gauche2,Gauche3),
    verifierDroiteLigne(X,Y,Joueur,Droite1,Droite2,Droite3).

verifierDroiteLigne(X,Y,Joueur,Droite1,Droite2,Droite3):-
    incr(X,X1),
    incr(X1,X2),
    incr(X2,X3), 
    (X1 =< 7, caseTest(X1,Y,Joueur), Droite1 is 1,!;
     X1 =< 7, not(caseTest(X1,Y,_)), Droite1 is 0,!;
     Droite1 is -1),
    (X2 =< 7, caseTest(X2,Y,Joueur), Droite2 is 1,!;
     X2 =< 7, not(caseTest(X2,Y,_)), Droite2 is 0,!;
     Droite2 is -1),
    (X3 =< 7, caseTest(X3,Y,Joueur), Droite3 is 1,!;
     X3 =< 7, not(caseTest(X3,Y,_)), Droite3 is 0,!;
     Droite3 is -1).

verifierGaucheLigne(X,Y,Joueur,Gauche1,Gauche2,Gauche3):-
    decr(X,X1),
    decr(X1,X2),
    decr(X2,X3), 
    (X1 >=1 , caseTest(X1,Y,Joueur), Gauche1 is 1,!;
     X1 >=1 , not(caseTest(X1,Y,_)), Gauche1 is 0,!;
     Gauche1 is -1),
    (X2 >=1 , caseTest(X2,Y,Joueur), Gauche2 is 1,!;
     X2 >=1 , not(caseTest(X2,Y,_)), Gauche2 is 0,!;
     Gauche2 is -1),
    (X3 >=1 , caseTest(X3,Y,Joueur), Gauche3 is 1,!;
     X3 >=1 , not(caseTest(X3,Y,_)), Gauche3 is 0,!;
     Gauche3 is -1).

evaluerColonne(X,Y,Joueur,Gauche1,Gauche2,Gauche3,Droite1, Droite2, Droite3) :-
    verifierGaucheColonne(X,Y,Joueur,Gauche1,Gauche2,Gauche3),
    verifierDroiteColonne(X,Y,Joueur,Droite1,Droite2,Droite3).

verifierDroiteColonne(X,Y,Joueur,Droite1,Droite2,Droite3):-
    incr(Y,Y1),
    incr(Y1,Y2),
    incr(Y2,Y3), 
    (Y1 =< 6, caseTest(X,Y1,Joueur), Droite1 is 1,!;
     Y1 =< 6, not(caseTest(X,Y1,_)), Droite1 is 0,!;
     Droite1 is -1),
    (Y2 =< 6, caseTest(X,Y2,Joueur), Droite2 is 1,!;
     Y2 =< 6, not(caseTest(X,Y2,_)), Droite2 is 0,!;
     Droite2 is -1),
    (Y3 =< 6, caseTest(X,Y3,Joueur), Droite3 is 1,!;
     Y3 =< 6, not(caseTest(X,Y3,_)), Droite3 is 0,!;
     Droite3 is -1).

verifierGaucheColonne(X,Y,Joueur,Gauche1,Gauche2,Gauche3):-
    decr(Y,Y1),
    decr(Y1,Y2),
    decr(Y2,Y3), 
    (Y1 >=1 , caseTest(X,Y1,Joueur), Gauche1 is 1,!;
     Y1 >=1 , not(caseTest(X,Y1,_)), Gauche1 is 0,!;
     Gauche1 is -1),
    (Y2 >=1 , caseTest(X,Y2,Joueur), Gauche2 is 1,!;
     Y2 >=1 , not(caseTest(X,Y2,_)), Gauche2 is 0,!;
     Gauche2 is -1),
    (Y3 >=1 , caseTest(X,Y3,Joueur), Gauche3 is 1,!;
     Y3 >=1 , not(caseTest(X,Y3,_)), Gauche3 is 0,!;
     Gauche3 is -1).

evaluerDiag1(X,Y,Joueur,Gauche1,Gauche2,Gauche3,Droite1, Droite2, Droite3) :-
    verifierGaucheColonne(X,Y,Joueur,Gauche1,Gauche2,Gauche3),
    verifierDroiteColonne(X,Y,Joueur,Droite1,Droite2,Droite3).

verifierDroiteDiag1(X,Y,Joueur,Droite1,Droite2,Droite3):-
    incr(Y,Y1),
    incr(Y1,Y2),
    incr(Y2,Y3), 
    incr(X,X1),
    incr(X1,X2),
    incr(X2,X3), 
    (Y1 =< 6, X1 =< 7, caseTest(X1,Y1,Joueur), Droite1 is 1,!;
     Y1 =< 6, X1 =< 7, not(caseTest(X1,Y1,_)), Droite1 is 0,!;
     Droite1 is -1),
    (Y2 =< 6, X2 =< 7, caseTest(X2,Y2,Joueur), Droite2 is 1,!;
     Y2 =< 6, X2 =< 7, not(caseTest(X2,Y2,_)), Droite2 is 0,!;
     Droite2 is -1),
    (Y3 =< 6, X3 =< 7, caseTest(X3,Y3,Joueur), Droite3 is 1,!;
     Y3 =< 6, X3 =< 7, not(caseTest(X3,Y3,_)), Droite3 is 0,!;
     Droite3 is -1).

verifierGaucheDiag1(X,Y,Joueur,Gauche1,Gauche2,Gauche3):-
    decr(Y,Y1),
    decr(Y1,Y2),
    decr(Y2,Y3), 
    decr(X,X1),
    decr(X1,X2),
    decr(X2,X3), 
    (Y1 >=1 ,X1 >= 1, caseTest(X1,Y1,Joueur), Gauche1 is 1,!;
     Y1 >=1 ,X1 >= 1, not(caseTest(X1,Y1,_)), Gauche1 is 0,!;
     Gauche1 is -1),
    (Y2 >=1 ,X2 >= 1, caseTest(X2,Y2,Joueur), Gauche2 is 1,!;
     Y2 >=1 ,X2 >= 1, not(caseTest(X2,Y2,_)), Gauche2 is 0,!;
     Gauche2 is -1),
    (Y3 >=1 ,X3 >= 1, caseTest(X3,Y3,Joueur), Gauche3 is 1,!;
     Y3 >=1 ,X3 >= 1, not(caseTest(X3,Y3,_)), Gauche3 is 0,!;
     Gauche3 is -1).

evaluerDiag2(X,Y,Joueur,Gauche1,Gauche2,Gauche3,Droite1, Droite2, Droite3) :-
    verifierGaucheColonne(X,Y,Joueur,Gauche1,Gauche2,Gauche3),
    verifierDroiteColonne(X,Y,Joueur,Droite1,Droite2,Droite3).

verifierDroiteDiag2(X,Y,Joueur,Droite1,Droite2,Droite3):-
    decr(Y,Y1),
    decr(Y1,Y2),
    decr(Y2,Y3), 
    incr(X,X1),
    incr(X1,X2),
    incr(X2,X3), 
    (Y1 >=1 6, X1 =< 7, caseTest(X1,Y1,Joueur), Droite1 is 1,!;
     Y1 >=1 6, X1 =< 7, not(caseTest(X1,Y1,_)), Droite1 is 0,!;
     Droite1 is -1),
    (Y2 >=1 6, X2 =< 7, caseTest(X2,Y2,Joueur), Droite2 is 1,!;
     Y2 >=1 6, X2 =< 7, not(caseTest(X2,Y2,_)), Droite2 is 0,!;
     Droite2 is -1),
    (Y3 >=1 6, X3 =< 7, caseTest(X3,Y3,Joueur), Droite3 is 1,!;
     Y3 >=1 6, X3 =< 7, not(caseTest(X3,Y3,_)), Droite3 is 0,!;
     Droite3 is -1).

verifierGaucheDiag2(X,Y,Joueur,Gauche1,Gauche2,Gauche3):-
    incr(Y,Y1),
    incr(Y1,Y2),
    incr(Y2,Y3), 
    decr(X,X1),
    decr(X1,X2),
    decr(X2,X3), 
    (Y1 =< 6, X1 >= 1, caseTest(X1,Y1,Joueur), Droite1 is 1,!;
     Y1 =< 6, X1 >= 1, not(caseTest(X1,Y1,_)), Droite1 is 0,!;
     Droite1 is -1),
    (Y2 =< 6, X2 >= 1, caseTest(X2,Y2,Joueur), Droite2 is 1,!;
     Y2 =< 6, X2 >= 1, not(caseTest(X2,Y2,_)), Droite2 is 0,!;
     Droite2 is -1),
    (Y3 =< 6, X3 >= 1, caseTest(X3,Y3,Joueur), Droite3 is 1,!;
     Y3 =< 6, X3 >= 1, not(caseTest(X3,Y3,_)), Droite3 is 0,!;
     Droite3 is -1).


%%%%% Heuristic d'état du plateau %%%%%%%%%
/*


3	4	5	7	5	4	3
4	6	8	10	8	6	4
5	8	11	13	11	8	5
5	8	11	13	11	8	5
4	6	8	10	8	6	4
3	4	5	7	5	4	3


*/

positionIA(CouleurJoueur, ScorePosition, PoidsCaseTableau):- 
    PoidsCaseTableau > 0,
    findall(S, calculerScorePlateau(CouleurJoueur, S),Scores),
    sum(Scores, ScoreDefensifTotJoueur),
    couleurAdverse(CouleurJoueur, JoueurAdverse),
    findall(S, calculerScorePlateau(JoueurAdverse,S), ScoresAdverse), 
    sum(ScoresAdverse, ScoreDefensifTotAdverse),
    ScorePosition is ScoreDefensifTotJoueur - ScoreDefensifTotAdverse.
positionIA(_, 0, _).


calculerScorePlateau(CouleurJoueur, Score):-
    caseTest(X,Y, CouleurJoueur),
    valeurCasePlateau(X,Y,S),
    Score is S.

valeurCasePlateau(1,1,3).
valeurCasePlateau(2,1,4). 
valeurCasePlateau(3,1,5). 
valeurCasePlateau(4,1,7). 
valeurCasePlateau(5,1,5). 
valeurCasePlateau(6,1,4). 
valeurCasePlateau(7,1,3). 

valeurCasePlateau(1,2,4).
valeurCasePlateau(2,2,6). 
valeurCasePlateau(3,2,8). 
valeurCasePlateau(4,2,10). 
valeurCasePlateau(5,2,8). 
valeurCasePlateau(6,2,6). 
valeurCasePlateau(7,2,4). 

valeurCasePlateau(1,3,5).
valeurCasePlateau(2,3,8). 
valeurCasePlateau(3,3,11). 
valeurCasePlateau(4,3,13). 
valeurCasePlateau(5,3,11). 
valeurCasePlateau(6,3,8). 
valeurCasePlateau(7,3,5). 

valeurCasePlateau(1,4,5).
valeurCasePlateau(2,4,8). 
valeurCasePlateau(3,4,11). 
valeurCasePlateau(4,4,13). 
valeurCasePlateau(5,4,11). 
valeurCasePlateau(6,4,8). 
valeurCasePlateau(7,4,5). 

valeurCasePlateau(1,5,4).
valeurCasePlateau(2,5,6). 
valeurCasePlateau(3,5,8). 
valeurCasePlateau(4,5,10). 
valeurCasePlateau(5,5,8). 
valeurCasePlateau(6,5,6). 
valeurCasePlateau(7,5,4). 

valeurCasePlateau(1,6,3).
valeurCasePlateau(2,6,4). 
valeurCasePlateau(3,6,5). 
valeurCasePlateau(4,6,7). 
valeurCasePlateau(5,6,5). 
valeurCasePlateau(6,6,4). 
valeurCasePlateau(7,6,3). 


/*
Heuristic du tricks du 7 ***
                           *
                          * 
*/


piege7IA(CouleurJoueur, ScorePiege, PoidsPiege):-
    PoidsPiege > 0,
    findall(S, evalPiege(CouleurJoueur, S),Scores),
    sum(Scores, ScorePiegeTot),
    ScorePiege is ScorePiegeTot.
piege7IA(_, 0, _).    

evalPiege(CouleurJoueur, Score) :-
    caseTest(X,Y,CouleurJoueur),
    calculerScorePiege(X, Y, CouleurJoueur, S),
    Score is S * -1.

calculerScorePiege(X, Y, CouleurJoueur, Score) :- 
    evaluerLigne(X,Y,CouleurJoueur,LigneGauche1, LigneGauche2, _, LigneDroite1, LigneDroite2, _),
    evaluerColonne(X,Y,CouleurJoueur,ColonneGauche1, ColonneGauche2, _, ColonneDroite1, ColonneDroite2, _),
    evaluerDiag1(X,Y,CouleurJoueur,Diag1Gauche1, Diag1Gauche2, _, Diag1Droite1, Diag1Droite2, _),
    evaluerDiag2(X,Y,CouleurJoueur,Diag2Gauche1, Diag2Gauche2, _, Diag2Droite1, Diag2Droite2, _),
    SeptBon1 = 1 + LigneGauche1 + LigneGauche2 + Diag1Gauche1 + Diag1Gauche2,
    piege7BonPositionnement(SeptBon1, ScoreBon1),
    SeptBon2 = 1 + LigneDroite1 + LigneDroite2 + Diag2Droite1 + Diag2Droite2,
    piege7BonPositionnement(SeptBon2, ScoreBon2),
    SeptBas1 = 1 + LigneDroite1 + LigneDroite2 + Diag1Droite1 + Diag1Droite2,
    piege7BasPositionnement(SeptBas1, ScoreBas1),
    SeptBas2 = 1 + LigneGauche1 + LigneGauche2 + Diag2Gauche1 + Diag2Gauche2,
    piege7BasPositionnement(SeptBas2, ScoreBas2),
    SeptMoyen1 = 1 + ColonneDroite1 + ColonneDroite2 + Diag2Gauche1 + Diag1Gauche2,
    piege7MoyenPositionnement(SeptMoyen1, ScoreMoyen1),
    SeptMoyen2 = 1 + ColonneDroite1 + ColonneDroite2 + Diag1Droite1 + Diag1Droite2,
    piege7MoyenPositionnement(SeptMoyen2, ScoreMoyen2),
    SeptMoyen3 = 1 + ColonneGauche1 + ColonneGauche2 + Diag2Droite1 + Diag2Droite2,
    piege7MoyenPositionnement(SeptMoyen3, ScoreMoyen3),
    SeptMoyen4 = 1 + ColonneGauche1 + ColonneGauche2 + Diag1Gauche1 + Diag1Gauche2,
    piege7MoyenPositionnement(SeptMoyen4, ScoreMoyen4),
    Score is ScoreBon1 + ScoreBon2 + ScoreBas1 
            + ScoreBas2 + ScoreMoyen1 + ScoreMoyen2
            + ScoreMoyen3 + ScoreMoyen4.


piege7BonPositionnement(5, 250).
piege7BonPositionnement(4,125).   
piege7BonPositionnement(_, 0). 
piege7MoyenPositionnement(5,150).
piege7MoyenPositionnement(4,75).
piege7MoyenPositionnement(_,0).
piege7BasPositionnement(5,100).
piege7BasPositionnement(4,50).
piege7BasPositionnement(_,0).


/* Evaluation de l'opening 



*/

opening(CouleurJoueur, ScoreOpening, PoidsOpening) :- 
    PoidsOpening > 0,
    couleurAdverse(CouleurJoueur, JoueurAdverse),
    findall(Nb, compterCaseOccuper(CouleurJoueur,Nb), SommeJ), 
    findall(NbAdverse, compterCaseOccuper(CouleurJoueur,NbAdverse), SommeA), 
    sum(SommeA, SommeAdverse),
    sum(SommeJ, SommeJoueur),
    evaluerOpening(SommeJoueur, CouleurJoueur, SommeAdverse, JoueurAdverse, S),
    ScoreOpening is S.
opening(_, 0, _).    

evaluerOpening(1, CouleurJoueur, 0, _, Score):- 
    caseTest(X,_,CouleurJoueur),
    (X == 1, Score is -1000, !;
     X == 2, Score is -1000, !;
     X == 3, Score is 0, !;
     X == 4, Score is 1000, !;
     X == 5, Score is 0, !;
     X == 6, Score is -1000, !;
     X == 7, Score is -1000, !;   
        Score is 0).
evaluerOpening(1, CouleurJoueur, 1, _, Score):- 
    caseTest(X,_,CouleurJoueur),
    (X == 1, Score is -1000, !;
     X == 2, Score is -1000, !;
     X == 3, Score is 500, !;
     X == 4, Score is 1000, !;
     X == 5, Score is 500, !;
     X == 6, Score is -1000, !;
     X == 7, Score is -1000, !;   
        Score is 0). 
evaluerOpening(_, _, _, _,0).

compterCaseOccuper(CouleurJoueur, Somme):-
    caseTest(X,Y,CouleurJoueur),
    Somme is 1.