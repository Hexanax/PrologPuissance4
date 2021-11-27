%%%%%%%%%%%% miniMax.pl %%%%%%%%%%%%
% Implémentation de minimax avec diverses optimisations propres au Puissance 4.

:- module(alphaBeta, [alphaBeta/6]).

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- dynamic caseTest/3.

:- use_module(util).



alphaBeta(0,CouleurJoueur,Alpha,Beta , Move, Value):- 
    evaluate(CouleurJoueur, Value).

alphaBeta(Profondeur, CouleurJoueur, Alpha, Beta, Move, Value):- 
    Profondeur > 0,
    findall(X, (between(1,7,X),coupValide(X)), Moves),
    Alpha1 is -Beta,
    Beta1 is -Alpha,
    Profondeur1 is Profondeur -1,
    evaluate_and_choose(Moves, CouleurJoueur, Profondeur1, Alpha1,Beta1,nil,(Move,Value)).

alphaBeta(Profondeur, CouleurJoueur, Alpha,Beta,Move, Value):- 
    Profondeur > 0,
    alphaBeta(0, CouleurJoueur, Alpha,Beta,Move, Value).

evaluate_and_choose([Move|Moves],CouleurJoueur,D,Alpha,Beta,Move1,BestMove) :- 
	move(Move,CouleurJoueur,CouleurJoueurSuivant,Ligne),
    alphaBeta(D,CouleurJoueurSuivant,Alpha,Beta,MoveX,Value),
    undoMove(Move, Ligne, CouleurJoueur),
    Value1 is -Value,   
    cutoff(Move,Value1,D,Alpha,Beta,Moves,CouleurJoueur,Move1,BestMove).

evaluate_and_choose([],CouleurJoueur,D,Alpha,Beta,Move,(Move,A)).
 
 
cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Move1,(Move,Value)) :- 
	Value >= Beta.
cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Move1,BestMove) :- 
    Alpha < Value, 
    Value < Beta, !,
	evaluate_and_choose(Moves,CouleurJoueur,D,Value,Beta,Move,BestMove).
cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Move1,BestMove) :-
    Value =< Alpha, !,
	evaluate_and_choose(Moves,CouleurJoueur,D,Alpha,Beta,Move1,BestMove).
    
    
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
    random_between(-6,6,Perturbation),
    Score is Perturbation.

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