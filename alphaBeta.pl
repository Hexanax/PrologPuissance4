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
    evaluate(CouleurJoueur, Value),!.

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
	Value >= Beta, !.
cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Record,(Move,Value),Maximizer) :- 
    couleurAdverse(CouleurJoueur,Maximizer),
	Value =< Alpha, !.

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


