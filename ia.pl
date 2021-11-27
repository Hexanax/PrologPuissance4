%%%%%%%%%%%% ia.pl %%%%%%%%%%%%
% Deux "moteurs" d'IA :
% - "Aléatoire" jouant aléatoirement ;
% - "Minimax", implémentation de minimax assez paramétrable.

:- module(ia, [iaAleatoire/1
			  ,iaMinimax/7
			  ,iaAlphaBeta/3
			  ,poidsPuissance3/1
			  ,poidsPosition/1
			  ,poidsDensite/1
			  ,poidsAdjacence/1]
).

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(jeu).
:- use_module(util).
:- use_module(miniMax).
:- use_module(alphaBeta).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats dynamiques %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic poidsPuissance3/1.
:- dynamic poidsPosition/1.
:- dynamic poidsDensite/1.
:- dynamic poidsAdjacence/1.

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

iaAleatoire(Coup) :-
	nbColonnes(NBCOLONNES),
	Coup is random(NBCOLONNES)+1,
	coupValide(Coup).
% AI Aléatoire a choisi une colonne pleine, donc on la fait recommencer.
iaAleatoire(Coup) :-
	iaAleatoire(Coup).

iaMinimax(JoueurCourant,Coup,Profondeur,PoidsPosition,PoidsPuissance3,PoidsDensite,PoidsAdjacence) :-
	assert(poidsPosition(PoidsPosition)),
	assert(poidsPuissance3(PoidsPuissance3)),
	assert(poidsDensite(PoidsDensite)),
	assert(poidsAdjacence(PoidsAdjacence)),
	parcoursArbre(JoueurCourant,Profondeur,Coup,_).

iaAlphaBeta(JoueurCourant,Coup,Profondeur) :-
	Alpha is -99999,
	Beta is 99999,
	alphaBeta(Profondeur, JoueurCourant, Alpha, Beta, Coup, _, JoueurCourant).