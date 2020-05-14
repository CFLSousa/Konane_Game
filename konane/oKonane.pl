% -------------------------------------------------------
%
%				KONANE
%
% -------------------------------------------------------

:- use_module(library(lists)).


% --------------------------------------
% o adversario
% --------------------------------------

adversario(pretas,brancas).
adversario(brancas,pretas).


% --------------------------------------
% O primeiro jogador e' o jogador preto
% --------------------------------------

primeiro_jogador(pretas).


% --------------------------------------
% dimensao do tabuleiro quadrado
% --------------------------------------

dim(6).


% -------------------------------------------------------
%
%  			EStado do Jogo
%
% -------------------------------------------------------

% tabuleiro(QuemJoga,JogadaNum,PecasBrancas,PecasPretas)


% -------------------------------------------------------
%  			Tabuleiro inicial
% -------------------------------------------------------

estado_inicial(tab(QuemAbre,0,PecasBrancas,PecasPretas)) :-
	dim(Dim),
	Meio is Dim // 2,
	MeioSeg is Meio + 1,
	primeiro_jogador(QuemAbre),
	seq(1,Dim,Seq),
	findall((L,C),(member(L,Seq),member(C,Seq),cor(L,C,branca),\+ livre(MeioSeg,Meio,L,C)),PecasBrancas),
	findall((L,C),(member(L,Seq),member(C,Seq),cor(L,C,preta),\+ livre(Meio,MeioSeg,L,C)),PecasPretas).
	

% Dadas as coordenadas verifica se a peca e' preta ou branca

cor(L,C,preta) :-
	(L + C) mod 2 =:= 0.
cor(L,C,branca) :-
	(L + C) mod 2 =:= 1.


% casas livres

livre(Meio,MeioSeg,Meio,MeioSeg).
livre(_,MeioSeg,MeioSeg,MeioSeg).	


% Devolve uma lista que e' uma serie de Inic a Fim

seq(Inic,Fim,Seq) :-
	seq(Inic,Fim,[],Seq).
seq(Inic,_,[Inic|R],[Inic|R]) :- !.
seq(Inic,Corr,SeqR,Seq) :-
	NCorr is Corr - 1,
	seq(Inic,NCorr,[Corr|SeqR],Seq).


% -----------------------------------------------------------

%		Tabuleiros Finais

% -----------------------------------------------------------

final(tab(Prox,Num,Brancas,Pretas), Outro) :-
	Num > 2,
	\+ jogada(tab(Prox,Num,Brancas,Pretas),_,_),
	adversario(Prox,Outro).


% ----------------------------------------------------------------------------------------
%
%		JOGADA
%
% ----------------------------------------------------------------------------------------

% Faz uma jogada: come(Peca,direccao,NumSaltos)
% Por exemplo come((3,4),leste,2) quer dizer que a peca em (3,4) se move para leste
% comendo duas pecas.

jogada(tab(Prox,N,TabBrancas,TabPretas),tab(Outro,NN,NTabBrancas,NTabPretas),Come) :-
	come(Prox,TabBrancas,TabPretas,NTabBrancas,NTabPretas,Come),
	adversario(Prox,Outro),
	NN is N + 1.

% ------------------------------------
% Come uma peca ou sequencia de pecas
% ------------------------------------

% Come uma peca branca e continua a comer (ou nao)

come(pretas,TabBrancas,TabPretas,NTabBrancas,NTabPretas,come(Peca,Dir,N)) :-
	come1(TabPretas,TabBrancas,TabTmpPretas,TabTmpBrancas,Peca,Dir,AdjAdj),
	continuaComer(AdjAdj,Dir,1,pretas,TabTmpBrancas,[AdjAdj|TabTmpPretas],NTabBrancas,NTabPretas,N).

% Come uma peca preta e continua a comer (ou nao)

come(brancas,TabBrancas,TabPretas,NTabBrancas,NTabPretas,come(Peca,Dir,N)) :-
	come1(TabBrancas,TabPretas,TabTmpBrancas,TabTmpPretas,Peca,Dir,AdjAdj),
	continuaComer(AdjAdj,Dir,1,brancas,[AdjAdj|TabTmpBrancas],TabTmpPretas,NTabBrancas,NTabPretas,N).


%come10(Minhas,Dele,NMinhas,NDele,Peca,Dir,AdjAdj) :-
%	adjacente(Peca,Adj,Dir),
%	adjacente(Adj,AdjAdj,Dir),
%	\+ member(AdjAdj,Minhas),
%	\+ member(AdjAdj,Dele),
%	select(Adj,Dele,NDele),
%	select(Peca,Minhas,NMinhas).

% Come 1 peca
come1(Minhas,Dele,NMinhas,NDele,Peca,Dir,AdjAdj) :-
	select(Peca,Minhas,NMinhas),
	adjacente(Peca,Adj,Dir),
	select(Adj,Dele,NDele),
	adjacente(Adj,AdjAdj,Dir),
	\+ member(AdjAdj,Minhas),
	\+ member(AdjAdj,Dele).


% Para de comer (mesmo que possa ainda comer)

continuaComer(_,_,N,_,TabBrancas,TabPretas,TabBrancas,TabPretas,N).

% Continua a comer pretas numa certa direccao

continuaComer(Peca,Dir,N,brancas,TabBrancas,TabPretas,NTabBrancas,NTabPretas,F) :-
	come1(TabBrancas,TabPretas,TabTmpBrancas,TabTmpPretas,Peca,Dir,AdjAdj),
	NN is N + 1,
	continuaComer(AdjAdj,Dir,NN,brancas,[AdjAdj|TabTmpBrancas],TabTmpPretas,NTabBrancas,NTabPretas,F).

% Continua a comer brancas numa certa direccao

continuaComer(Peca,Dir,N,pretas,TabBrancas,TabPretas,NTabBrancas,NTabPretas,F) :-
	come1(TabPretas,TabBrancas,TabTmpPretas,TabTmpBrancas,Peca,Dir,AdjAdj),
	NN is N + 1,
	continuaComer(AdjAdj,Dir,NN,pretas,TabTmpBrancas,[AdjAdj|TabTmpPretas],NTabBrancas,NTabPretas,F).

% ----------------
% casas adjacentes
% ----------------

adjacente((L,C),(L,IncC),leste) :-
	dim(Dim),
	C < Dim,
	IncC is C + 1.
adjacente((L,C),(L,DecC),oeste) :-
	C > 1,
	DecC is C - 1.
adjacente((L,C),(IncL,C),sul) :-
	dim(Dim),
	L < Dim,
	IncL is L + 1.
adjacente((L,C),(DecL,C),norte) :-
	L > 1,
	DecL is L - 1.


% ------------------------------------------------
% 	MOSTRA TABULEIRO KONANE
% ------------------------------------------------

mostra_tab(tab(Prox,Num,TabB,TabP)) :-
	dim(Dim),
	nl,write('  '),mostraLinhaInicial(1,Dim),nl,
	mostraLinhas(1,Dim,tab(Prox,Num,TabB,TabP)),
	nl,nl,write(Num),write(' - '),write('Proximo a jogar: '),write(Prox),nl,nl.

% Mostra todas as linhas
mostraLinhas(Corr,Dim,_) :-
	Corr > Dim,!.
mostraLinhas(Corr,Dim,Tab) :-
	write(Corr),write(' '),
	mostraLinha(Corr,1,Dim,Tab),
	NCorr is Corr + 1,
	mostraLinhas(NCorr,Dim,Tab).

% Mostra a linha inicial
mostraLinhaInicial(Corr,Dim) :-
	Corr > Dim,
	nl,!.
mostraLinhaInicial(Corr,Dim) :-
	write(' '),write(Corr),write(' '),
	NCorr is Corr + 1,
	mostraLinhaInicial(NCorr,Dim).


% Mostra uma linha
mostraLinha(_,Corr,Dim,_) :-
	Corr > Dim,
	nl,!.
mostraLinha(Linha,Corr,Dim,Tab) :-
	desenha(Linha,Corr,Tab),
	NCorr is Corr + 1,
	mostraLinha(Linha,NCorr,Dim,Tab).

% Mostra uma casa 
desenha(Linha,Coluna,tab(_,_,Brancas,_)) :-
	member((Linha,Coluna),Brancas),
	desenhaBranca,!.
desenha(Linha,Coluna,tab(_,_,_,Pretas)) :-
	member((Linha,Coluna),Pretas),
	desenhaPreta,!.
desenha(_,_,_) :-
	desenhaVazio.

% desenha peca (B ou P) ou casa vazia (.)
desenhaPreta :-
	write(' P ').
desenhaBranca :-
	write(' B ').
desenhaVazio :-
	write(' . ').


% Mostra a jogada no ecran

mostraJogada(Jog) :-
	write(Jog),nl.


% O score e' o numero de jogadas
score(tab(_,Score,_,_),Score).



% Extras
tab(X) :- X=<0,!.
tab(X) :- X1 is X-1, write(' '), tab(X1).