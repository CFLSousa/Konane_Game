:- module(evaluate,[main/0,arranque/0,inicio/3]).
:- use_module(library(prologbeans)).
:- use_module(library(codesio), [read_from_codes/2]).

/*
Faz o consult ao ficheiro ojogo.pl
(que realiza o consult aos restantes ficheiros do projecto)
e disponibiliza o motor de jogo.
*/
arranque :-
	consult(ojogo),
	tell('../caracteristicas.txt'),
	write([minha_mobilidade,mobilidade_dele,meus_moves_possiveis,moves_possiveis_dele,
	minhasPodeComerDuas,delePodeComerDuas,meuNumeroPecas,deleNumeroPecas,
	meusCantosDaCor,deleCantosDaCor,minhasParedes,deleParedes]),
	told.

/*
Arranca o prolog.
*/
:- arranque.

%% Register acceptable queries and start the server (using default port)
main :- 
	register_query(my_predicate(LIDP,LCars,LR), inicio(LIDP,LCars,LR)),	
	start.

/*
Converte a string recebida do java numa lista que o prolog possa manusear internamente
e chama o predicado que vai fazer a seleccao do melhor jogador de konane,
devolve a lista de jogadores identificados pelos seus id's e ordenada por pontos.
*/
inicio(LIDP,LCars,LR) :-
	read_from_codes(LIDP,LI), 
	read_from_codes(LCars,LC),
	iniciar(LI,LC,LR).	
