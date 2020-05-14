/*
Fornece um gerador de numeros aleatorios.
*/
:- use_module(library(random)).


iniciar_teste(L,LCars,LR) :-
	adiciona_funcoes(L), 
	adiciona_pesos_teste(L,LCars), 
	adiciona_func_avals(L),
	cria_populacao_inicial(L),
	jogar_campeonato(L),
	organiza_jogadores(LR),
	limpar_pontos,
	retira_func_avals(L),
	retira_pesos_teste(L,LCars),
	retira_funcoes(L).

/*
Adiciona os pesos das caracteristicas de cada jogador a base de conhecimento.
*/
adiciona_pesos_teste([],_).
adiciona_pesos_teste([(ID,LPesos)|RL],[PCar|LCars]) :-
	name(pesos_,L), 
	constroi_componentes(ID,LIC), 
	constroi_nome(L,LIC,LR), 
	name(Func,LR),
	W =.. [Func,LPesos,PCar],
	assert((W)), 
	adiciona_pesos_teste(RL,LCars).

/*
Retira os pesos das caracteristicas de cada jogador da base de conhecimento.
*/	
retira_pesos_teste([],_).
retira_pesos_teste([(ID,LPesos)|RL],[PCar|LCars]) :-
	name(pesos_,L), 
	constroi_componentes(ID,LIC), 
	constroi_nome(L,LIC,LR), 
	name(Func,LR),
	W =.. [Func,LPesos,PCar],
	retract((W)), 
	retira_pesos_teste(RL,LCars).



