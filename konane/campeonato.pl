/*
Estes predicados em dynamic podem ser asserted e retracted da base de conhecimento.
*/
:- dynamic(dados_jogador/2).

/*
Encontra o melhor jogador de Konane, devolve a lista de jogadores
identificados pelos seus id's e ordenada por pontos.
*/
iniciar(L,LCars,LR) :-
	adiciona_funcoes(L), 
	adiciona_pesos(L,LCars), 
	adiciona_func_avals(L),
	cria_populacao_inicial(L), 
	jogar_campeonato(L),
	organiza_jogadores(LR),
	limpar_pontos,
	retira_func_avals(L),
	retira_pesos(L,LCars),
	retira_funcoes(L).

/*
Cria uma populacao inicial de jogadores,
todos com zero pontos e um numero de jogador individual unico.
*/
cria_populacao_inicial([]).
cria_populacao_inicial([(ID,_)|RL]) :- 
	assert(dados_jogador(ID,0)),
	cria_populacao_inicial(RL).

/*
Limpa todos os jogadores na base de conhecimento ao fim de cada geracao.
*/
limpar_pontos :-
	retractall(dados_jogador(_,_)).

/*
Efectua todos os jogos de um campeonato.
*/
jogar_campeonato([(_,_)]).
jogar_campeonato([L|LS]) :-
	jogar_um_contra_todos([L|LS]),
	jogar_campeonato(LS).

/*
Coloca o jogador no primeiro tuplo da lista a jogar contra todos os restantes jogadores
contra quem ainda nao jogou.
*/
jogar_um_contra_todos([(_,_)]).
jogar_um_contra_todos([(ID,_),(IDOpon,_)|R]) :-
	minicamp(1,3,ID,IDOpon,VitoriasJog,VitoriasOponente),
	ve_quem_ganha(ID,IDOpon,VitoriasJog,VitoriasOponente),
	jogar_um_contra_todos([(ID,_)|R]).

/*
Verifica se o jogo teve um vencedor ou se houve empate e actualiza os pontos em conformidade.
*/
ve_quem_ganha(NumJogador,_,VitoriasJog,VitoriasOponente) :-
	VitoriasJog > VitoriasOponente, 
	obter_pontos_jogador(NumJogador,Pontos),
	NovosPontos is Pontos + 2, 
	actualiza_pontos_jogador(NumJogador,Pontos,NovosPontos).

ve_quem_ganha(_,NumOponente,VitoriasJog,VitoriasOponente) :-
	VitoriasOponente > VitoriasJog, 
	obter_pontos_jogador(NumOponente,PontosOponente),
	NovosPontosOponente is PontosOponente + 2, 
	actualiza_pontos_jogador(NumOponente,PontosOponente,NovosPontosOponente).

ve_quem_ganha(NumJogador,NumOponente,_,_) :-
	obter_pontos_jogador(NumJogador,Pontos),
	obter_pontos_jogador(NumOponente,PontosOponente),
	NovosPontos is Pontos + 1, 
	NovosPontosOponente is PontosOponente + 1,
	actualiza_pontos_jogador(NumJogador,Pontos,NovosPontos), 
	actualiza_pontos_jogador(NumOponente,PontosOponente,NovosPontosOponente).

/*
Obtem os pontos de um dado jogador.
*/
obter_pontos_jogador(NumJogador,Pontos) :-
	dados_jogador(NumJogador,Pontos).

/*
Actualiza o fitness de um dado jogador dado o seu id.
*/
actualiza_pontos_jogador(NumJogador,PontosAntigos,NovosPontos) :-
	retract(dados_jogador(NumJogador,PontosAntigos)),
	assert(dados_jogador(NumJogador,NovosPontos)).

/*
Obtém todos os jogadores e seus pontos e devolve outra lista com esses mesmos jogadores 
identificados pelo seu id e ordenada pelos respectivos pontos do maior para o menor valor.
*/
organiza_jogadores(SortedPlayerList) :-
	setof((NumJogador,Pontos),dados_jogador(NumJogador,Pontos),UnsortedPlayerList),
	ordena_jogadores_por_pontos(UnsortedPlayerList,SortedPlayerList).

/*
Realiza a chamada ao predicado de ordenacao com a lista desordenada.
*/
ordena_jogadores_por_pontos(UnsortedPlayerList,SortedPlayerList) :-
	ordena_jogadores_por_pontos(UnsortedPlayerList,[],SortedPlayerList).

/*
Ordena todos os jogadores pelo criterio do maior fitness usando uma lista intermedia
que serve de suporte para a construcao da lista resultado que ser a lista ordenada,
os primeiros jogadores sao os que tem mais pontos
(em caso de empate quem tiver id menor aparece primeiro na lista ordenada).
*/
ordena_jogadores_por_pontos([],NewListSorted,NewListSorted).
ordena_jogadores_por_pontos(UnsortedPlayerList,LI,NewListSorted) :-
	maior_da_lista(UnsortedPlayerList,MaiorTuplo),
	apaga(MaiorTuplo,UnsortedPlayerList,UnsortedPlayerListWithoutGreater),
	inserir_na_cauda(LI,MaiorTuplo,NewLI),
	ordena_jogadores_por_pontos(UnsortedPlayerListWithoutGreater,NewLI,NewListSorted).

/*
Apaga um elemento (o primeiro) que aparece numa dada lista.
*/
apaga(X,[X|Xs],Xs).
apaga(X,[Y|Ys],[Y|Yss]) :- 
	X \= Y, 
	apaga(X,Ys,Yss).

/*
Devolve o comprimento de uma lista.
*/
comprimento([],0).
comprimento([_|Xs],Z) :- 
	comprimento(Xs,V), 
	Z is V + 1.

/*
Dada uma lista de tuplos (Numero_de_jogador,Pontos_desse_Jogador), devolve o tuplo com maior valor de pontos na lista.
Este predicado usa o predicado auxiliar maior_da_lista/3 para facilitar o cálculo.
*/
maior_da_lista([(NumJog,Pontos)|Resto],(NumJogComMaisPontos,MaiorVal)) :- 
	MaxIntermedio = Pontos, 
	NumJogIntermedio = NumJog, 
	maior_da_lista(Resto,MaxIntermedio,NumJogIntermedio,(NumJogComMaisPontos,MaiorVal)).

maior_da_lista([],MaxIntermedio,NumJogIntermedio,(NumJogComMaisPontos,MaxIntermedio)) :-
	NumJogComMaisPontos = NumJogIntermedio.
maior_da_lista([(NumJog,Pontos)|R],MaxIntermedio,NumJogIntermedio,(NumJogComMaisPontos,MaiorVal)) :- 
	(Pontos > MaxIntermedio,
	NumJogInt = NumJog, 
	NovoMaior = Pontos, 
	maior_da_lista(R,NovoMaior,NumJogInt,(NumJogComMaisPontos,MaiorVal)));
	(Pontos =< MaxIntermedio,
	maior_da_lista(R,MaxIntermedio,NumJogIntermedio,(NumJogComMaisPontos,MaiorVal))).
