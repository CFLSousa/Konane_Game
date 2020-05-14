/*
Fornece um gerador de numeros aleatorios.
*/
:- use_module(library(random)).

/*
Adiciona as funcoes de avaliacao a base de conhecimento.
*/
adiciona_funcoes([]).
adiciona_funcoes([(ID,_)|RL]) :-
	name(mr_arre_burro_,L),
	constroi_componentes(ID,LIC), 
	constroi_nome(L,LIC,LR), 
	name(Func,LR),
	Y =.. [Func,Cor,Tab,V], 
	W =.. [final,Cor,Res], 
	M =.. [valorFinal,Cor,Res,V],
	name(pesos_,LP), 
	constroi_componentes(ID,LICP), 
	constroi_nome(LP,LICP,LRP), 
	name(FuncP,LRP),
	N =.. [FuncP,Pesos,Cars], 
	O =.. [soma_pesada,Cor,Tab,Pesos,Cars,V],
	assert((Y :- (W,M);(N,O))), 
	adiciona_funcoes(RL).

/*
Retira as funcoes de avaliacao da base de conhecimento.
*/
retira_funcoes([]).
retira_funcoes([(ID,_)|RL]) :-
	name(mr_arre_burro_,L), 
	constroi_componentes(ID,LIC), 
	constroi_nome(L,LIC,LR), 
	name(Func,LR),
	Y =.. [Func,Cor,Tab,V], 
	W =.. [final,Cor,Res], 
	M =.. [valorFinal,Cor,Res,V],
	name(pesos_,LP), 
	constroi_componentes(ID,LICP), 
	constroi_nome(LP,LICP,LRP), 
	name(FuncP,LRP),
	N =.. [FuncP,Pesos,Cars], 
	O =.. [soma_pesada,Cor,Tab,Pesos,Cars,V],
	retract((Y :- (W,M);(N,O))), 
	retira_funcoes(RL).

/*
Adiciona os predicados que chamam as funcoes de avaliacao a base de conhecimento.
*/
adiciona_func_avals([]).
adiciona_func_avals([(ID,_)|RL]) :-
	name(mr_arre_burro_,L), 
	constroi_componentes(ID,LIC), 
	constroi_nome(L,LIC,LR), 
	name(Func,LR),
	A =.. [func_avaliacao,ID,Func], 
	assert(A), 
	adiciona_func_avals(RL). 

/*
Retira os predicados que chamam as funcoes de avaliacao da base de conhecimento.
*/
retira_func_avals([]).
retira_func_avals([(ID,_)|RL]) :-
	name(mr_arre_burro_,L), 
	constroi_componentes(ID,LIC), 
	constroi_nome(L,LIC,LR), name(Func,LR),
	A =.. [func_avaliacao,ID,Func], 
	retract(A), 
	retira_func_avals(RL). 

/*
Adiciona os pesos das caracteristicas de cada jogador a base de conhecimento.
*/
adiciona_pesos([],_).
adiciona_pesos([(ID,LPesos)|RL],LCars) :-
	name(pesos_,L), 
	constroi_componentes(ID,LIC), 
	constroi_nome(L,LIC,LR), 
	name(Func,LR),
	W =.. [Func,LPesos,LCars],
	assert((W)), 
	adiciona_pesos(RL,LCars).

/*
Retira os pesos das caracteristicas de cada jogador da base de conhecimento.
*/	
retira_pesos([],_).
retira_pesos([(ID,LPesos)|RL],LCars) :-
	name(pesos_,L), 
	constroi_componentes(ID,LIC), 
	constroi_nome(L,LIC,LR), 
	name(Func,LR),
	W =.. [Func,LPesos,LCars],
	retract((W)), 
	retira_pesos(RL,LCars).

/*
Junta o nome do predicado na lista dada como primeiro argumento com a lista de componentes (codigos ASCII) do valor do ID 
de uma funcao de avaliacao dada como segundo argumento e coloca numa lista o resultado dessa juncao.
*/
constroi_nome(L,[],L).
constroi_nome(L,[P|LICR], LR) :- 
	D is P + 48, 
	inserir_na_cauda(L,D,LInt), 
	constroi_nome(LInt,LICR,LR).

/*
Constroi a lista de inteiros (codigos ASCII) componentes de um dado valor (ID de uma funcao de avaliacao). 
*/
constroi_componentes(I, LIC) :- 
	constroi_divisor(I,Div), 
	constroi_lic([],I,Div,LIC).

/*
Pega num inteiro (por exemplo: 1210) e devolve os códigos ASCII constituintes dentro de uma lista (resultado: [49,50,49,48]).
*/
constroi_lic(LIC,0,0,LIC).
constroi_lic(L,I,Div,LIC) :-
	R is I // Div, 
	inserir_na_cauda(L,R,LInt), 
	S is I mod Div, 
	T is Div // 10, 
	constroi_lic(LInt,S,T,LIC).

/*
Constroi o divisor de um numero de maneira a conseguir obter o numero inteiro na posicao mais a 
esquerda do numero dado como argumento (usa predicado auxiliar constroi_divisor_aux e adicionaRec).
*/
constroi_divisor(X,Div) :-
	X >= 0,
	name(X,L), 
	length(L,C), 
	D is C - 1, 
	constroi_divisor_aux(D,LDiv), 
	name(Div,LDiv).

/*
Coloca um 1 no algarismo mais significativo do divisor em construcao 
(se ID passado ao constroi_divisor tiver pelo menos dois digitos terao de ser adicionados zeros a direita).
*/
constroi_divisor_aux(0,IR) :- 
	name(1,IR).
constroi_divisor_aux(I,IR) :- 
	name(1,LI), 
	adicionaRec(I,LI,IR). 

/*
Coloca zeros a direita do 1 que esta como algarismo mais significativo do divisor em construcao.
*/
adicionaRec(0,LR,LR).
adicionaRec(I,LI,LR) :-
	inserir_na_cauda(LI,48,LInt), 
	J is I - 1, 
	adicionaRec(J,LInt,LR).

/*
Insere um elemento na cauda de uma dada lista.
*/
inserir_na_cauda(L,Elem,LR) :-
	comprimento(L,V), 
	inserir(L,V,Elem,LR).

/*
Insere um elemento num dado indice de uma lista.
*/
inserir(L,0,E,[E|L]).
inserir([X|Xs],I,E,[X|Xss]) :- 
	I1 is I - 1,
	inserir(Xs,I1,E,Xss).

/*
Dado quem esta a pensar e o resultado do jogo, se o primeiro for igual ao segundo devolve 1000; 
caso contrario devolve -1000.  
*/
valorFinal(Nome,Nome,1000) :- !.
valorFinal(_,_,-1000).

/*
Recebe como argumentos a cor do jogador para o qual vai ser efectuado o calculo, um estado do tabuleiro, 
a lista de pesos, a lista de caracteristicas e devolve o valor do calculo.
Faz o somatorio das multiplicacoes das caracteristicas pelos pesos respectivos (P1 * C1 + P2 * C2+ ... + Pn * Cn).
*/
soma_pesada(_,_,[],[],0).
soma_pesada(Cor,Tab,[P1|RPs],[C1|RCs],Valor) :-
	soma_pesada(Cor,Tab,RPs,RCs,Rv), 
	F1 =.. [C1,Cor,Tab,X], 
	call(F1), 
	Valor is Rv + ((X * P1) / 1000).

%%----------------------------------%%
%%                                  %%
%%   As 12 caracteristicas usadas   %%
%%                                  %%
%%----------------------------------%%

%%--------------------------------------------------------------------------------------------------------------%%
%%   O numero de pecas do adversario que podem ser comidas por cada peca deste jogador (movimentos possiveis)   %%
%%--------------------------------------------------------------------------------------------------------------%%

/*
Calcula os movimentos possiveis para o jogador que esta a pensar.
Para cada peca, ve para quantas direccoes e que ela pode comer (varia entre 0 e 4) e faz a soma do numero 
de movimentos possiveis de todas as pecas deste jogador.
*/
meus_moves_possiveis(brancas,tab(_,_,LB,LP),V) :-
	findall(J,jogada(tab(brancas,20,LB,LP),_,J),JS),
	length(JS,V).
meus_moves_possiveis(pretas,tab(_,_,LB,LP),V) :-
	findall(J,jogada(tab(pretas,20,LB,LP),_,J),JS),
	length(JS,V).

/*
Calcula os movimentos possiveis para o adversario do jogador que esta a pensar.
Para cada peca, ve para quantas direccoes e que ela pode comer (varia entre 0 e 4) e faz a soma do numero 
de movimentos possiveis de todas as pecas deste jogador.
*/
moves_possiveis_dele(pretas,tab(_,_,LB,LP),V) :-
	findall(J,jogada(tab(brancas,20,LB,LP),_,J),JS),
	length(JS,V).
moves_possiveis_dele(brancas,tab(_,_,LB,LP),V) :-
	findall(J,jogada(tab(pretas,20,LB,LP),_,J),JS),
	length(JS,V).

%%---------------------------------------------------------------------------%%
%%   O numero de pecas que podem comer uma peca do adversario (mobilidade)   %%
%%---------------------------------------------------------------------------%%

/*
Calcula o numero de pecas que podem comer do jogador que esta a pensar.
*/
minha_mobilidade(brancas,tab(_,_,LB,LP),V) :-
	setof(X,W^Y^Z^jogada(tab(brancas,20,LB,LP),W,come(X,Y,Z)),JS),
	length(JS,V),!.
minha_mobilidade(pretas,tab(_,_,LB,LP),V) :-
	setof(X,W^Y^Z^jogada(tab(pretas,20,LB,LP),W,come(X,Y,Z)),JS),
	length(JS,V),!.
minha_mobilidade(_,_,0).

/*
Calcula o numero de pecas que podem comer do adversario do jogador que esta a pensar.
*/
mobilidade_dele(pretas,tab(_,_,LB,LP),V) :-
	setof(X,W^Y^Z^jogada(tab(brancas,20,LB,LP),W,come(X,Y,Z)),JS),
	length(JS,V).
mobilidade_dele(brancas,tab(_,_,LB,LP),V) :-
	setof(X,W^Y^Z^jogada(tab(pretas,20,LB,LP),W,come(X,Y,Z)),JS),
	length(JS,V).
mobilidade_dele(_,_,0).

%%-------------------------------------------------%%
%%    Numero de pecas nos cantos que podem comer   %%
%%-------------------------------------------------%%

/*
Calcula o numero de pecas nos cantos do tabuleiro do jogador desta cor que podem comer.
*/
meusCantosBonsDaCor(Cor,Tab,N) :-
  	findall(C,cantoBomDaCor(Cor,Tab,C),L),
	length(L,N).

/*
Calcula o numero de pecas nos cantos do tabuleiro do jogador adversario do jogador desta cor que podem comer.
*/
deleCantosBonsDaCor(Cor,Tab,N) :-
  	adversario(Cor,CorDele),
	findall(C,cantoBomDaCor(CorDele,Tab,C),L),
	length(L,N).

/*
Dada uma peca de uma dada cor num canto do tabuleiro, ve se ela pode comer alguma peca adversaria.
*/
cantoBomDaCor(Cor,Tab,CantoCor) :-
   	canto_cor(Cor,Tab,CantoCor),
   	jogada(Tab,_,come(CantoCor,_,_)).

/*
Obtem as pecas brancas que estiverem nos cantos do tabuleiro.
*/
canto_cor(brancas,tab(_,_,Bs,_),CantoCor) :-
	dim(Dim),
	member(CantoCor,[(1,Dim),(Dim,1),(1,1),(Dim,Dim)]),
	member(CantoCor,Bs).

/*
Obtem as pecas pretas que estiverem nos cantos do tabuleiro.
*/
canto_cor(pretas,tab(_,_,_,Ps),CantoCor) :-
	dim(Dim),
	member(CantoCor,[(1,Dim),(Dim,1),(1,1),(Dim,Dim)]),
	member(CantoCor,Ps).

%%------------------------------------------------------------------------------------%%
%%   Numero de pecas junto as paredes do tabuleiro (excepto cantos) que podem comer   %%
%%------------------------------------------------------------------------------------%%

/*
Calcula o numero de pecas junto as paredes do tabuleiro do jogador desta cor que podem comer.
*/
minhasParedesBoas(Cor,Tab,N) :-
	findall(C,paredeBoaDaCor(Cor,Tab,C),L),
	length(L,N).	

/*
Calcula o numero de pecas junto as paredes do tabuleiro do jogador adversario do jogador desta cor que podem comer.
*/
deleParedesBoas(Cor,Tab,N) :-
	adversario(Cor,CorDele),
	findall(C,paredeBoaDaCor(CorDele,Tab,C),L),
	length(L,N).

/*
Dada uma peca de uma dada cor junto a uma das paredes do tabuleiro, 
ve se ela pode comer alguma peca adversaria.
*/
paredeBoaDaCor(Cor,Tab,ParedeCor) :-
   	parede_cor(Cor,Tab,ParedeCor),
   	jogada(Tab,_,come(ParedeCor,_,_)).

/*
Obtem as pecas brancas que estiverem junto as paredes do tabuleiro.
*/
parede_cor(brancas,tab(_,_,Bs,_),ParedeCor) :-
	member(ParedeCor,[(1,2),(1,3),(1,4),(1,5),(2,1),(3,1),(4,1),(5,1),(6,2),(6,3),(6,4),(6,5),(2,6),(3,6),(4,6),(5,6)]),
	member(ParedeCor,Bs).

/*
Obtem as pecas pretas que estiverem junto as paredes do tabuleiro.
*/
parede_cor(pretas,tab(_,_,_,Ps),ParedeCor) :-
	member(ParedeCor,[(1,2),(1,3),(1,4),(1,5),(2,1),(3,1),(4,1),(5,1),(6,2),(6,3),(6,4),(6,5),(2,6),(3,6),(4,6),(5,6)]),
	member(ParedeCor,Ps).

%%-----------------------------------------------%%
%%  Numero de pecas que podem comer duas pecas   %%
%%-----------------------------------------------%%

/*
Calcula o numero de pecas do jogador desta cor que podem comer duas pecas.
*/
minhasPodeComerDuas(Cor,Tab,N) :-
	findall(C,podeComerDuas(Cor,Tab,C),L),
	length(L,N).	

/*
Calcula o numero de pecas do jogador adversario do jogador desta cor que podem comer duas pecas.
*/
delePodeComerDuas(Cor,Tab,N) :-
	adversario(Cor,CorDele),
	findall(C,podeComerDuas(CorDele,Tab,C),L),
	length(L,N).

/*
Dada uma peca de uma dada cor no tabuleiro, 
ve se ela pode comer duas pecas adversarias na mesma jogada.
*/
podeComerDuas(Cor,Tab,Peca) :-
   	possibilidadeComerDuas(Cor,Tab,Peca),
   	jogada(Tab,_,come(Peca,_,2)).

/*
Obtem as pecas brancas no tabuleiro.
*/
possibilidadeComerDuas(brancas,tab(_,_,Bs,_),Peca) :-
	member(Peca,Bs).

/*
Obtem as pecas pretas no tabuleiro.
*/
possibilidadeComerDuas(pretas,tab(_,_,_,Ps),Peca) :-
	member(Peca,Ps).


%%------------------------------------%%
%%   O numero de pecas no tabuleiro   %%
%%------------------------------------%%

/*
Calcula o numero de pecas do jogador desta cor no tabuleiro.
*/
meuNumeroPecas(Cor,Tab,N) :-
	numeroPecas(Cor,Tab,N).	

/*
Calcula o numero de pecas do adversario do jogador desta cor no tabuleiro.
*/
deleNumeroPecas(Cor,Tab,N) :-
	adversario(Cor,CorDele),
	numeroPecas(CorDele,Tab,N).

/*
Calcula o numero de pecas brancas no tabuleiro.
*/
numeroPecas(brancas,tab(_,_,Bs,_),N) :-
	length(Bs,N).

/*
Calcula o numero de pecas pretas no tabuleiro.
*/
numeroPecas(pretas,tab(_,_,_,Ps),N) :-
	length(Ps,N).	

%%-----------------------------------%%
%%   Características acrescentadas   %%
%%-----------------------------------%%


%%---------------------------------------------------------%%
%%    Numero de pecas nos cantos (não podem ser comidas)   %%
%%---------------------------------------------------------%%

/*
Calcula o numero de pecas nos cantos do tabuleiro do jogador desta cor.
*/
meusCantosDaCor(Cor,Tab,N) :-
  	findall(C,cantoCor(Cor,Tab,C),L),
	length(L,N).

/*
Calcula o numero de pecas nos cantos do tabuleiro do jogador adversario do jogador desta cor.
*/
deleCantosDaCor(Cor,Tab,N) :-
  	adversario(Cor,CorDele),
	findall(C,cantoCor(CorDele,Tab,C),L),
	length(L,N).

/*
Obtem as pecas brancas que estiverem nos cantos do tabuleiro.
*/
cantoCor(brancas,tab(_,_,Bs,_),CantoCor) :-
	dim(Dim),
	member(CantoCor,[(1,Dim),(Dim,1),(1,1),(Dim,Dim)]),
	member(CantoCor,Bs).

/*
Obtem as pecas pretas que estiverem nos cantos do tabuleiro.
*/
cantoCor(pretas,tab(_,_,_,Ps),CantoCor) :-
	dim(Dim),
	member(CantoCor,[(1,Dim),(Dim,1),(1,1),(Dim,Dim)]),
	member(CantoCor,Ps).

%%--------------------------------------------------------------------%%
%%   Numero de pecas junto as paredes do tabuleiro (excepto cantos)   %%
%%--------------------------------------------------------------------%%

/*
Calcula o numero de pecas junto as paredes do tabuleiro do jogador desta cor.
*/
minhasParedes(Cor,Tab,N) :-
	findall(C,paredeDaCor(Cor,Tab,C),L),
	length(L,N).	

/*
Calcula o numero de pecas junto as paredes do tabuleiro do jogador adversario do jogador desta cor.
*/
deleParedes(Cor,Tab,N) :-
	adversario(Cor,CorDele),
	findall(C,paredeDaCor(CorDele,Tab,C),L),
	length(L,N).

/*
Dada uma peca de uma dada cor junto a uma das paredes do tabuleiro.
*/
paredeDaCor(Cor,Tab,ParedeCor) :-
   	paredeCor(Cor,Tab,ParedeCor).

/*
Obtem as pecas brancas que estiverem junto as paredes do tabuleiro.
*/
paredeCor(brancas,tab(_,_,Bs,_),ParedeCor) :-
	member(ParedeCor,[(1,2),(1,3),(1,4),(1,5),(2,1),(3,1),(4,1),(5,1),(6,2),(6,3),(6,4),(6,5),(2,6),(3,6),(4,6),(5,6)]),
	member(ParedeCor,Bs).

/*
Obtem as pecas pretas que estiverem junto as paredes do tabuleiro.
*/
paredeCor(pretas,tab(_,_,_,Ps),ParedeCor) :-
	member(ParedeCor,[(1,2),(1,3),(1,4),(1,5),(2,1),(3,1),(4,1),(5,1),(6,2),(6,3),(6,4),(6,5),(2,6),(3,6),(4,6),(5,6)]),
	member(ParedeCor,Ps).
