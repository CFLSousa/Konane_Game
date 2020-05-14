# Name
Konane

## Description

Konane is a Prolog/Java application for evolve and find the best Konane player, apllying genetic algorithms. Konane is an ancient hawaiian checkers game.

## Configuration

First you must configure Java to work together with Prolog. For this it is necessary to do the following:
- Set the value of *JDK_HOME* with the location of the Java compiler (1.4.* Minimum);
- Set the value of *SP_PATH* with the location of the SICStus PROLOG;
- Add the **prologbeans.jar** library to *CLASSPATH* plus those that are needed (our **client.bat** file redefines this variable, so it is only necessary to set this variable if you are planning to run our client in another way than through the **client.bat**);
- Add the location of the SICStus PROLOG to *PATH* (only up to the *\bin* folder).

## Important files
The program maintains the status of the winners so far (their weights, the size of the gene, cutoff point, ranges). If the user wants to stop the program suddenly or in the event of a power failure, the relevant information is stored in some important files at the root of the project (**geneticos/** folder). The most important file for reading the information of winners is the *vencedores.txt* file, which, at any given moment, can present the following:

>Geracao 1:
O vencedor foi:
2: [0,871;0,806;-0,935
;0,032]

>Geracao 2:
O vencedor foi:
6: [0,871
;0,806;-0,935;0,032]

>Geracao 3:
O vencedor foi:
7: [0,548;-0,226
;-0,419;-0,871]

>...

The files where information is stored are:
- vencedores.txt: the winners information in textual format.
- vencedores.bin: the winners information is saved in binary for easier loading.
- pesos.bin: the last generation weights are saved in binary mode.
- parametros: are stored in textual format the values of: population size, size and number of genes, mutation rate, cutoff point, weights generation range and features previously selected by user.

## Usage
To execute our program, are available two files with the extension *.bat* (the *server.bat* must be the first to be executed and followed by the *client.bat*).

### Details/examples about usage

When executing the Java interface, one of the buttons is called `"Carregar população"` and it is used to load the population.´

When you press this button, if one of the files (*parametros*, *pesos.bin* or *vencedores.bin*) does not exist, it is given a message that a game may not have been previously saved. Thus, The first time the program is run, it is necessary to click on the `"Encontrar melhor jogador"` button.

You can stop the game at any time (exit the Java client and turn off the Prolog server). When you start up again with the Prolog server and then the Java client, you can click on the `"Carregar população"` button. A dialog box will appear asking you to enter the number of generations you want (values in text boxes `"Tamanho da população"` and `"Número de gerações"` are ignored).

To give an example, let's imagine that the first time you run the application you give an input of 50 generations and decided to stop the program at the 20th generation. Next time you run the application, if you enter 50 generations, it will calculate 30 more (if you want you can enter more or less generations than the first time, except a value that is less than or equal to the number of generations already calculated). For example, in this case if you enter a value less than or equal to 20 the program will not continue calculating generations.

It should be noted that, even when the program computes all the generations entered, as long as the *pesos.bin*, *vencedores.bin* and *parametros* files are not deleted, it is possible to make more generations on the weights that were computed, which allows it to continue to improve a player for the generations you want.

It should also be noted that if you have saved a game previously and in a later run click on `"Encontrar melhor jogador"`, the files from the previous calculation/run are lost, so you will have to save them elsewhere if you wish (*pesos.bin*, *vencedores.bin* and *parametros* files). Later, you can copy them back to the root of the project and continue to improve the player you had started.

## Extra configurations 

### Other games

If you want to run a game other than Konane, perform the following steps to change the characteristics of the game that Prolog will consider:
1. In **geneticos/konane/osjogadores.pl** file, implement the predicates of the characteristics you want to use.
2. In **geneticos/konane/server.pl** file, in predicate `arranque/0` where is written `write([minha_mobilidade,mobilidade_dele,meus_moves_possiveis,moves_possiveis_dele,
	minhasPodeComerDuas,delePodeComerDuas,meuNumeroPecas,deleNumeroPecas,
	meusCantosDaCor,deleCantosDaCor,minhasParedes,deleParedes])`, replace the list elements in this `write` predicate with the exact names of the predicates that were implemented in the **geneticos/konane/osjogadores.pl** file. These predicates correspond to the characteristics that the user intends to consider in the new game.
3. It should be noted that, due to the implementation of the *ExtendedBitSet* class of the gajit library (genetic algorithms in Java), the bit string of each chromosome cannot be longer than 64 bits. As we are using genes with size of 5 bits, we cannot have more than 12 characteristics in the list of characteristics given to `write` in predicate `arranque/0`.
4. When we start the Prolog server, it will write to the file *caracteristicas.txt* (at the root of the project) the list of characteristics, which will be loaded into the list box of the Java interface when running the Java program (obviously it is necessary to start the Prolog server first and then the Java program).
5. Before starting the Prolog server, it will be necessary to check the file corresponding to the new game that was implemented. For this, in predicate `initial/0` of the **geneticos/konane/ojogo.pl** file, replace `consult (oKonane)` with `consult(<Prolog_game_implemented_file>)`. Now it is possible to start the Prolog server and then the Java client.

### For changing the weights generation interval

Steps:
1. Edit **geneticos/geneticos_java/GeraPopulacao.java** file.
2. Inside the constructor: `public GeraPopulacao (int numCromossomas, String[] caracteristicas)` where you read `vista = new FixView(tamGene, startRange, offsetRange);`
(the parameters used for the variables are: *tamGene=5*, *startRange=-1.0*, *offsetRange=2.0*), change the second and third parameters. In the example shown here, the start range is [-1,1] (weights generated between -1 and 1).
3. Note that if the range is increased, the values will be more dispersed over the range. Why? Our gene is 5 bits long which gives 32 values and these 32 values must be distributed by the new range. So, if you increase the range, you have to keep in mind that, to maintain the same dispersion, you will have to increase the size of the gene, except that care must be taken so that the chromosome does not exceed 64 bits (due to the limitation described above).
4. If you change this range, you will have to compile **geneticos/geneticos_java/GeraPopulacao.java** file. To do this, on a command line, put our project's folder **geneticos/geneticos_java** as the current directory and execute the command: `javac -cp .;..\lib\gajit.jar GeraPopulacao.java`
This compilation method is used for any of the files in this directory having only to change the name of the class that the user wants to compile.

### For changing the depth level of the alpha-beta algorithm in Prolog

The user can also change the depth level of the alpha-beta algorithm in **geneticos/konane/campeonato.pl** file (currently this level is 3).
To do this, just go to the predicate `jogar_um_contra_todos/1` and change the second argument in `minicamp` call.

## Roadmap
Addition of the weights generation interval to the Java interface.

Prevent loss of files *pesos.bin*, *vencedores.bin* and *parametros* when two consecutive clicks occur in the button `"Encontrar melhor jogador"`. For that, it is necessary to save all the players that were being evolved during the first calculation/run, presenting the user a list with all of them so that he can choose which one the user pretends to continue to evolve at that point.

## Support
Email contact: zizo2@sapo.pt

## Authors
Grupo IIA018:
Celso Sousa Nº32441
Sérgio das Neves Nº32536
Henrique Cunha Nº33321

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## License
[MIT](https://choosealicense.com/licenses/mit/)