import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.Scanner;

import se.sics.prologbeans.Bindings;
import se.sics.prologbeans.PBTerm;
import se.sics.prologbeans.PrologSession;
import se.sics.prologbeans.QueryAnswer;

/**
 * Realiza a interaccao entre o java e o prolog.
 * @author Grupo018
 * Celso Sousa Nº32441
 * Sergio das Neves Nº32536
 * Henrique Cunha Nº33321
 */
public class PrologInteraction {
	/**
	 * A sessao que servira para fazer a interaccao com o prolog.
	 */
	private PrologSession session = new PrologSession();
	/**
	 * O numero de cromossomas.
	 */
	private int numCromossomas;
	/**
	 * O numero de genes.
	 */
	private int numGenes;
	/**
	 * Liga o java ao prolog ate o java receber de volta uma resposta. 
	 * @param numCromossomas O numero de cromossomas.
	 * @param numGenes O numero de genes.
	 * @throws java.io.IOException 
	 */
	public PrologInteraction (int numCromossomas, int numGenes) throws java.io.IOException {
		this.numCromossomas = numCromossomas;
		this.numGenes = numGenes;
		session.setTimeout(0);
		session.connect();
	}

	/**
	 * Devolve a string que sera passada ao prolog e que contem uma lista de tuplos
	 * constituida por id e lista de pesos respectiva ao cromossoma com esse id.
	 * @param pesos A lista de pesos relativa a esta populacao.
	 * @param jogadores A populacao.
	 * @return A string com a lista de ids e pesos respectivos que sera passada ao prolog. 
	 */
	public String empacotarDados (LinkedList<Double> pesos, Populacao jogadores) {
		StringBuilder sb = new StringBuilder("");  
		sb.append("[(");
		for (int i = 0; i < numCromossomas; i++) {
			sb.append(jogadores.getIdFromChrom(jogadores.getChromItem(i).getChrom()) + ",[");
			for(int j = 0; j < numGenes; j++) {
				sb.append(pesos.get(j));
				if(j != (numGenes-1))
					sb.append(",");
			}
			if(i != (numCromossomas-1))
				sb.append("]),(");
		}
		sb.append("])]");
		return sb.toString();
	}

	/**
	 * Devolve uma string que será passada ao prolog e que contem uma lista constituida pelas caracteristicas.
	 * @param caracteristicas O array de strings a empacotar.
	 * @return A string contendo a lista de caracteristicas.
	 */
	public String empacotarCars(String[] caracteristicas){
		StringBuilder sb = new StringBuilder("");
		sb.append("[");
		for (int i = 0; i < caracteristicas.length; i++){
			sb.append(caracteristicas[i]);
			if (i != caracteristicas.length-1)
				sb.append(",");
		}
		sb.append("]");
		return sb.toString();
	}
	/**
	 * Coloca os pares dos ids de jogadores e fitness's respectivos numa lista de IdFits.
	 * @param s A string que vem do prolog que contem a lista ordenada por fitness's.
	 * @return Devolve uma lista de IdFits que sao tuplos (id,fitness).
	 */
	public LinkedList<IdFit> desempacotarDados (String s) {
		String[] tuplos = s.split("\\),\\(");
		tuplos[0] = tuplos[0].substring(2);
		tuplos[numCromossomas-1] = tuplos[numCromossomas-1].substring(0, tuplos[numCromossomas-1].length()-2);
		LinkedList<IdFit> idFits = new LinkedList<IdFit>();
		for (int i = 0; i < tuplos.length; i++){
			String[] arr = tuplos[i].split(",");
			idFits.add(new IdFit(Integer.parseInt(arr[0]), Integer.parseInt(arr[1])));
		}
		return idFits;
	}

	/**
	 * Passa ao prolog a string com os ids e os pesos respectivos,
	 * recebe a lista de ids com os fitness's respectivos.
	 * @param s A string de ids e pesos respectivos que vai ser passada ao prolog.
	 * @return A string com os ids e os fitness's ordenada por fitness's.
	 */
	public String callProlog(String sPesos, String sCars) {
		try {
			Bindings bindings = new Bindings();
			bindings.bind("Pesos", sPesos + ".");
			bindings.bind("Cars", sCars + ".");
			QueryAnswer answer = session.executeQuery("my_predicate(Pesos,Cars,R)", bindings);
			PBTerm result = answer.getValue("R");
			if (result != null) {
				String str = result.toString();//Este metodo toString() coloca uma virgula a mais antes de cada tuplo da string.
				StringBuilder sb = new StringBuilder(str.replaceAll(",,", ","));//Tira todas as virgulas a mais excepto a que esta antes do primeiro parentesis curvo.
				return sb.deleteCharAt(1).toString();//Apaga a primeira virgula da lista.
			} else {
				return "Error: " + answer.getError() + '\n';
			}
		} catch (Exception e) {
			e.printStackTrace();
			return("Error when querying Prolog Server: " +
					e.getMessage() + '\n');
		}
	}

	public static String[] leCaracteristicas(){
		Scanner leitor = null;
		try {
			leitor = new Scanner(new File("../caracteristicas.txt"));	
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		String linha = leitor.nextLine();
		String[] arrCars = linha.split(",");
		arrCars[0] = arrCars[0].substring(1);
		int comprimentoUltimaCar = arrCars[arrCars.length-1].length();
		arrCars[arrCars.length-1] = arrCars[arrCars.length-1].substring(0,comprimentoUltimaCar-1);
		return arrCars;
	}
}  

