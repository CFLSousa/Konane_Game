import java.util.LinkedList;
import java.util.Random;

import com.micropraxis.gajit.Chrom;
import com.micropraxis.gajit.ChromItem;
import com.micropraxis.gajit.MutOp;

/**
 * Classe que representa uma populacao.
 * 
 * @author Grupo018
 * Celso Sousa Nº32441
 * Sergio das Neves Nº32536
 * Henrique Cunha Nº33321
 */
public class Populacao {
	/**
	 * A lista de ChromItems
	 */
	private LinkedList<ChromItem> chromItems;
	/**
	 * O numero de genes desta populacao.
	 */
	private int numGenes;
	/**
	 * O tamanho de um gene desta populacao. 
	 */
	private int tamanhoGene;
	/**
	 * O numero de cromossomas desta populacao.
	 */
	private int numCromossomas;
	/**
	 * O operador de mutacao.
	 */
	private MutOp mutador;
	/**
	 * O operador de cruzamento.
	 */
	private XOverCutOp cruzador;

	/**
	 * Constroi uma populacao aleatoria de um dado numero de cromossomas com tamanho dos genes e numero de genes definido.
	 * Os fitness's em cada ChromItem são inicializados a 0.0.
	 * @param numCromossomas O numero de cromossomas.
	 * @param tamanhoGene O tamanho dos genes.
	 * @param numGenes O numero de genes.
	 */
	public Populacao (int numCromossomas, int tamanhoGene, int numGenes) {
		chromItems = new LinkedList<ChromItem>();
		this.numGenes = numGenes;
		this.tamanhoGene = tamanhoGene;
		this.numCromossomas = numCromossomas;
		for (int i = 0; i < numCromossomas; i++){
			Chrom c = new Chrom(this.tamanhoGene*this.numGenes);
			ChromItem ci = new ChromItem(0.0, c);
			chromItems.addFirst(ci);
		}
		cruzador = new XOverCutOp(((tamanhoGene*numGenes)/2)-1, numGenes, tamanhoGene);
		mutador = new MutOp(0.005);
	}

	/**
	 * Constroi uma populacao aleatoria de um dado numero de cromossomas com tamanho dos genes, numero de genes e ponto de corte definidos.
	 * A taxa de mutacao e' por defeito 0.005.
	 * @param numCromossomas O numero de cromossomas.
	 * @param tamanhoGene O tamanho dos genes.
	 * @param numGenes O numero de genes.
	 * @param pontoDeCorte O ponto de corte.
	 */
	public Populacao (int numCromossomas, int tamanhoGene, int numGenes, int pontoDeCorte) {
		this(numCromossomas, tamanhoGene, numGenes);
		cruzador = new XOverCutOp(pontoDeCorte, numGenes, tamanhoGene);
	}

	/**
	 * Constroi uma populacao aleatoria de um dado numero de cromossomas com tamanho dos genes, numero de genes e taxa de mutacao definidos.
	 * O ponto de corte e' por defeito (tamanhoGene*numGenes)/2)-1) em que tamanhoGene e' 4 e numGenes e' 12.
	 * @param numCromossomas O numero de cromossomas.
	 * @param tamanhoGene O tamanho dos genes.
	 * @param numGenes O numero de genes.
	 * @param taxaMutacao A taxa de mutacao.
	 */
	public Populacao (int numCromossomas, int tamanhoGene, int numGenes, double taxaMutacao) {
		this(numCromossomas, tamanhoGene, numGenes);
		mutador = new MutOp(taxaMutacao);
	}

	/**
	 * Constroi uma populacao aleatoria de um dado numero de cromossomas com tamanho dos genes, numero de genes, ponto de corte e taxa de mutacao definidos.
	 * @param numCromossomas O numero de cromossomas.
	 * @param tamanhoGene O tamanho dos genes.
	 * @param numGenes O numero de genes.
	 * @param pontoDeCorte O ponto de corte.
	 * @param taxaMutacao A taxa de mutacao.
	 */
	public Populacao (int numCromossomas, int tamanhoGene, int numGenes, int pontoDeCorte, double taxaMutacao) {
		this(numCromossomas, tamanhoGene, numGenes);
		cruzador = new XOverCutOp(pontoDeCorte, numGenes, tamanhoGene);
		mutador = new MutOp(taxaMutacao);
	}

	/**
	 * Actualiza em cada ChromItem o fitness respectivo utilizando para isso a lista de fitness's dada como parametro
	 * e efectua os cruzamentos necessários para obter os individuos da proxima geracao.  
	 * @param fit A lista de IdFits .
	 */
	public void newGeneration(LinkedList<IdFit> fit){
		LinkedList<ChromItem> temp = new LinkedList<ChromItem>();
		assignFitsToChromItems(fit);
		for(int i = 0; i < numCromossomas; i++){
			Chrom pai = selectRandomChrom();
			Chrom mae = selectRandomChrom();
			Chrom filho = cruzador.apply(pai, mae);
			mutador.apply(filho);
			ChromItem ci = new ChromItem(0.0, filho);
			temp.addFirst(ci);
		}
		chromItems = temp;
	}

	/**
	 * Selecciona e devolve um cromossoma aleatorio segundo o metodo de seleccao denominado roleta.
	 * Este metodo funciona do seguinte modo:
	 * Faz a soma de todos os fitness's para achar o valor maximo da roleta. De seguida, gera um numero aleatorio entre 0 e a soma - 1.
	 * Depois, percorre todos os ChromItems ate encontrar um, cujo fitness associado seja maior que o numero aleatorio gerado e 
	 * devolve o cromossoma correspondente.
	 * Quanto maior o fitness de um dado cromossoma, maior a probabilidade de ele ser escolhido.
	 * @return O cromossoma. 
	 */
	private Chrom selectRandomChrom () {
		double soma = somatorioFits(); 
		Random gerador = new Random();
		int aleatorio = gerador.nextInt((int)soma);
		double verificador = 0.0;
		for (ChromItem ci : chromItems){
			verificador += ci.getDoubleFitness();
			if (verificador >= aleatorio)
				return ci.getChrom();
		}
		return null;
	}

	/**
	 * Devolve o somatorio de todos os fitness's presentes na lista de ChromItems.
	 * @return O somatorio dos fitness's.
	 */
	private double somatorioFits () {
		double soma = 0.0;
		for (ChromItem ci : chromItems){
			soma += ci.getDoubleFitness();
		}
		return soma;
	}

	/**
	 * Actualiza os ids na lista de chromItems atraves dos ids actualizados que vem na lista de IdFits.
	 * @param fit A lista de IdFits.
	 */
	private void assignFitsToChromItems(LinkedList<IdFit> fit){
		for (ChromItem ci : chromItems){
			for (IdFit f : fit){
				if (f.getId() == getIdFromChrom(ci.getChrom()))
					ci.setFitness(f.getFitness());
			}
		}
	}

	/**
	 * Obtem o id de um dado cromossoma.
	 * @param c O cromossoma.
	 * @return O id que identifica este cromossoma.
	 */
	public int getIdFromChrom(Chrom c){
		String[] arr = c.toString().split(":");
		return Integer.parseInt(arr[0].substring(1));
	}

	/**
	 * Obtem o indice relativo ao cromossoma com o id dado como parametro.
	 * @param id O id de um cromossoma.
	 * @return O indice correspondente a este id.
	 */
	public int getIndexFromId (int id) {
		for (int i = 0; i < chromItems.size(); i++)
			if (getIdFromChrom(chromItems.get(i).getChrom()) == id)
				return i;
		return -1;
	}

	/**
	 * Obtem o chromItem no indice dado como parametro.
	 * @param index O indice do chromItem.
	 * @return O chromItem num dado indice.
	 */
	public ChromItem getChromItem (int index) {
		return chromItems.get(index);
	}
  
  public LinkedList<ChromItem> getChromItemsList(){
    return chromItems;
  }
  
  public void setChromItemsList(LinkedList<ChromItem> chromItems){
    this.chromItems = chromItems;
  }
}
