import com.micropraxis.util.ExtendedBitSet;
import com.micropraxis.gajit.Chrom;
import com.micropraxis.gajit.GenOp;

/**
 * Classe que representa o operador de recombinacao genetica.
 * @author Grupo018
 * Celso Sousa Nº32441
 * Sergio das Neves Nº32536
 * Henrique Cunha Nº33321
 */
public class XOverCutOp extends GenOp {
	/**
	 * O ponto de corte. 
	 */
	private int pontoDeCorte;

	/**
	 * O número de genes (caracteristicas).
	 */
	private int numGenes;
	/**
	 * O tamanho de acda gene.
	 */
	private int tamGene;

	/**
	 * Cria o operador de cruzamento com um dado ponto de corte. 
	 * @param pontoDeCorte O ponto de corte.
	 */
	public XOverCutOp (int pontoDeCorte, int numGenes, int tamGene) {
		super(true);
		this.pontoDeCorte = pontoDeCorte;
		this.numGenes = numGenes;
		this.tamGene = tamGene;
	}

	/**
	 * Aplica o cruzamento sobre dois cromossomas gerando um cromossoma filho
	 * (requer que os cromossomas tenham o mesmo tamanho).
	 * @param c1 O cromossoma pai.
	 * @param c2 O cromossoma mae.
	 * @return O cromossoma filho.
	 */
	public Chrom apply(Chrom c1, Chrom c2){
		String[] arr1 = c1.toString().split(":");
		String cadeia1 = arr1[1].substring(0, arr1[1].length()-1);
		String[] arr2 = c2.toString().split(":");
		String cadeia2 = arr2[1].substring(0, arr2[1].length()-1);
		String cadeiaNova = "";
		if (pontoDeCorte == 0)
			cadeiaNova = cadeia2;
		else if(pontoDeCorte == ((numGenes*tamGene)-1))
			cadeiaNova = cadeia1;
		else
			cadeiaNova = cadeia1.substring(0, pontoDeCorte) + cadeia2.substring(pontoDeCorte);
		return new Chrom(new ExtendedBitSet(cadeiaNova));
	}
}
