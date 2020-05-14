import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.text.NumberFormat;
import java.util.LinkedList;
import java.util.Random;

import com.micropraxis.gajit.ChromItem;
import com.micropraxis.gajit.FixView;
import com.micropraxis.gajit.View;

/**
 * Classe que gera a populacao, constroi os pesos de cada cromossoma e efectua a interaccao com o prolog. 
 * @author Grupo018
 * Celso Sousa Nº32441
 * Sergio das Neves Nº32536
 * Henrique Cunha Nº33321
 */
public class GeraPopulacao {
  /**
   * As caracteristicas.
   */
  private String sCars;
  /**
   * A populacao.
   */
  private Populacao jogadores;
  /**
   * A vista. 
   */
  private View vista;
  /**
   * A interaccao com o prolog.
   */
  private PrologInteraction interacao;
  /**
   * O numero de cromossomas desta populacao.
   */
  private int numCromossomas;
  /**
   * O numero de genes de cada cromossoma desta populacao. 
   */
  private int numGenes;
  /**
   * O tamanho de cada gene.
   */
  private int tamGene;

  /**
   * O ponto de corte. 
   */
  private int pontoCorte;

  /**
   * A taxa de mutacao.
   */
  private double taxaMutacao;

  private double startRange;
  private double offsetRange;

  private int geracaoCorrente;

  /**
   * Constroi uma populacao. 
   */
  public GeraPopulacao () {
    try {
      leParametros();
    } catch (NumberFormatException e1) {
      e1.printStackTrace();
    } catch (IOException e1) {
      e1.printStackTrace();
    }
    jogadores = new Populacao (numCromossomas, tamGene, numGenes, pontoCorte, taxaMutacao);
    try {
      lePesos();
      salvaPesos();
    } catch (IOException e1) {
      e1.printStackTrace();
    }
    catch (ClassNotFoundException e) {
      e.printStackTrace();
    } 
    vista = new FixView(tamGene, startRange, offsetRange);
    try {
      interacao = new PrologInteraction(numCromossomas, numGenes);
    } catch (IOException e) {
      e.printStackTrace();
    }
    try {
      salvaParametros();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  /**
   * Constroi uma populacao.
   * @param numCromossomas O numero de cromossomas desta populacao.
   */
  public GeraPopulacao (int numCromossomas, String[] caracteristicas) {
    constroiDadosFixos(numCromossomas, caracteristicas);
    jogadores = new Populacao (numCromossomas, tamGene, numGenes, pontoCorte, taxaMutacao);
    try {
      salvaPesos();
    } catch (IOException e1) {
      e1.printStackTrace();
    }
    vista = new FixView(tamGene, startRange, offsetRange);
    try {
      interacao = new PrologInteraction(numCromossomas, numGenes);
    } catch (IOException e) {
      e.printStackTrace();
    }
      sCars = interacao.empacotarCars(caracteristicas);
    try {
      salvaParametros();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private void constroiDadosFixos(int numCromossomas, String[] caracteristicas){
    geracaoCorrente = 0;
    this.numCromossomas = numCromossomas;
    numGenes = caracteristicas.length;
    tamGene = 5;
    Random gen = new Random();
    pontoCorte = gen.nextInt(numGenes*tamGene);
    taxaMutacao = 0.005;
    startRange = -1.0;
    offsetRange = 2.0;
  }

  /**
   * Constroi os pesos, envia-os ao prolog, recebe a lista dos ids que 
   * identificam cada cromossoma e respectivos fitness's, cria uma nova 
   * geracao e retorna a string que contem o id e a respectiva lista de pesos do melhor cromossoma da geracao antiga.
   * 
   * @return A string contendo o id e os pesos do melhor jogador.
   * @throws IOException 
   */
  public String interage () {
    NumberFormat nf = NumberFormat.getInstance();
    nf.setMaximumFractionDigits(3);
    nf.setMinimumFractionDigits(3);
    LinkedList<Double> pesos = constroiPesos();
    String sPesos = interacao.empacotarDados(pesos,jogadores);
    String sFits = interacao.callProlog(sPesos, sCars);
    LinkedList<IdFit> idFits = interacao.desempacotarDados(sFits);
    int fit = idFits.get(0).getFitness();
    int id = idFits.get(0).getId();
    for (IdFit idf : idFits)
      if (fit < idf.getFitness()){
        fit = idf.getFitness();
        id = idf.getId();
      }
    int index = jogadores.getIndexFromId(id);
    int comeco = index*numGenes;
    StringBuilder sb = new StringBuilder("");
    sb.append(id + ": [");
    for(int i = comeco; i < comeco+numGenes; i++) {
      sb.append(nf.format(pesos.get(i)));
      if(i%3 == 0)
        sb.append("\n");
      if (i != comeco+numGenes-1)
        sb.append(";");
    }
    sb.append("]");
    jogadores.newGeneration(idFits);
    geracaoCorrente++;
    try {
      salvaPesos();
    } catch (IOException e) {
      e.printStackTrace();
    }
    return sb.toString();
  }

  private void salvaPesos() throws IOException{
    FileOutputStream os = new FileOutputStream("../pesos.bin");
    ObjectOutputStream oos = new ObjectOutputStream(os);
    oos.writeObject(jogadores.getChromItemsList());
    oos.writeObject(new Integer(geracaoCorrente));
    oos.close();
  }

  @SuppressWarnings("unchecked")
  private void lePesos() throws IOException, ClassNotFoundException{
    FileInputStream is = new FileInputStream("../pesos.bin");
    ObjectInputStream ois = new ObjectInputStream(is);
    jogadores.setChromItemsList((LinkedList<ChromItem>)ois.readObject());
    geracaoCorrente = (Integer)ois.readObject();
    ois.close();

  }
  
  private void salvaParametros() throws IOException{
    FileWriter f = new FileWriter(new File("../parametros"));
    f.write(new Integer(numCromossomas).toString() + "\n");
    f.write(new Integer(tamGene).toString() + "\n");
    f.write(new Integer(numGenes).toString() + "\n");
    f.write(new Integer(pontoCorte).toString() + "\n");
    f.write(new Double(taxaMutacao).toString() + "\n");
    f.write(new Double(startRange).toString() + "\n");
    f.write(new Double(offsetRange).toString() + "\n");
    f.write(sCars + "\n");
    f.close();
  }

  private void leParametros() throws NumberFormatException, IOException{
    FileReader r = new FileReader("../parametros");
    BufferedReader buf = new BufferedReader(r);
    numCromossomas = Integer.parseInt(buf.readLine());
    tamGene = Integer.parseInt(buf.readLine());
    numGenes = Integer.parseInt(buf.readLine()); 
    pontoCorte = Integer.parseInt(buf.readLine());
    taxaMutacao = Double.parseDouble(buf.readLine());
    startRange = Double.parseDouble(buf.readLine());
    offsetRange = Double.parseDouble(buf.readLine());
    sCars = buf.readLine();
    buf.close();
  }

  /**
   * Devolve uma lista de pesos.
   * 
   * @return A lista de pesos.
   */
  public LinkedList<Double> constroiPesos () {
    LinkedList<Double> pesos = new LinkedList<Double>();
    for(int i = 0; i < numCromossomas; i++) {
      for(int j = 0; j < numGenes; j++) {
        pesos.addFirst((Double)vista.getGene(jogadores.getChromItem(i).getChrom(), j));	
      }
    }
    return pesos;
  }

  public int getGeracaoCorrente(){
    return geracaoCorrente;
  }
}
