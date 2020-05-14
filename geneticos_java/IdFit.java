/**
 * A classe que associa um id a um fitness.
 * 
 * @author Grupo018
 * Celso Sousa Nº32441
 * Sergio das Neves Nº32536
 * Henrique Cunha Nº33321
 */
public class IdFit {
  /**
   * O fitness.
   */
  private int fitness;
  /**
   * O id.
   */
  private int id;

  /**
   * Constroi um IdFit.
   * @param id O id.
   * @param fitness O fitness.
   */
  public IdFit(int id, int fitness) {
    this.fitness = fitness;
    this.id = id;
  }

  /**
   * Devolve o id deste IdFit.
   * @return O id deste IdFit.
   */
  public int getId(){
    return id;
  }

  /**
   * Devolve o fitness deste IdFit.
   * @return O fitness deste IdFit.
   */
  public int getFitness(){
    return fitness;
  }
  
  /**
   * Devolve uma representacao textual deste IdFit.
   * @return A string com a representacao textual.
   */
  public String toString(){
    String str = fitness + ":" + id;
    return str;
  }
}
