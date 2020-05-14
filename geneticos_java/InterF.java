import java.awt.Rectangle;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

public class InterF extends JFrame {

  private StringBuffer sb = new StringBuffer("");  //  @jve:decl-index=0:
  private static final long serialVersionUID = 1L;
  private JPanel jContentPane = null;
  private JTextField jTextFieldTamPop = null;
  private JTextField jTextFieldNumGeracoes = null;
  private JLabel jLabelTamPop = null;
  private JLabel jLabelNumGeracoes = null;
  private JButton jButtonEncontrarMelhorJogador = null;
  private JButton jButtonLimparDados = null;
  private JButton jButtonSair = null;
  private JButton jButtonAcercaDe = null;
  private JTextArea jTextAreaOMelhorJogador = null;
  private JLabel jLabelOMelhorJogador = null;
  private JScrollPane jScrollPane = null;
  private JScrollPane jScrollPaneListCars = null;
  private JList jListListaCars = null;
  private JLabel jLabelListaCars = null;
  private JButton jButtonCarregaPop = null;
  /**
   * This method initializes jTextFieldTamPop	
   * 	
   * @return javax.swing.JTextField	
   */
  private JTextField getJTextFieldTamPop () {
    if (jTextFieldTamPop == null) {
      jTextFieldTamPop = new JTextField();
      jTextFieldTamPop.setBounds(new Rectangle(151, 18, 100, 20));
    }
    return jTextFieldTamPop;
  }

  /**
   * This method initializes jTextFieldNumGeracoes	
   * 	
   * @return javax.swing.JTextField	
   */
  private JTextField getJTextFieldNumGeracoes () {
    if (jTextFieldNumGeracoes == null) {
      jTextFieldNumGeracoes = new JTextField();
      jTextFieldNumGeracoes.setBounds(new Rectangle(152, 44, 100, 20));
    }
    return jTextFieldNumGeracoes;
  }

  /**
   * This method initializes jButtonEncontrarMelhorJogador	
   * 	
   * @return javax.swing.JButton	
   */
  private JButton getJButtonEncontrarMelhorJogador () {
    if (jButtonEncontrarMelhorJogador == null) {
      jButtonEncontrarMelhorJogador = new JButton();
      jButtonEncontrarMelhorJogador.setBounds(new Rectangle(13, 326, 179, 21));
      jButtonEncontrarMelhorJogador.setText("Encontrar melhor jogador");
      jButtonEncontrarMelhorJogador.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed (java.awt.event.ActionEvent e) {
          String tamPopStr = jTextFieldTamPop.getText();
          String numGeracoesStr = jTextFieldNumGeracoes.getText();
          try{
            Integer.parseInt(tamPopStr); 
            Integer.parseInt(numGeracoesStr);
          } catch (NumberFormatException nfe) {
            JOptionPane.showMessageDialog(jContentPane,
            "Introduza apenas valores numéricos");
            return;
          }
          if(tamPopStr.equals("") || numGeracoesStr.equals("")){
            JOptionPane.showMessageDialog(jContentPane,
            "Introduza os valores em falta");
            return;
          }
          if (Integer.parseInt(tamPopStr) <= 1){
            JOptionPane.showMessageDialog(jContentPane,
            "Introduza um numero de jogadores maior que 1.");
            return;
          }
          if (Integer.parseInt(numGeracoesStr) <= 0){
            JOptionPane.showMessageDialog(jContentPane,
            "Introduza um numero de geracoes maior que 0.");
            return;
          }
          int tamPop = Integer.parseInt(tamPopStr);
          final int numGeracoes = Integer.parseInt(numGeracoesStr);
          Object[] caractObjs = jListListaCars.getSelectedValues();
          String[] caracteristicas = new String[caractObjs.length];
          for (int i = 0; i < caractObjs.length; i++)
            caracteristicas[i] = (String)caractObjs[i];
          final GeraPopulacao gp;
          if (caracteristicas.length == 0)
            gp = new GeraPopulacao(tamPop, PrologInteraction.leCaracteristicas());
          else
            gp = new GeraPopulacao(tamPop, caracteristicas);
          jTextAreaOMelhorJogador.requestFocus();
          jButtonEncontrarMelhorJogador.setEnabled(false);
          jButtonCarregaPop.setEnabled(false);
          processa(gp, numGeracoes);
        }
      });
    }
    return jButtonEncontrarMelhorJogador;
  }

  private void processa(final GeraPopulacao gp, final int numGeracoes){
    SwingWorker<Void,Void> worker = new SwingWorker<Void, Void>() {
      @Override
      public Void doInBackground() {
        String melhorJogador = "";  
        int i = gp.getGeracaoCorrente();
        while (i < numGeracoes){
          melhorJogador = gp.interage();
          String vencedor = "Geracao " + (i+1) + ":\nO vencedor foi:\n" + melhorJogador + "\n\n";
          sb.append(vencedor);
          jTextAreaOMelhorJogador.append(vencedor);
          escreveFichVencedores(sb.toString());
          escreveFichVencedoresLegivel(sb.toString());
          i++;
        }
        jButtonEncontrarMelhorJogador.setEnabled(true);
        jButtonCarregaPop.setEnabled(true);
        jTextAreaOMelhorJogador.append("O melhor jogador ao fim de " + numGeracoes + 
            ((numGeracoes == 1) ? " geração" :  " gerações") + " é o jogador:\n" + melhorJogador);
        return null;
      }

    };
    worker.execute();
  }

  private void escreveFichVencedores(String str){
    try {
      FileOutputStream fos = new FileOutputStream("../vencedores.bin");
      ObjectOutputStream oos = new ObjectOutputStream(fos);
      oos.writeObject(str);
      oos.close();
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private void escreveFichVencedoresLegivel(String str){
    BufferedWriter bf = null;
    try {
      bf = new BufferedWriter(new FileWriter("../vencedores.txt"));
      bf.append(str);
      bf.close();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  /**
   * This method initializes jButtonLimparDados	
   * 	
   * @return javax.swing.JButton	
   */
  private JButton getJButtonLimparDados () {
    if (jButtonLimparDados == null) {
      jButtonLimparDados = new JButton();
      jButtonLimparDados.setBounds(new Rectangle(14, 390, 144, 24));
      jButtonLimparDados.setText("Reset dos dados");
      jButtonLimparDados.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed (java.awt.event.ActionEvent e) {
          jTextFieldTamPop.setText("");
          jTextFieldNumGeracoes.setText("");
          jTextFieldTamPop.requestFocus();
        }
      });
    }
    return jButtonLimparDados;
  }

  /**
   * This method initializes jButtonSair	
   * 	
   * @return javax.swing.JButton	
   */
  private JButton getJButtonSair () {
    if (jButtonSair == null) {
      jButtonSair = new JButton();
      jButtonSair.setBounds(new Rectangle(16, 466, 75, 22));
      jButtonSair.setText("Sair");
      jButtonSair.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed (java.awt.event.ActionEvent e) {
          System.exit(0);
        }
      });
    }
    return jButtonSair;
  }

  /**
   * This method initializes jButtonAcercaDe	
   * 	
   * @return javax.swing.JButton	
   */
  private JButton getJButtonAcercaDe () {
    if (jButtonAcercaDe == null) {
      jButtonAcercaDe = new JButton();
      jButtonAcercaDe.setBounds(new Rectangle(15, 438, 100, 20));
      jButtonAcercaDe.setText("Acerca...");
      jButtonAcercaDe.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed (java.awt.event.ActionEvent e) {
          String message = "Programa desenvolvido pelo grupo IIA018 (Celso, Henrique e Sérgio), no ano lectivo 2008/2009 :)";
          JOptionPane.showMessageDialog(jContentPane, message, "Acerca de geneticos", JOptionPane.INFORMATION_MESSAGE);
        }
      });
    }
    return jButtonAcercaDe;
  }

  /**
   * This method initializes jTextAreaOMelhorJogador	
   * 	
   * @return javax.swing.JTextArea	
   */
  private JTextArea getJTextAreaOMelhorJogador () {
    if (jTextAreaOMelhorJogador == null) {
      jTextAreaOMelhorJogador = new JTextArea();
      jTextAreaOMelhorJogador.setEditable(false);
    }
    return jTextAreaOMelhorJogador;
  }

  /**
   * This method initializes jScrollPane	
   * 	
   * @return javax.swing.JScrollPane	
   */
  private JScrollPane getJScrollPane () {
    if (jScrollPane == null) {
      jScrollPane = new JScrollPane();
      jScrollPane.setBounds(new Rectangle(399, 134, 164, 289));
      jScrollPane.setViewportView(getJTextAreaOMelhorJogador());
    }
    return jScrollPane;
  }

  /**
   * This method initializes jScrollPaneListCars	
   * 	
   * @return javax.swing.JScrollPane	
   */
  private JScrollPane getJScrollPaneListCars () {
    if (jScrollPaneListCars == null) {
      jScrollPaneListCars = new JScrollPane();
      jScrollPaneListCars.setBounds(new Rectangle(102, 174, 137, 151));
      jScrollPaneListCars.setViewportView(getJListListaCars());
    }
    return jScrollPaneListCars;
  }

  /**
   * This method initializes jListListaCars	
   * 	
   * @return javax.swing.JList	
   */
  private JList getJListListaCars () {
    if (jListListaCars == null) {
      String[] arr = PrologInteraction.leCaracteristicas();
      jListListaCars = new JList(arr);
    }
    return jListListaCars;
  }

  /**
   * This method initializes jButtonCarregaPop	
   * 	
   * @return javax.swing.JButton	
   */
  private JButton getJButtonCarregaPop () {
    if (jButtonCarregaPop == null) {
      jButtonCarregaPop = new JButton();
      jButtonCarregaPop.setBounds(new Rectangle(13, 357, 153, 20));
      jButtonCarregaPop.setText("Carregar população");
      jButtonCarregaPop.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed (java.awt.event.ActionEvent e) {
          File parametros = new File("../parametros");
          File vencedores = new File("../vencedores.bin");
          File pesos = new File("../pesos.bin");
          if (!parametros.exists() || !vencedores.exists() || !pesos.exists()){
            JOptionPane.showMessageDialog(jContentPane,
            "Não existe nenhum jogo salvo previamente ou talvez tenha\n"+
            "apagado alguns ficheiros necessarios.\n" +
            "Carregue novamente no botão \"Encontrar melhor jogador\",\n" +
            "ou caso tenha os ficheiros salvos anteriormente copie-os para o\n" +
            "local apropriado");
            return;
          }
          String v = leFichVencedores();
          boolean flag = true;
          String ger = "";
          GeraPopulacao gp = new GeraPopulacao();
          int numGeracoesCorrente = gp.getGeracaoCorrente();
          int numGeracoes = 0;
          while (flag){
            try{
              ger = JOptionPane.showInputDialog(jContentPane, "Número de gerações a partir da" +
                  " primeira (número deverá ser maior que " + numGeracoesCorrente + ")");
              if (ger.equals(null))
                return;
              numGeracoes = Integer.parseInt(ger);   
              while (numGeracoes <= numGeracoesCorrente){
                ger = JOptionPane.showInputDialog(jContentPane, "O número de gerações tem de ser maior que " + numGeracoesCorrente);
                if (ger.equals(null))
                  return;
                numGeracoes = Integer.parseInt(ger);
              }
              flag = false;
            } catch (NumberFormatException ex){
              JOptionPane.showMessageDialog(jContentPane,
              "Introduza um valor numérico");
            }
          }
          jTextAreaOMelhorJogador.append(v);
          sb.append(v);
          jTextAreaOMelhorJogador.requestFocus();
          jButtonEncontrarMelhorJogador.setEnabled(false);
          jButtonCarregaPop.setEnabled(false);
          processa(gp, numGeracoes);
          
        }
      });
    }
    return jButtonCarregaPop;
  }

  private String leFichVencedores(){
    String venc = "";
    try {
      FileInputStream fis = new FileInputStream("../vencedores.bin");
      ObjectInputStream ois = new ObjectInputStream(fis);
      venc = (String)ois.readObject();
      ois.close();
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    } catch (IOException e) {
      e.printStackTrace();
    } catch (ClassNotFoundException e) {
      e.printStackTrace();
    }
    return venc;
  }
  /**
   * @param args
   */
  public static void main (String[] args) {
    PrologInteraction.leCaracteristicas();
    SwingUtilities.invokeLater(new Runnable() {
      public void run () {
        InterF thisClass = new InterF();
        thisClass.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        thisClass.setVisible(true);
      }
    });
  }

  /**
   * This is the default constructor
   */
  public InterF () {
    super();
    initialize();
  }

  /**
   * This method initializes this
   * 
   * @return void
   */
  private void initialize () {
    this.setSize(600, 600);
    this.setContentPane(getJContentPane());
    this.setTitle("JFrame");
  }

  /**
   * This method initializes jContentPane
   * 
   * @return javax.swing.JPanel
   */
  private JPanel getJContentPane () {
    if (jContentPane == null) {
      jLabelListaCars = new JLabel();
      jLabelListaCars.setBounds(new Rectangle(10, 154, 161, 16));
      jLabelListaCars.setText("Características pretendidas");
      jLabelOMelhorJogador = new JLabel();
      jLabelOMelhorJogador.setBounds(new Rectangle(287, 141, 101, 16));
      jLabelOMelhorJogador.setText("O melhor jogador");
      jLabelNumGeracoes = new JLabel();
      jLabelNumGeracoes.setBounds(new Rectangle(13, 45, 124, 16));
      jLabelNumGeracoes.setText("Número de gerações");
      jLabelTamPop = new JLabel();
      jLabelTamPop.setBounds(new Rectangle(8, 20, 138, 16));
      jLabelTamPop.setText("Tamanho da população");
      jContentPane = new JPanel();
      jContentPane.setLayout(null);
      jContentPane.add(getJTextFieldTamPop(), null);
      jContentPane.add(getJTextFieldNumGeracoes(), null);
      jContentPane.add(jLabelTamPop, null);
      jContentPane.add(jLabelNumGeracoes, null);
      jContentPane.add(jLabelOMelhorJogador, null);
      jContentPane.add(getJButtonEncontrarMelhorJogador(), null);
      jContentPane.add(getJButtonLimparDados(), null);
      jContentPane.add(getJButtonSair(), null);
      jContentPane.add(getJButtonAcercaDe(), null);
      jContentPane.add(getJScrollPane(), null);
      jContentPane.add(getJScrollPaneListCars(), null);
      jContentPane.add(jLabelListaCars, null);
      jContentPane.add(getJButtonCarregaPop(), null);
    }
    return jContentPane;
  }

}
