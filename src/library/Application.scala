package library

import java.io.FileWriter
import library.H.htmlclass

object Application extends App {
  
/*La fonction construire prendre au parametre une expression et elle nous retourne une liste des liens et chaque lien
 * contient un des mot dans l'expression on a mis dans le parametre sauf (and,or)*/

  def construire(l:Expression):List[String]={
    var y = List[String]()
    for ((s:String) <- consListWord(l)){
    y = ("http://www.leboncoin.fr/annonces/offres/?f=a&th=1&q=" + s) :: y
    }
    y
  }
  
  /*La fonction consListWord prendre au parametre une expression et elle nous retourne une liste des mots de l'expression 
   * sauf (and,or)*/
  
  def consListWord(l:Expression): List[String]={
      l match{
      case Word(r:String) => List(r)
      case And(s:Expression,a:Expression) => consListWord(s) ++ consListWord(a)
      case Or(z:Expression,p:Expression) => consListWord(z) ++ consListWord(p)  
    }
  }
  
  /*J'ai ajouté la fonction consListExp pour verifier qu'il n y a pas une erreur dans les fonctions precedentes
   */
  
//  def consListExp(l:Expression): List[Expression]={
//          l match{
//      case Word(r:String) => List(Word(r))
//      case And(s:Expression,a:Expression) => consListExp(s) ++ consListExp(a)
//      case Or(z:Expression,p:Expression) => consListExp(z) ++ consListExp(p)  
//    }
//  }

  def ecritDsUnFichier(s:String): Unit={
    val file = new FileWriter("résultat.html")
    try{
      file.write(s)
    }finally file.close()
  }
  
    /*La fonction app lance l'application et retourne rien mais affiche le mot "done" quand l'application se termine*/
  
  def app(ap:AnalysePage,pr:ProductionResultat,h2s:Html2String): Unit={
    var requete = ExpressionParser.readExp
    var l = List[(String,String)]()
    for ((s:String) <- construire(requete)){
         l = l ++ ap.resultats(s, requete)
     }
    var h = pr.resultat2html(l)
    var s = h2s.process(h)
    ecritDsUnFichier(s)
    println("done")
  }
  
  var filtrageurl = new filtrageAnnonce
  var ap = new Url(filtrageurl,FiltreHtml)
  var pr = ProdResultat
  var h2s = new htmlclass
  app(ap, pr, h2s)
  
}