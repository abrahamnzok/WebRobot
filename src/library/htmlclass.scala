package library.H

import library.{Text, Tag, Html, Html2String}

class htmlclass extends Html2String{
  
  def process(h:Html):String = {
    // valeur qui nous permettra de stocker notre résultat
    var str : String = ""
    // si h est un des cas suivants
    h match {
        // un tag, on reforme le contenue dans une string afin d'avoir le corps d'un code html
        /* n nom de la balise
        * a contient les attributs de la balise
        * l contient les enfants
         */
      case Tag(n, a, l) => str = str + "<" + n + attributsToString(a) + ">" + "\t"+ childrenToString(l)  +  "</" + n + ">"
        // un text, on le rajoute à la string
      case Text(c) =>  str = str + c
    }
    str // on retourne la chaîne de caractères
  }

  def attributsToString(l : List[(String, String)]):String = {
    // la variable dans laquelle on stocke le corps de l'attribut
    var str : String = ""
    // pour chaque bout de l'attribut
    for(a <- l)
      // on prendra la premiere partie de la liste qui contiendra une référence à quelque chose et on ajoutera à la fin la deuxième partie du corps de l'attribut
      str = str + " " + a._1  + "=" + "\"" + a._2 + "\""
    // on retourne la valeur de str
    str
  }

  def childrenToString(c : List[Html]) : String = {
    // variable dans laquelle on stocke le corps de c
    var str = ""
    // pour chaque page html qui se trouve dans la liste c
    for (html <- c)
      // on applique recursivement process à chaque page et on stoke le resultat dans str
      str = str + process(html)
    // on retourne str et on l'utilisera dans process
    str
  }

}
