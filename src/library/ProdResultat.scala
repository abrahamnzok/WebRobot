package library

object ProdResultat extends ProductionResultat{
  def resultat2html(l:List[(String,String)]):Html={
    
    val res = print ( l )
    
    return Tag("html",List(),
                   List(Tag("head",List(),
                            List(Tag("meta",List(("content","text/html; charset=iso-8859-1")),List()),
                            		Tag("title",List(),List(Text("Liste des resultats"))))),
                        Tag("body",List(), List(
                           Tag ( "center", List(),  res)))))
}
  

  def print ( l:List[(String,String)]):List[Html]={
    var res = List[Html]()

    for ( (t1,t2) <- l ) {
      res = res :+ Tag("a", List(("href",t2)), List(Text(t1)))
    }
    
    return res
    
  }
      

    
    //    var liste = l
//    
//		liste match {
//		  case Nil => return res
//		  
//		  case ((t1,t2)::q)=> res = res :+ Tag("a", List(("href",t2)), List(Text(t1)))
//
//    }
  
}