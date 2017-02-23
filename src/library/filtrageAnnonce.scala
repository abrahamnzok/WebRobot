package library

class filtrageAnnonce extends FiltrageURLs {




	def valAtt(l : List [(String , String)]) : List[String] = {
			l match {
			case Nil => Nil
			case t::q => if (t._1 == "href")  valAtt(q).+:(t._2) else valAtt(q)
			}
	}

	def valChild ( l : List[Html]) : List [String] = {
			l match {
			case Nil => Nil 
			case t::q => filtragePrimaire(t) ++ (valChild(q))                      
			}
	}


	def  filtragePrimaire (h : Html) : List[String]={
	  
			h match {
			case Text(content : String) => Nil 
			case Tag (nom : String , attribute : List[(String,String)] , children : List[Html]) => 
			valAtt(attribute) ++ (valChild(children))

			}
			
	}
	
	
	def retireLien (l : List [String]) : List [String] ={
	  l match {
	    case Nil => Nil 
	    case t::q => if ((t).startsWith("//www.leboncoin.fr/")) List ("http:"+t) ++ retireLien(q) 
	    else  retireLien(q)
     }
     
	}
	
	def filtreAnnonce (h : Html) : List [String] = {
	  retireLien(filtragePrimaire(h))
	}



}