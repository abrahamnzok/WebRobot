package library

object FiltreHtml extends FiltrageHtml {

	/**
	 * La fonction *chercheList* rend 'true' si la chaine de caractere est presente dans la liste
	 *  de documents Html
	 * @param l une liste de documents Html
	 * @param m une chaine de caractère
	 * @return Vrai si la chaine de caractère est dans la liste de documents Html
	 */

	def chercheList (l:List[Html],m:String):Boolean={ // regarde dans les children (List) d'un HTML  
		/*	var u = false
					for(i <- l) {
						if(cherche(i,m)) 
							u = true
					}
			return u*/

					l match{
				  case Nil => false
					case x :: cdr => cherche(x,m) || chercheList(cdr,m);		
			}
	}

	/**
	 * La fonction *cherche* rend 'true' si la chaine de caractere est presente
	 *  dans le document Html
	 * @param h un document Html
	 * @param m une chaine de caractère
	 * @return Vrai si la chaine de caractère est dans le document Html
	 */

	def cherche (h:Html,m:String):Boolean={
			h match{
			  case Text(s) => s.contains(m)
			  case Tag(n,a,h1) => chercheList(h1,m)
			  case _ => false
			}
	}

	/**
	 * La fonction *filtreHtml* rend 'true' si l'expression est presente dans le document Html
	 * @param h un document Html
	 * @param e une expression
	 * @return Vrai si l'expression est presente dans le document Html
	 */

	def filtreHtml(h:Html,e:Expression):Boolean={

			e match{   
			  case Word(e1) => cherche(h,e1)
			  case And(e2,e3) => filtreHtml(h,e2) && filtreHtml(h,e3)
			  case Or(e4,e5) => filtreHtml(h,e4) || filtreHtml(h,e5)
			  case _ => false
			}					
	}	
}  