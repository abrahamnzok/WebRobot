package library


class Url (filtrageURLsImpl: FiltrageURLs, filtrageHtmlImpl: FiltrageHtml) extends AnalysePage {

	val filtrageURLs = filtrageURLsImpl;
	val filtreurHTML = filtrageHtmlImpl;

	/** A partir d'une URL de requête sur le bon coin et d'une expression exp, 
    retourne de pages issues de la requête et satisfaisant l'expression.

    @param url l'URL de la requête sur le bon coin 
    @param exp l'expression à vérifier sur les pages trouvées
    @return la liste des couples (titre,ref) où ref est l'URL d'une page
            satisfaisant l'expression et titre est son titre. 
	 */
	def resultats(url:String,exp:Expression):List[(String,String)] = {
			val page:Html = recuperPageHtml(url)

					val listeURL:List[String] = extraireListeURL(page)
					
					val ensemblePage:List[Html] = extrairePageDepuisURL(listeURL)

					val couplesUrlDoc = construireListeCouples(exp, listeURL, ensemblePage)

					val coupleTitreUrl = construireListeCouplesTitreURL(couplesUrlDoc)

					return coupleTitreUrl;
	}

	private def recuperPageHtml(url:String):Html = {
			UrlProcessor.fetch(url)
	}

	private def extraireListeURL(page:Html):List[String] = {
			//supprimerDoublon(filtrageURLs.filtreAnnonce(page))
	  filtrageURLs.filtreAnnonce(page)
	}

	private def supprimerDoublon(urls:List[String]):List[String] = {
	    //A chaque fois, on regarde si notre element de tête "e" est aussi present dans la liste urls privée de son premier element.
	    //Si c'est le cas, on l'ignore car c'est un doublon. supprimerDoublon(r) nous permet d'analyser recursivement reste de la liste, tant que le paramètre n'est pas une liste vide.
			urls match {
			  case Nil => List() 
			  case e::r => if (r.contains(e)) { return supprimerDoublon(r) } else { return e::supprimerDoublon(r) } 
			}
	}

	private def extrairePageDepuisURL(urls:List[String]):List[Html] = {
			
		urls match {
			  case Nil => List()
			  case e::r => recuperPageHtml(e)::extrairePageDepuisURL(r)
			}
	}

	/**
	 * 
	 * Construit une liste de couple (URL, page Html). Dans chaque couple la page Html est la page vers laquel pointe l'URL.
	 * 
	 * Un couple URL/PAGE est pris en compte uniquement si la page Html corresponde à la recherche utilisateur.
	 *  
	 * @param condition Une expression de recherche (exemple: abc OR bcd) 
	 * @param listeURL Une liste d'URL qui correspondent aux pages de ensemblePage
	 * @param ensemblePage Les pages dans lequelles on effectue recherche de texte qui correspond à notre expression
	 * @return
	 */
	private def construireListeCouples(condition:Expression, listeURL:List[String], ensemblePage:List[Html]):List[(String, Html)] = {
	    
	    //Nous exigons que les deux listes données aient la même taille. Le warning emit par le compilateur peut être ignoré, si l'on tente de violer la condition cela sera détecté pendant les tests.
	    //Cela ne devrait pas arriver, car cette méthode n'etant pas visible depuis l'exterieur, l'on maitrise le fait d'être sûr de toujours respecter cette condition.
			require(listeURL.length == ensemblePage.length)
			
			(listeURL, ensemblePage) match {
			  case (Nil, Nil) => List()
			  case (e1::r1, e2::r2) =>  println(condition)
			   println(filtreurHTML.filtreHtml(e2, condition))   
			    if (filtreurHTML.filtreHtml(e2, condition)) {//Si le contenu de la page contient ce que l'utilisateur cherche...           
			        (e1, e2)::construireListeCouples(condition, r1, r2)} //... alors on créer le couple (URL,PAGE HTML).
			                            else { 
			                              construireListeCouples(condition, r1, r2) //Si la page actuelle ne contient pas ce que recherche l'utilisateur, on se contente de l'ignorer et l'on passe à la suite.
			                            }
			}
	}

	private def construireListeCouplesTitreURL(couplesURLtoDoc:List[(String, Html)]):List[(String, String)] = {

	    couplesURLtoDoc match {
	      case Nil => List() //Plus rien à extraire, on peut stopper la recursivité.
	      case e::r => (extraireTitreDepuisPage(e._2), e._1)::construireListeCouplesTitreURL(r) //On extrait le titre depuis le code source, il deviens alors un element de la liste de couple à constuire.
	    }
	}


	//Pour extraire notre titre, on vérifie si la balise (Tag) passée en paramètre est la balise title recherché.
	//Cela consiste à regarder si la balise de notre page correspond à ce que l'on recherche, puis si ce n'est pas le cas, de chercher dans toute les sous-balises qu'elle contient.
	//Puisque Tag est un type algébrique, on peut extraire la liste des sous-balises facilement. 
	//Il faut avoir recours à la double recursivité entre extraireTitreDepuisPage et extraireTitreDepuisListDePage car une balise peut contenir plusieurs sous balise.
	//On applique donc extraireTitreDepuisPage pour extraire la liste des sous-balises puis extraireTitreDepuisListDePage pour parcourire tout les sous-balises.
	//Pour chacune de ses sous-balises on réappelle extraireTitreDepuisPage pour extraire la listes des sous-sous-balise et ainsi on descend de plus en plus profond dans la structure imbriqué de balise html jusqu'a trouver ce qui nous interresse.
	//L'interet de cette méthode est que l'on a pas besoin de savoir exactement pù se situe la balise recherchée.
	private def extraireTitreDepuisPage(page:Html):String = {
			page match {
				//Si la balise donné contient du texte et que le nom de cette balise est "title", nous avons alors trouvé notre titre.
		  	case Tag(name,_,List(Text(s))) => if (name.toLowerCase() == "title") { return s } else {return "Sans titre"} //Si la structure donnée n'est composée que d'une balise et que cette balise est le titre, on l'extrait. 
			  case Tag(_, _, e::r) => return extraireTitreDepuisListDePage(e::r) //Si la premiere balise de la structure n'est pas le titre, on extrait la liste des sous-balises qu'elle contient et on y recherche notre balise titre.
			  case _ => return " " //Si la page ne contient pas de balise ou si elle contient une balise qui ne convient pas et qui n'a pas de sous-balise à analyser.
			}

	}

	private def extraireTitreDepuisListDePage(pages:List[Html]):String = {

			pages match {
				//Si dans le 1er element de notre liste de Html (e) on ne trouve pas ce que l'on cherche (e == null), alors on appelle recursivement cette méthode avec en paramètre notre liste privée de ce 1er element.
				//La récursivité s'arrête quand la liste donées est vide ou que l'on ai trouvé notre titre.
			  case e::r => if(extraireTitreDepuisPage(e) != " ") {return extraireTitreDepuisPage(e)} else { return extraireTitreDepuisListDePage(r)}
	      case Nil => return "Sans titre" //Si la liste est vide, on considère qu'il n'y a pas de titre: on renvoi un titre par défaut.
			}
	}

}