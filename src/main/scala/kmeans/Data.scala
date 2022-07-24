package kmeans

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
// import breeze.plot._

/**
 * Gère une matrice d'Exemples
 *
 *
 * @constructor Initialise une matrice d'Exemples vide
 * @example
 * {{{
 *   val donnees = new kmeans.Data
 * }}}
 */
class Data(fichierDonnees: String, fichierAttributs : String) :

	private val data 					    = new ArrayBuffer[Exemple]
	private val classesNames			= new ArrayBuffer[String]

	private var normalizedData    : Array[Exemple]       = _
	private var attributesNames   : Array[String]        = _
	private var nbDonnees	        : Int 		         	   = _
	private var nbAttributs	      : Int			        	   = _
	private var moyennes	        : Individu 	           = _
	private var mins		          : Individu	           = _
	private var maxs		          : Individu	           = _
	private var attributesValues  : Array[Array[Double]] = _
	private var nbClasses         : Int                  = 0

	this.loadData


			// accesseurs

	/**
	 *
	 * @return le nombre d'attributs des Exemples
	 */
	def nbAttributes        : Int    				       = this.nbAttributs

	/**
	 *
	 * @return le nombre d'Exemples (lignes) dans la matrice
	 */
	def nbData 			        : Int    	 		         = this.nbDonnees

	/**
	 *
	 * @return les données initiales
	 */
	def getData             : ArrayBuffer[Exemple] = this.data

	/**
	 *
	 * @return les données normalisées
	 */
	def getNormalizedData   : Array[Exemple]       = this.normalizedData

	/**
	 *
	 * @return un tableau contenant les noms des classes
	 */
	def getClassesNames     : ArrayBuffer[String]  = this.classesNames

	/**
	 *
	 * @return le nombre de classes
	 */
	def getNbClasses	      : Int		   			       = this.classesNames.size

	/**
	 *
	 * @return l'kmeans.Exemple situé à la ligne ''i'' de la matrice
	 */
	def get(i : Int) : Exemple = this.data(i)
	// ================================================ PLOTS ========================================================
	/**
	  * representation graphique des valeurs de tous les attributs
	  */
		/*
	def plotAttributesValues() : Unit =
	{

		val f = Figure("Attributs")

                        // pour chaque attribut
		for (j <- 0 until this.nbAttributs)
		{
			val p = f.subplot(this.nbAttributs, 1, j)

			p.xlabel = "indices"
			p.ylabel = "valeurs"
			p.title = this.attributesNames(j)

						// y contiendra les donneees de l'attribut reparties par classe
                        // y contiendra les indices des donnees
            val y : Array[ArrayBuffer[Double]] = Array.ofDim(this.nbClasses)
			val x : Array[ArrayBuffer[Double]] = Array.ofDim(this.nbClasses)

			for (c <- 0 until this.nbClasses)
			{
				x(c) = new ArrayBuffer[Double]()
				y(c) = new ArrayBuffer[Double]()
			}

                        // on distribue les valeurs dans y en fonction de la classe de
                        // l'exemple contenant l'attribut
			for (i <- this.attributesValues(j).indices)
			{
							// recupere la classe de la donnee i
				val c = this.normalizedData(i).classNumber
                x(c).append(i)
                y(c).append(this.attributesValues(j)(i))
			}

						// fait un plot de chaque classe avec une couleur differente
			for (c <- 0 until this.nbClasses)
			{
				if (j == this.nbAttributs -1)
                {
                    p.legend = true
                    p += plot(x(c), y(c), style = '+', colorcode = LearningData.colors(c),
                        name = this.classesNames(c))
                }
                else
                    p += plot(x(c), y(c), style = '+', colorcode = LearningData.colors(c))
			}
		}
	}
*/
    /**
      * representation graphique des valeurs de tous les attributs
      */
			/*
    def plotAttributeAgainstAttribute(a1 : Int, a2 : Int) : Unit =
    {
        val nomFigure = this.attributesNames(a2) + "/" + this.attributesNames(a1)
        val f = Figure(nomFigure)
        val p = f.subplot(0)

        p.xlabel = this.attributesNames(a1)
        p.ylabel = this.attributesNames(a2)
        p.title  = nomFigure

             // x contiendra les donneees de l'attribut a1 reparties par classe
             // y contiendra les donnees de l'attributs a2 reparties par classe
        val y : Array[ArrayBuffer[Double]] = Array.ofDim(this.nbClasses)
        val x : Array[ArrayBuffer[Double]] = Array.ofDim(this.nbClasses)

        for (c <- 0 until this.nbClasses)
        {
            x(c) = new ArrayBuffer[Double]()
            y(c) = new ArrayBuffer[Double]()
        }

        // on distribue les valeurs des 2 attributes en fonction de leur classe
        for (i <- 0 until this.nbDonnees)
        {
            // println("i = " + i)
            // recupere la classe de la donnee i
            val c = this.normalizedData(i).classNumber
            x(c).append(this.attributesValues(a1)(i))
            y(c).append(this.attributesValues(a2)(i))
        }
        

               // fait un plot de chaque classe avec une couleur differente
        for (c <- 0 until this.nbClasses)
        {
           p.legend = true
           p += plot(x(c), y(c), style = '+', colorcode = LearningData.colors(c),
                     name = this.classesNames(c))
        }
    }
    */

    // ============================================= ACCESSEURS ========================================================

	/**
	 * @return le numero de classe de nom ''nomClasse'', ou -1 si le nom de la classe
		* est inconnu
		*/

	def getClassNumber(nomClasse : String): Int =
		var i = 0

		while (i < this.classesNames.size && this.classesNames(i) != nomClasse)
			i += 1

		if (i < this.classesNames.size)
			return i

		return -1

	/**
	 * @return le nom de la classe de numero ''classNumber''
	 */
	def getClassName(classNumber : Int) : String = this.classesNames(classNumber)



// =========================================== LECTURE DES DONNEEES ====================================================

	/**
	  * recupere les Exemples du fichier fichierData, les normalise et les place dans la matrice.
	 * (les données originales et normalisées sont accessibles)
	  */
	def loadData: Unit =
		this.readData()
		this.initializeData()
		this.computeStats()
		this.normalizeData()
		this.computeAttributesNormalizedValues()
		// this.displayAttributesNormalizedValues()
		this.readAttributesNames

		// this.plotDataAttributes(2,3)
		// this.plotAttributesValues()
		// this.plotAttributeAgainstAttribute(2,3)


	/**
	  * lit les noms des attributs
	  */
	private def readAttributesNames: Unit =
		this.attributesNames = new Array[String](this.nbAttributs)

		val bufferedSource = Source.fromFile(this.fichierAttributs)

		var i = 0

		for (ligne <- bufferedSource.getLines)
			println(ligne)
			this.attributesNames(i) = ligne

			i += 1

		bufferedSource.close

	/**
		* lit les donnees depuis le fichier nomFichier
		*/
	private def readData() =
		val bufferedSource = Source.fromFile(this.fichierDonnees)

		var i = 0

		for (ligne <- bufferedSource.getLines)
			// la ligne est scindée en attributs
			// le dernier attribut est le classement

			val attributs = ligne.split(",")
			val a = attributs.length // nombre Donnees par ligne
			// a-1 = nombre attributs

			val className = attributs(a-1)

			var classNumber = this.getClassNumber(className)

			if (classNumber == -1)
				this.classesNames.append(className) 					// stocke le nom de sa classe
				classNumber = this.nbClasses
				this.nbClasses += 1

			val donnee = new Exemple(a-1, classNumber)

			// parcours des attributs
			for (j <- 0 until a - 1)
				donnee.set(j, attributs(j).toDouble)
				// print(donnee(j))

			this.data.append(donnee)

			i = i + 1

		bufferedSource.close

	/**
		* initiliaze les donnees
		*/
	private def initializeData(): Unit =
		this.nbDonnees      = this.data.length
		this.nbAttributs    = this.data(0).nbAttributes
		this.moyennes       = new Individu(this.nbAttributs)
		this.mins 		      = new Individu(this.nbAttributs)
		this.maxs 		      = new Individu(this.nbAttributs)
		this.normalizedData = new Array[Exemple](this.nbDonnees)


// ============================================= CALCULS SUR LES DONNEES ==============
	/**
	  *
	  */
	private def computeAttributesNormalizedValues() : Unit =
		this.attributesValues = Array.ofDim[Double](this.nbAttributs, this.nbDonnees)

		for (j <- 0 until this.nbAttributs ; i <- 0 until this.nbDonnees)
			this.attributesValues(j)(i) = this.normalizedData(i).get(j)


	/**
		* calcul les moyennes, valeur min et valeur max de chaque attribut
		*/
	private def computeStats(): Unit =
		// initialisation des valeurs statistiques
		for (j <- 0 until this.nbAttributs)
			this.moyennes.set(j,this.data(0).get(j))
			this.mins.set(j, this.data(0).get(j))
			this.maxs.set(j, this.data(0).get(j))

		// calcul des statistiques
		for (i <- 1 until this.nbDonnees)
			for (j <- 0 until this.nbAttributs)
				val attribut = this.data(i).get(j)

				this.moyennes.set(j, this.moyennes.get(j) + attribut)

				if (attribut < mins.get(j))
					this.mins.set(j, attribut)

				if (attribut > maxs.get(j))
					this.maxs.set(j, attribut)

		// calcul final des moyennes
		for (j <- 0 until this.nbAttributs)
			this.moyennes.set(j, this.moyennes.get(j) / this.nbDonnees)


	/**
		* normalise les données entre 0 et 1
		*/
	private def normalizeData(): Unit =
		for (i <- 0 until this.nbDonnees)
			this.normalizedData(i) = new Exemple(this.nbAttributs, this.data(i).classNumber)

			for (j <- 0 until this.nbAttributs)
				this.normalizedData(i).set(j, (this.data(i).get(j) - this.mins.get(j)) /
											                 (this.maxs.get(j) - this.mins.get(j)))


// ======================================= AFFICHAGE DES DONNEES ======================================================

	/**
	 * affiche les valeurs normalisees de l'attribut numero a
	  */
	def displayAttributesNormalizedValues(a : Int) : Unit =
			for (i <- 0 until this.nbDonnees)
				print(f"${this.attributesValues(a)(i)} ")
			println

	/**
	  *  affiche les valeurs normalisees des attributs
	  */
	def displayAttributesNormalizedValues() : Unit =
		for (j <- 0 until this.nbAttributs)
			for (i <- 0 until this.nbDonnees)
				print(f"${this.attributesValues(j)(i)} ")
			println


	/**
		* affiche les donnees originales
		*/
	def displayData(): Unit =
		for (i <- 0 until this.nbDonnees)
			println(this.data(i))
			println(s" ${this.getClassName(this.data(i).classNumber)}")


	/**
		* affiche les donnees normalisees
		*/
	def displayNormalizedData(): Unit =
		for (i <- 0 until this.nbDonnees)
			print(this.normalizedData(i))
			println(s" ${this.getClassName(this.normalizedData(i).classNumber)}")

	/**
	  * affiche les noms des attributs
	  */
	def displayAttributesNames(): Unit =
		for (name <- this.attributesNames)
			println(s" $name")

	/**
		* affiche les donnees lues
		*/
	def displayAllData(): Unit =
		for (i <- 0 until this.nbDonnees)
			print(f"$i : ")
			for (j <- 0 until this.nbAttributs)
				print(f"(${this.data(i).get(j)}%.2f;${this.normalizedData(i).get(j)}%.2f) ")
			println(s" ${this.getClassName(this.normalizedData(i).classNumber)} (${this.normalizedData(i).classNumber})")

	/**
	 * affiche l'ensemble les noms des classes
	 */
	def displayClasses() : Unit =
		println("classes : ")
		for (name <- this.classesNames)
			println(name)


	/**
		* affiche les statistiques associées aux Exemples
		*/
	def displayStats(): Unit =
		for (j <- 0 until this.nbAttributs)
			println(f"attribut $j : moyenne = ${this.moyennes.get(j)}%.3f, min = ${this.mins.get(j)}%.3f, " +
				                              f"max = ${this.maxs.get(j)}%.3f")

// fin class LearningData


object Data :

	private val colors = Array[String]("green", "red", "blue", "black",  "yellow")
	private val styles = Array[Char]('+', '.')
