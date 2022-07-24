package kmeans


/**
 * Gère un kmeans.Exemple.
 *
 * Un kmeans.Exemple est un kmeans.Individu associé à une classe
 * @example pour créer un kmeans.Exemple avec 4 attributs de type Double, associé à la classe numéro 2
 * {{{
 *  val ex = New kmeans.Exemple(4,2)
 * }}}
 * @constructor Initialise un kmeans.Exemple
 * @param _nbAttributes : le nombre d'attributs
 * @param _classNumber : numéro de la classe associée à l'kmeans.Exemple
 */

class Exemple(_nbAttributes : Int, 
              _classNumber : Int) extends Individu(_nbAttributes) :
  /**
   * @return le numéro de classe de l'kmeans.Exemple
   */
  def classNumber: Int = _classNumber

  /**
   *
   *  @return le String décrivant l'kmeans.Exemple
   */
  override def toString : String = super.toString + s" classe = ${this.classNumber}"
