package kmeans

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer

/**
 * Gère un cluster
 *
 * @param _name Le nom du cluster
 * @param _exemples l'ensemble des exemples
 * @param _nbAttributes le nombre d'attributs des exemples
 * @param _nbCategories le nombre de catégories différentes des exemples
 */
class Cluster(
               private[this] val _name : String,
               private[this] val _exemples : Array[Exemple],
               private[this] val _nbAttributes : Int,
               private[this] val _nbCategories : Int
             ) :


  private[this] val _contents : ArrayBuffer[Exemple] = new ArrayBuffer[Exemple](this._exemples.length)
  private[this] var _centroid : Individu = _
  private[this] var _classCluster : Int = _
  private[this] var _erreur : Double = _
  private[this] var _intraDistance : Double = _

  def name : String = this._name
  def exemples : Array[Exemple] = this._exemples
  def nbAttributes : Int = this._nbAttributes
  def nbCategories : Int = this._nbCategories

  /**
   * Contient les exemples affectés au cluster
   * @return
   */
  private[this] def contents : ArrayBuffer[Exemple] = this._contents

  def centroid : Individu = this._centroid
  def centroid_=(c : Individu) : Unit = this._centroid = c

  def classCluster : Int = this._classCluster
  private[this] def classCluster_=(value : Int) : Unit =
    this._classCluster = value

  def erreur : Double = this._erreur
  private[this] def erreur_=(value : Double) : Unit =
    this._erreur = value

  def intraDistance : Double = this._intraDistance
  private[this] def intraDistance_=(value : Double) : Unit =
    this._intraDistance = value

  def add(i : Int) : Unit =
    if(!this.contents.contains(this.exemples(i))) then
      this.contents.append(this.exemples(i))

  def empty() : Unit =
    this.contents.clear()

  def get(i : Int) : Exemple =
    this.contents(i)

  def size() : Int =
    this._contents.length

  /**
   * Calcule le centroid en fonction des Exemples affectés au cluster
   * La valeur des attributs du centroid est la moyenne de chaque attribut de tous les Exemples affectés au cluster
   * @return true si le centre a changé
   */
  def computeCentroid() : Boolean =
    var oldCentroid : Individu = this.centroid
    this.centroid = new Individu(this.nbAttributes)
    (0 until this.nbAttributes).foreach(
      i => this.centroid.set(i, this.contents.map(e => e.get(i)).sum / this.contents.length)
    )
    return oldCentroid == null || this.centroid.distance(oldCentroid) > 0.01

  /**
   * Calcul la classe majoritaire parmi les exemples du cluster
   */
  def computeClassCluster() : Int =
    this.classCluster = this.contents.groupBy(_.classNumber).map(
                        (classNumber, exemples) => (classNumber, exemples.length)
                      ).maxBy(_._2)._1
    return this.classCluster

  /**
   * Calcul le proportion d'exemples du cluster qui ne correspondent pas à sa classe
   */
  def computeClusterError() : Double =
    // byIsClassNumber = le nombre d'Exemples qui ont pour classe this.classCluster et le nombre d'Exemples qui n'ont pas pour classe this.classCluster
    val byIsClassNumber : Map[Boolean, Int] = this.contents.groupBy(_.classNumber).map(
                                              (classNumber, exemples) => (classNumber, exemples.length)
                                            ).groupBy(_._1==this.classCluster).map(
                                              (b, e) => (b, e.size)
                                            )
    var nbOk : Int = if(byIsClassNumber.contains(true)) byIsClassNumber(true) else 0
    var nbKO : Int = if(byIsClassNumber.contains(false)) byIsClassNumber(false) else 0
    this.erreur = nbKO / (nbOk + nbKO)
    return this.erreur

  /**
   * Calcul la distance moyenne entre les points du cluster
   * @return la distance moyenne entre les points du cluster
   */
  def computeIntraDistance() : Double =
    if(this.contents.length < 2) return 0d

    val distanceValues : ArrayBuffer[Double] = this.contents.map(e => Math.pow(this.centroid.distance(e), 2))

    this.intraDistance = Math.pow(distanceValues.sum / distanceValues.length, 1/2)

    return this.intraDistance

  override def toString : String = return f"${this.name} -> centroid=${this.centroid} | contents=${this.contents}"




