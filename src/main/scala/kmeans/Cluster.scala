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
   */
  def computeCentroid() : Unit =
    this.centroid = new Individu(this.nbAttributes)
    (0 until this.nbAttributes).foreach(
      i => this.centroid.set(i, this.contents.map(e => e.get(i)).sum / this.contents.length)
    )

  /**
   * Calcul la classe majoritaire parmi les exemples du cluster
   */
  def computeClassCluster() : Unit =
    this.classCluster = this.contents.groupBy(_.classNumber).map(
                        (classNumber, exemples) => (classNumber, exemples.length)
                      ).maxBy(_._2)._2

  /**
   * Calcul le proportion d'exemples du cluster qui ne correspondent pas à sa classe
   */
  def computeClusterError() : Unit =
    // byIsClassNumber = le nombre d'Exemples qui ont pour classe this.classCluster et le nombre d'Exemples qui n'ont pas pour classe this.classCluster
    val byIsClassNumber : Map[Boolean, Int] = this.contents.groupBy(_.classNumber).map(
                                              (classNumber, exemples) => (classNumber, exemples.length)
                                            ).groupBy(_._1==this.classCluster).map(
                                              (b, e) => (b, e.size)
                                            )
    this.erreur = byIsClassNumber(false) / byIsClassNumber(false) + byIsClassNumber(false)

  /**
   * Calcul la distance moyenne entre les points du cluster
   * @return la distance moyenne entre les points du cluster
   */
  def computeIntraDistance() : Double =
    val distanceValues : Array[Double] = new Array[Double](this.contents.length)
    this.contents.indices.reduce(
      (i, j) => {
        Math.pow(this.get(i).distance(this.get(j)), 2)
        return j
      }
    )
    this.intraDistance = distanceValues.sum / this.contents.length
    return this.intraDistance

  override def toString : String = return ""




