package kmeans

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Kmeans(fichierDonnees: String, fichierAttributs : String) :

  private[this] val _data : Data = new Data(fichierDonnees, fichierAttributs)
  private[this] var _clusters : ArrayBuffer[Cluster] = _

  private[this] def data : Data = this._data

  private[this] def clusters : ArrayBuffer[Cluster] = this._clusters
  private[this] def clusters_=(value: ArrayBuffer[Cluster]) : Unit =
    this._clusters = value

  def clustering(k : Int) : Double =
    this.clusters = new ArrayBuffer[Cluster](k)
    var cluster : Cluster = null

    for(i: Int <- 0 until k)
      cluster = new Cluster(f"cluster(${i.toString})", this.data.getNormalizedData, this.data.nbAttributes, this.data.getNbClasses)
      cluster.centroid = Individu.generateRandomIndividu(cluster.nbAttributes, new Random(System.currentTimeMillis() + i))

    var hasChanged : Boolean = true

    while(hasChanged)
      hasChanged = false
      this.data.getNormalizedData.zipWithIndex.foreach(
        (e, i) => this.clusters.map(
                    c => (c, c.centroid.distance(e))
                  ).minBy(_._2)._1.add(i)
      )
      this.clusters.foreach(c => hasChanged = hasChanged || c.computeCentroid())

    return this.computeInterDistance() / (this.clusters.map(c => c.computeIntraDistance()).sum / this.clusters.size)

  private[this] def computeInterDistance() : Double =
    return 0

  private[this] def computeQuality() : Double =
    return 0

