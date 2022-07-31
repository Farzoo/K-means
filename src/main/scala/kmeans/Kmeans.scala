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

  def clustering(k : Int) : KmeansModel =
    this.clusters = new ArrayBuffer[Cluster](k)
    var cluster : Cluster = null

    for(i: Int <- 0 until k)
      cluster = new Cluster(f"cluster(${i.toString})", this.data.getNormalizedData, this.data.nbAttributes, this.data.getNbClasses)
      cluster.centroid = Individu.generateRandomIndividu(cluster.nbAttributes, new Random(System.currentTimeMillis() + i))
      this.clusters.append(cluster)

    var hasChanged : Boolean = true

    while(hasChanged)
      hasChanged = false
      this.clusters.foreach(_.empty())
      this.data.getNormalizedData.zipWithIndex.foreach(
        (e, i) => this.clusters.map(
                    c => (c, c.centroid.distance(e))
                  ).minBy(_._2)._1.add(i)
      )
      this.clusters.filterInPlace(_.size()>0)
      this.clusters.foreach(c => {
        hasChanged = hasChanged || c.computeCentroid()
        c.computeClassCluster()
        c.computeIntraDistance()
        c.computeClusterError()
      })

    return new KmeansModel(k, this.data, this.clusters.toArray, this.computeInterDistance(), this.computeQuality())

  private[this] def computeInterDistance() : Double =
    var sommeDistance : Double = 0
    for(i <- (0 until this.clusters.length-1)) {
      for(j <- (i+1 until this.clusters.length)) {
        sommeDistance += this.clusters(i).centroid.distance(this.clusters(j).centroid)
      }
    }

    return (sommeDistance * 2) / (this.clusters.length * (this.clusters.length -1))

  private[this] def computeQuality() : Double =
    return this.computeInterDistance() / (this.clusters.map(c => c.computeIntraDistance()).sum / this.clusters.length)

