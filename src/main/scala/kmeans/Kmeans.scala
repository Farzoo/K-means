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
      this.clusters.foreach(c => {
        hasChanged = hasChanged || c.computeCentroid()
      })
      this.clusters.filterInPlace(_.size()>0)

    this.clusters.foreach(c => {
        c.computeClassCluster()
        c.computeIntraDistance()
        c.computeClusterError()
      }
    )


    return new KmeansModel(k, this.data, this.clusters.toArray, this.computeInterDistance(), this.computeQuality(), this.computeClusteringError())

  private[this] def computeInterDistance() : Double =
    if(this.clusters.length < 2) return 0.0d
    var sommeDistance : Double = 0
    for(i <- (0 until this.clusters.length-1)) {
      for(j <- (i+1 until this.clusters.length)) {
        sommeDistance += Math.pow(this.clusters(i).centroid.distance(this.clusters(j).centroid), 2)
      }
    }
    return (sommeDistance * 2) / (this.clusters.length * (this.clusters.length -1))

  /**
   *
   * @return
   */
  private[this] def computeQuality() : Double =
    //println(this.clusters.map(c => (c, c.computeIntraDistance())).filter(_._2.isNaN))
    var dbQuality : Double = this.clusters.map(ci => this.clusters.map(cj =>
                                if(cj != ci) (ci.intraDistance + cj.intraDistance) / ci.centroid.distance(cj.centroid) else 0.0d
                              ).max).sum / this.clusters.length
    return if (dbQuality == 0.0d) 0.0d else 1 / dbQuality


  private[this] def computeClusteringError() : Double =
    return this.clusters.map(c => c.computeClusterError()).sum / this.clusters.length
