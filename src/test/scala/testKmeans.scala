import kmeans.{Cluster, Data, Exemple, Kmeans, KmeansModel, Tabulator}
import breeze.plot
import breeze.plot.{Figure, Plot}

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object testKmeans :
  def main(args : Array[String]) : Unit =

    val nClusterings : Int = 20

    var kmeansAlgo : Kmeans = new Kmeans("iris.data", "irisAttributesNames.txt")

    var model : KmeansModel = null
    var kClusterings : Map[Int, Array[KmeansModel]] = new ListMap
    var kFinalClusterings : Map[Int, ArrayBuffer[KmeansModel]] = new ListMap

    for(k <- (2 until 13))
      kClusterings = kClusterings.updated(k, new Array[KmeansModel](nClusterings))
      for(c <- (0 until nClusterings))
        model = kmeansAlgo.clustering(k)
        kClusterings(k).update(c, model)
        if(!kFinalClusterings.contains(model.kFinal)) then kFinalClusterings = kFinalClusterings.updated(model.kFinal, new ArrayBuffer[KmeansModel]())
        kFinalClusterings(model.kFinal).append(model)
      model = kClusterings(k).maxBy(_.quality)
      plotClustering(model)

    var kmoy : List[List[String]] = kClusterings.toList.map(
      (k, moy) => List(k.toString(), (moy.map(_.quality).sum / moy.length).toString)
    )

    println(Tabulator.formatTable(List(List("k initial", "moy. qualité")).appendedAll(kmoy)))

    kmoy = kFinalClusterings.toList.sortBy(_._1).map(
      (k, moy) => List(k.toString(), (moy.map(_.quality).sum / moy.length).toString)
    )

    println(Tabulator.formatTable(List(List("k final", "moy. qualité")).appendedAll(kmoy)))


  def plotClustering(model: KmeansModel) : Unit =
    val f : Figure = new Figure(f"Kmeans ki=${model.kInitial} kf=${model.clusters.length} Q=${model.quality}", model.data.nbAttributes, model.data.nbAttributes)
    var n : Int = model.data.nbAttributes
    for(i <- (0 until n))
      for(j <- (0 until n))
          plotAttributeAgainstAttributeByCluster(f, (i*n + j), i, j, model)

  def plotAttributeAgainstAttributeByCluster(f : Figure, i : Int, a1: Int, a2: Int, model: KmeansModel) : Unit =
    val p: Plot = f.subplot(i)
    p.xlabel = model.data.getAttributesNames(a1)
    p.ylabel = model.data.getAttributesNames(a2)
    p.title = f"${p.xlabel} en fonction de ${p.ylabel}"

    // x contiendra les donneees de l'attribut a1 reparties par classe
    // y contiendra les donnees de l'attributs a2 reparties par classe
    val y: Array[ArrayBuffer[Double]] = Array.ofDim(model.clusters(0).nbCategories)
    val x: Array[ArrayBuffer[Double]] = Array.ofDim(model.clusters(0).nbCategories)

    for (c <- 0 until model.data.getNbClasses) {
      x(c) = new ArrayBuffer[Double]()
      y(c) = new ArrayBuffer[Double]()
    }

    // on distribue les valeurs des 2 attributes en fonction de leur classe
    for (c <- model.clusters) {
      for (i <- (0 until c.size())) {
        x(c.classCluster).append(c.get(i).get(a1))
        y(c.classCluster).append(c.get(i).get(a2))
      }
    }

    // fait un plot de chaque classe avec une couleur differente
    for (c <- 0 until model.clusters(0).nbCategories) {
      p.legend = true
      p += plot.plot(x(c), y(c), style = '.', colorcode = Data.colors(c),
        name = model.data.getClassesNames(c))
    }

    var centroidByClasse = model.clusters.map(cluster => (cluster.classCluster, cluster.centroid)).groupBy(_._1).map((n, v) => (n, v.map(_._2)))

    // On dessine les centroïds sur le graphique
    for ((classe, centroids) <- centroidByClasse)
      p.legend = true
      p += plot.plot(centroids.map(_.get(a1)), centroids.map(_.get(a2)), style = '+', colorcode = if (classe < Data.colors.length) Data.colors(classe) else "black", name = classe.toString)


