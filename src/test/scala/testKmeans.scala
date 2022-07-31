import kmeans.{Cluster, Data, Exemple, Kmeans, KmeansModel}
import breeze.plot
import breeze.plot.{Figure, Plot}

import scala.collection.mutable.ArrayBuffer

object testKmeans :
  def main(args : Array[String]) : Unit =
    var donnees : Data = new Data("iris.data", "irisAttributesNames.txt")
    donnees.getNormalizedData

    donnees.plotAttributesValues()

    var c : Cluster = new Cluster("Cluster 1", donnees.getNormalizedData, donnees.nbAttributes, donnees.getNbClasses)

    c.add(0)
    c.add(1)
    c.add(2)

    println(c.computeCentroid())

    c.add(4)

    println(c.computeCentroid())
    println(c.computeCentroid())


    var kmeansAlgo : Kmeans = new Kmeans("iris.data", "irisAttributesNames.txt")
    val f = new Figure("K-means", 4, 2)
    var model : KmeansModel = null
    for(k <- (2 until 10))
      model = kmeansAlgo.clustering(k)
      println(f"Avec k=${model.clusters.length} => ${model.quality} ${model.clusters.map(_.classCluster).mkString("Array(", ", ", ")")}")
      plotAttributeAgainstAttributeByCluster(f, k-2, 1, 3, model)

  def plotAttributeAgainstAttributeByCluster(f : Figure, i : Int, a1: Int, a2: Int, model: KmeansModel) : Unit =
    val p : Plot = f.subplot(i)
    p.xlabel = model.data.getAttributesNames(a1)
    p.ylabel = model.data.getAttributesNames(a2)
    p.title = f"k=${model.kInitial} initiaux"

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
      for(i <- (0 until c.size())) {
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
      p += plot.plot(centroids.map(_.get(a1)), centroids.map(_.get(a2)), style = '+', colorcode = if(classe < Data.colors.length) Data.colors(classe) else "red", name = classe.toString)



