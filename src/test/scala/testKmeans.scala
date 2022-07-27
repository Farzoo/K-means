import kmeans.{Cluster, Data, Exemple}

object testKmeans :
  def main(args : Array[String]) : Unit =
    var donnees : Data = new Data("iris.data", "irisAttributesNames.txt")
    donnees.getNormalizedData

    var c : Cluster = new Cluster("Cluster 1", donnees.getNormalizedData, donnees.nbAttributes, donnees.getNbClasses)

    var maListe : Seq[Int] = (0 until 3)
    println(maListe.map(x => maListe.map(y => (x, y))))





