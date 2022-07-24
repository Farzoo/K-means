import kmeans.Data

object testKmeans :
  def main(args : Array[String]) : Unit =
    var donnees : Data = new Data("iris.data", "irisAttributesNames.txt")
    donnees.displayAllData()



