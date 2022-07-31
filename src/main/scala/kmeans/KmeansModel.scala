package kmeans

class KmeansModel(
                 val kInitial : Int,
                 val data : Data,
                 val clusters : Array[Cluster],
                 val interDistance : Double,
                 val quality : Double
                 )
