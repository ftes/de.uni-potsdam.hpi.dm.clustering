import sun.org.mozilla.javascript.ast.Yield

object Clustering extends App {
  type Item = Vector[Int]

  class Cluster(children: Array[Cluster]) {
    def items: Array[Item] = (children map { _.items })./:[Array[Item]](Array()) { _ ++ _ }
    def printTree: Unit = print(0, true)
    protected def printTree(level: Int, whitespace: Boolean): Unit = {
      if (whitespace) printf(" " * (level - 1))
      printf("-")
      if (children != null) for (i <- 0 until children.size) children(i).printTree(level, i == 0)
    }
    def printItems = { printItemsInternal; println }
    protected def printItemsInternal: Unit = children foreach { _.printItemsInternal }
  }
  class LeafCluster(item: Item) extends Cluster(null) {
    override def items = Array(item)
    override protected def printTree(level: Int, whitespace: Boolean) = {
      super.printTree(level, whitespace)
      println(item)
    }
    override def printItemsInternal = print(item + " ")
  }

  class MyTuple2(_1: Int, _2: Int) {
    def |-| = (_1 - _2).abs
  }

  implicit def tuple2MyTuple(tuple: (Int, Int)) = new MyTuple2(tuple._1, tuple._2)
  def rowByRow[T](a: Vector[T], b: Vector[T]) = for (i <- 0 until a.size) yield (a(i), b(i))
  def pairwise[T](a: Iterable[T], b: Iterable[T]) = for (x <- a; y <- b) yield (x, y)
  def pairwise[T](coll: Seq[T]) = for (x <- coll; y <- coll.drop(coll.indexOf(x)); if (x != y)) yield (x, y)
  def |-|(pair: (Int, Int)) = (pair._1 - pair._2).abs

  def manhattan(a: Item, b: Item) = (rowByRow(a, b) :\ 0) { _.|-| + _ }
  def manhattan(pair: (Item, Item)): Int = manhattan(pair._1, pair._2)

  def minDistance(pair: (Cluster, Cluster)) = manhattan(pairwise(pair._1.items, pair._2.items) minBy { manhattan(_) })
  def maxDistance(pair: (Cluster, Cluster)) = manhattan(pairwise(pair._1.items, pair._2.items) maxBy { manhattan(_) })

  def smallestMinDistance(clusters: Array[Cluster]) = pairwise(clusters) minBy { minDistance(_) }
  def smallestMaxDistance(clusters: Array[Cluster]) = pairwise(clusters) minBy { maxDistance(_) }

  def agglomerativeClustering(items: Array[Item], toMerge: Array[Cluster] => (Cluster, Cluster)) = {
    def recursive(clusters: Array[Cluster]): Cluster = {
      if (clusters.size == 1) clusters(0)
      else {
        val closest = toMerge(clusters)
        printf("Merging (min pairwise distance: %d)\n", minDistance(closest))
        closest._1.printItems
        closest._2.printItems
        val mergedCluster = new Cluster(Array(closest._1, closest._2))
        val newClusters = (clusters diff Array(closest._1, closest._2)) :+ mergedCluster
        recursive(newClusters)
      }
    }
    recursive(items map { new LeafCluster(_) })
  }

  def agglomerativeSingleLinkClustering(items: Array[Item]) = agglomerativeClustering(items, smallestMinDistance)
  def agglomerativeCompleteLinkClustering(items: Array[Item]) = agglomerativeClustering(items, smallestMaxDistance)

  val data = Array(Vector(1, 1, 1), Vector(1, 3, 3), Vector(2, 4, 5),
    Vector(5, 1, 1), Vector(8, 2, 1), Vector(6, 1, 2), Vector(5, 3, 2))

    printf("Single link:\n")
    agglomerativeSingleLinkClustering(data)
//    printf("Complete Link:\n")
//    agglomerativeCompleteLinkClustering(data)
}