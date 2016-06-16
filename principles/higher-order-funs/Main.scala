package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  println(contains(singletonSet(2), 2))
  println(printSet(map(union(union(singletonSet(1), singletonSet(2)),singletonSet(3)), x => x + 10)))
}
