List(1, 2, 3).map(x => 0)

val vv = Stream.from(1) // #::: Stream.from(0)

val t = for {
  s <- vv
} yield (s)

t.take(3).foreach(println)

List(1,2,3,4).foldLeft(0)((m: Int, n: Int) => m)



