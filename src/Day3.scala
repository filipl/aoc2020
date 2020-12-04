import scala.annotation.tailrec

object Day3 extends Base(3) {

  sealed trait Tile
  case object Open extends Tile
  case object Tree extends Tile

  case class Map(map: List[List[Tile]]) {
    def tileAt(pos: Pos): Option[Tile] =
      if (pos.y >= map.length)
        None
      else
        Some(map(pos.y)(pos.x % map.head.length))

    def height: Int = map.length
  }

  object Map {
    def build: Map = Map(inputLines.map(_.map {
      case '.' => Open
      case '#' => Tree
    }.toList))
  }

  case class Dir(right: Int, down: Int)
  case class Pos(x: Int, y: Int) {
    def slide(dir: Dir): Pos = {
      Pos(x + dir.right, y + dir.down)
    }
  }

  def countTrees(profile: Seq[Dir])(implicit map: Map): BigInt = {
    def slide(pos: Pos): Pos =
      profile.foldLeft(pos)((p, d) => p.slide(d))

    @tailrec
    def rec(pos: Pos, tiles: Seq[Tile]): Seq[Tile] = {
      if (pos.y >= map.height)
        tiles
      else {
        val newPos = slide(pos)
        val tile = map.tileAt(newPos).getOrElse(Open)
        rec(newPos, tiles :+ tile)
      }
    }

    rec(Pos(0, 0), Seq()).count(_ == Tree)
  }

  val Down: Dir = Dir(0, 1)
  val Right: Dir = Dir(1, 0)

  override def first: BigInt =
    countTrees(Seq(Right, Right, Right, Down))(Map.build)

  override def second: BigInt = {
    implicit val map: Map = Map.build
    countTrees(Seq(Right, Down)) *
      countTrees(Seq(Right, Right, Right, Down)) *
      countTrees(Seq(Right, Right, Right, Right, Right, Down)) *
      countTrees(Seq(Right, Right, Right, Right, Right, Right, Right, Down)) *
      countTrees(Seq(Right, Down, Down))
  }
}