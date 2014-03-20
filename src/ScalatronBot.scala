// Example Bot #1: The Reference Bot

/**
 * This bot builds a 'direction value map' that assigns an attractiveness score to
 * each of the eight available 45-degree directions. Additional behaviors:
 * - aggressive missiles: approach an enemy master, then explode
 * - defensive missiles: approach an enemy slave and annihilate it
 *
 * The master bot uses the following state parameters:
 *  - dontFireAggressiveMissileUntil
 *  - dontFireDefensiveMissileUntil
 *  - lastDirection
 * The mini-bots use the following state parameters:
 *  - mood = Aggressive | Defensive | Lurking
 *  - target = remaining offset to target location
 */
import scala.util.Random

object Items {
  val wall = 'W'
  val zugar = 'P'
  val toxifera = 'p'
  val fluppet = 'B'
  val snorg = 'b'
  val miniBot = 'S'
}

case class Choice(direction: XY) {
  def dangerous = {
    closestToxifera < 2 || closestWall < 2 || closestSnorg < 3
  }
  def betterNotGoHereMiniBot = {
    dangerous || closestMiniBot < 2
  }
  var closestWall = 1000.0
  var closestZugar = 1000.0
  var closestToxifera = 1000.0
  var closestFluppet = 1000.0
  var closestSnorg = 1000.0
  var closestMiniBot = 1000.0
}

object ControlFunction {

  def getBestDirection(bot: Bot) = {
    var availablePositions = Array(-1, 0, 1)
    //converts (-1, 0, 1) to ((-1, -1), (-1, 0), (-1, 1), (0, -1) ... )
    var availableDirections = (List[List[Int]]() /: availablePositions)((b, a) => b.union((List[List[Int]]() /: availablePositions)((d, c) =>
      a match {
        // omit (0, 0)
        case 0 =>
          c match {
            case 0 => d
            case _ => d :+ List(a, c)
          }
        case _ => d :+ List(a, c)
      })))
    var choices = (List[Choice]() /: availableDirections)((b, a) => b :+ Choice(XY(a(0), a(1))))
    val distances = find(bot.view, Items.wall)
    val distancesZugar = find(bot.view, Items.zugar)
    val distancesToxifera = find(bot.view, Items.toxifera)
    val distancesFluppets = find(bot.view, Items.fluppet)
    val distancesSnorgs = find(bot.view, Items.snorg)
    val distancesMiniBots = find(bot.view, Items.miniBot)
    
    for ((choice, index) <- choices.zipWithIndex) {
      choice.closestToxifera = distancesToxifera(index)
      choice.closestWall = distances(index)
      choice.closestZugar = distancesZugar(index)
      choice.closestFluppet = distancesFluppets(index)
      choice.closestSnorg = distancesSnorgs(index)
      choice.closestMiniBot = distancesMiniBots(index)
    }
    
    choices
  }

  def forMaster(bot: Bot) {
    var choices = getBestDirection(bot)
    var nonDangerousDirections = choices.filter(choice => !choice.dangerous)
    var movingDirection = Choice(XY(0, 0))
    if (nonDangerousDirections.size > 0) {
      var movingTowardsClosestFluppet = nonDangerousDirections.minBy[Double](x => x.closestFluppet)
      var movingDirectionTowardsClosestZugar = nonDangerousDirections.minBy[Double](x => x.closestZugar)
      if (movingTowardsClosestFluppet.closestFluppet / movingDirectionTowardsClosestZugar.closestZugar < 2.5) {
        movingDirection = movingTowardsClosestFluppet
      } else {
        movingDirection = movingDirectionTowardsClosestZugar
      }
      if (movingTowardsClosestFluppet.closestFluppet + movingDirectionTowardsClosestZugar.closestZugar > 1500) {
        movingDirection = Random.shuffle(nonDangerousDirections).head
      }
    }
    bot move (movingDirection.direction)
    //if (countBeasts(bot.view, Items.zugar) + countBeasts(bot.view, Items.fluppet) > 1) {
    bot.status(bot.time.toString)
    if (bot.time < 100 || countBeasts(bot.view, Items.zugar) + countBeasts(bot.view, Items.fluppet) > 2) {
      bot.spawn(nonDangerousDirections.minBy[Double](x => x.closestZugar).direction)
    }
    //}
  }

  def forSlave(bot: MiniBot) {
    var choices = getBestDirection(bot)
    println(choices)
    var nonDangerousDirections = choices.filter(choice => !choice.betterNotGoHereMiniBot)
    
    var movingDirection = Choice(XY(0, 0))
    if (nonDangerousDirections.size > 0) {
      var movingTowardsClosestFluppet = nonDangerousDirections.minBy[Double](x => x.closestFluppet)
      var movingDirectionTowardsClosestZugar = nonDangerousDirections.minBy[Double](x => x.closestZugar)
      if (movingTowardsClosestFluppet.closestFluppet / movingDirectionTowardsClosestZugar.closestZugar < 2.5) {
        movingDirection = movingTowardsClosestFluppet
      } else {
        movingDirection = movingDirectionTowardsClosestZugar
      }
      if (movingTowardsClosestFluppet.closestFluppet + movingDirectionTowardsClosestZugar.closestZugar > 1500) {
        movingDirection = Choice(bot.offsetToMaster)
      }
    }
    if ((1000 - bot.time) / 10 < bot.offsetToMaster.length) {     
      movingDirection = Choice(XY.fromDirection45(bot.offsetToMaster.toDirection45))
      if (nonDangerousDirections.filter(x => x.direction.x == movingDirection.direction.x &&
          x.direction.y == movingDirection.direction.y).length == 0){
    	  movingDirection = Random.shuffle(nonDangerousDirections).head
      } 
    }
    bot.move(movingDirection.direction)
    bot.status(bot.energy.toString)
    if (countBeasts(bot.view, Items.zugar) + countBeasts(bot.view, Items.fluppet) > 2) {
      bot.spawn(XY(1, 1))
    }
  }

  def find(view: View, item: Char) = {
    var directionValue = Array.fill(8)(1000.0) //1000 is infinity
    try{
    for ((cell, index) <- view.cells.zipWithIndex) {
      cell match {
        case `item` =>
          if (index != 4){
        	  directionValue = updateDirectionValue(view, directionValue, index)
          }
        case default =>
      }
    }
    }
    catch{
      case e : Exception => println(e.getMessage())
    }
    directionValue
  }

  def countBeasts(view: View, item: Char) = {
    view.cells.filter(x => x == item).length()
  }

  def updateDirectionValue(view: View, directionValue: Array[Double], index: Int) = {
    val relativePos = view relPosFromIndex (index)
    val direction = relativePos.toDirection45
    val i = getIndexFromPosition(direction)
    val distance = relativePos.length
    if (distance < directionValue(i)) {
      directionValue(i) = distance
    }
    directionValue
  }

  def getIndexFromPosition(direction: Int) = {
    direction match {
      case Direction45.Down => 4
      case Direction45.DownRight => 7
      case Direction45.Left => 1
      case Direction45.LeftDown => 2
      case Direction45.Right => 6
      case Direction45.RightUp => 5
      case Direction45.Up => 3
      case Direction45.UpLeft => 0
    }
  }

  /**
   * Analyze the view, building a map of attractiveness for the 45-degree directions and
   * recording other relevant data, such as the nearest elements of various kinds.
   */
  def analyzeViewAsMaster(view: View) = {
    val directionValue = Array.ofDim[Double](8)
    var nearestEnemyMaster: Option[XY] = None
    var nearestEnemySlave: Option[XY] = None

    val cells = view.cells
    val cellCount = cells.length
    for (i <- 0 until cellCount) {
      val cellRelPos = view.relPosFromIndex(i)
      if (cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = cells(i) match {
          case 'm' => // another master: not dangerous, but an obstacle
            nearestEnemyMaster = Some(cellRelPos)
            if (stepDistance < 2) -1000 else 0

          case 's' => // another slave: potentially dangerous?
            nearestEnemySlave = Some(cellRelPos)
            -100 / stepDistance

          case 'S' => // out own slave
            0.0

          case 'B' => // good beast: valuable, but runs away
            if (stepDistance == 1) 600
            else if (stepDistance == 2) 300
            else (150 - stepDistance * 15).max(10)

          case 'P' => // good plant: less valuable, but does not run
            if (stepDistance == 1) 500
            else if (stepDistance == 2) 300
            else (150 - stepDistance * 10).max(10)

          case 'b' => // bad beast: dangerous, but only if very close
            if (stepDistance < 4) -400 / stepDistance else -50 / stepDistance

          case 'p' => // bad plant: bad, but only if I step on it
            if (stepDistance < 2) -1000 else 0

          case 'W' => // wall: harmless, just don't walk into it
            if (stepDistance < 2) -1000 else 0

          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }
    (directionValue, nearestEnemyMaster, nearestEnemySlave)
  }
}

// -------------------------------------------------------------------------------------------------
// Framework
// -------------------------------------------------------------------------------------------------

class ControlFunctionFactory {
  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "React" =>
        val bot = new BotImpl(params)
        if (bot.generation == 0) {
          ControlFunction.forMaster(bot)
        } else {
          ControlFunction.forSlave(bot)
        }
        bot.toString
      case _ => "" // OK
    }
  }
}

// -------------------------------------------------------------------------------------------------

trait Bot {
  // inputs
  def inputOrElse(key: String, fallback: String): String
  def inputAsIntOrElse(key: String, fallback: Int): Int
  def inputAsXYOrElse(keyPrefix: String, fallback: XY): XY
  def view: View
  def energy: Int
  def time: Int
  def generation: Int

  // outputs
  def move(delta: XY): Bot
  def say(text: String): Bot
  def status(text: String): Bot
  def spawn(offset: XY, params: (String, Any)*): Bot
  def set(params: (String, Any)*): Bot
  def log(text: String): Bot
}

trait MiniBot extends Bot {
  // inputs
  def offsetToMaster: XY

  // outputs
  def explode(blastRadius: Int): Bot
}

case class BotImpl(inputParams: Map[String, String]) extends MiniBot {
  // input
  def inputOrElse(key: String, fallback: String) = inputParams.getOrElse(key, fallback)
  def inputAsIntOrElse(key: String, fallback: Int) = inputParams.get(key).map(_.toInt).getOrElse(fallback)
  def inputAsXYOrElse(key: String, fallback: XY) = inputParams.get(key).map(s => XY(s)).getOrElse(fallback)

  val view = View(inputParams("view"))
  val energy = inputParams("energy").toInt
  val time = inputParams("time").toInt
  val generation = inputParams("generation").toInt
  def offsetToMaster = inputAsXYOrElse("master", XY.Zero)

  // output

  private var stateParams = Map.empty[String, Any] // holds "Set()" commands
  private var commands = "" // holds all other commands
  private var debugOutput = "" // holds all "Log()" output

  /** Appends a new command to the command string; returns 'this' for fluent API. */
  private def append(s: String): Bot = { commands += (if (commands.isEmpty) s else "|" + s); this }

  /** Renders commands and stateParams into a control function return string. */
  override def toString = {
    var result = commands
    if (!stateParams.isEmpty) {
      if (!result.isEmpty) result += "|"
      result += stateParams.map(e => e._1 + "=" + e._2).mkString("Set(", ",", ")")
    }
    if (!debugOutput.isEmpty) {
      if (!result.isEmpty) result += "|"
      result += "Log(text=" + debugOutput + ")"
    }
    result
  }

  def log(text: String) = { debugOutput += text + "\n"; this }
  def move(direction: XY) = append("Move(direction=" + direction + ")")
  def say(text: String) = append("Say(text=" + text + ")")
  def status(text: String) = append("Status(text=" + text + ")")
  def explode(blastRadius: Int) = append("Explode(size=" + blastRadius + ")")
  def spawn(offset: XY, params: (String, Any)*) =
    append("Spawn(direction=" + offset +
      (if (params.isEmpty) "" else "," + params.map(e => e._1 + "=" + e._2).mkString(",")) +
      ")")
  def set(params: (String, Any)*) = { stateParams ++= params; this }
  def set(keyPrefix: String, xy: XY) = { stateParams ++= List(keyPrefix + "x" -> xy.x, keyPrefix + "y" -> xy.y); this }
}

// -------------------------------------------------------------------------------------------------

/**
 * Utility methods for parsing strings containing a single command of the format
 * "Command(key=value,key=value,...)"
 */
object CommandParser {
  /** "Command(..)" => ("Command", Map( ("key" -> "value"), ("key" -> "value"), ..}) */
  def apply(command: String): (String, Map[String, String]) = {
    /** "key=value" => ("key","value") */
    def splitParameterIntoKeyValue(param: String): (String, String) = {
      val segments = param.split('=')
      (segments(0), if (segments.length >= 2) segments(1) else "")
    }

    val segments = command.split('(')
    if (segments.length != 2)
      throw new IllegalStateException("invalid command: " + command)
    val opcode = segments(0)
    val params = segments(1).dropRight(1).split(',')
    val keyValuePairs = params.map(splitParameterIntoKeyValue).toMap
    (opcode, keyValuePairs)
  }
}

// -------------------------------------------------------------------------------------------------

/**
 * Utility class for managing 2D cell coordinates.
 * The coordinate (0,0) corresponds to the top-left corner of the arena on screen.
 * The direction (1,-1) points right and up.
 */
case class XY(x: Int, y: Int) {
  override def toString = x + ":" + y

  def isNonZero = x != 0 || y != 0
  def isZero = x == 0 && y == 0
  def isNonNegative = x >= 0 && y >= 0

  def updateX(newX: Int) = XY(newX, y)
  def updateY(newY: Int) = XY(x, newY)

  def addToX(dx: Int) = XY(x + dx, y)
  def addToY(dy: Int) = XY(x, y + dy)

  def +(pos: XY) = XY(x + pos.x, y + pos.y)
  def -(pos: XY) = XY(x - pos.x, y - pos.y)
  def *(factor: Double) = XY((x * factor).intValue, (y * factor).intValue)

  def distanceTo(pos: XY): Double = (this - pos).length // Phythagorean
  def length: Double = math.sqrt(x * x + y * y) // Phythagorean

  def stepsTo(pos: XY): Int = (this - pos).stepCount // steps to reach pos: max delta X or Y
  def stepCount: Int = x.abs.max(y.abs) // steps from (0,0) to get here: max X or Y

  def signum = XY(x.signum, y.signum)

  def negate = XY(-x, -y)
  def negateX = XY(-x, y)
  def negateY = XY(x, -y)

  /** Returns the direction index with 'Right' being index 0, then clockwise in 45 degree steps. */
  def toDirection45: Int = {
    val unit = signum
    unit.x match {
      case -1 =>
        unit.y match {
          case -1 =>
            if (x < y * 3) Direction45.Left
            else if (y < x * 3) Direction45.Up
            else Direction45.UpLeft
          case 0 =>
            Direction45.Left
          case 1 =>
            if (-x > y * 3) Direction45.Left
            else if (y > -x * 3) Direction45.Down
            else Direction45.LeftDown
        }
      case 0 =>
        unit.y match {
          case 1 => Direction45.Down
          case 0 => throw new IllegalArgumentException("cannot compute direction index for (0,0)")
          case -1 => Direction45.Up
        }
      case 1 =>
        unit.y match {
          case -1 =>
            if (x > -y * 3) Direction45.Right
            else if (-y > x * 3) Direction45.Up
            else Direction45.RightUp
          case 0 =>
            Direction45.Right
          case 1 =>
            if (x > y * 3) Direction45.Right
            else if (y > x * 3) Direction45.Down
            else Direction45.DownRight
        }
    }
  }

  def rotateCounterClockwise45 = XY.fromDirection45((signum.toDirection45 + 1) % 8)
  def rotateCounterClockwise90 = XY.fromDirection45((signum.toDirection45 + 2) % 8)
  def rotateClockwise45 = XY.fromDirection45((signum.toDirection45 + 7) % 8)
  def rotateClockwise90 = XY.fromDirection45((signum.toDirection45 + 6) % 8)

  def wrap(boardSize: XY) = {
    val fixedX = if (x < 0) boardSize.x + x else if (x >= boardSize.x) x - boardSize.x else x
    val fixedY = if (y < 0) boardSize.y + y else if (y >= boardSize.y) y - boardSize.y else y
    if (fixedX != x || fixedY != y) XY(fixedX, fixedY) else this
  }
}

object XY {
  /** Parse an XY value from XY.toString format, e.g. "2:3". */
  def apply(s: String): XY = { val a = s.split(':'); XY(a(0).toInt, a(1).toInt) }

  val Zero = XY(0, 0)
  val One = XY(1, 1)

  val Right = XY(1, 0)
  val RightUp = XY(1, -1)
  val Up = XY(0, -1)
  val UpLeft = XY(-1, -1)
  val Left = XY(-1, 0)
  val LeftDown = XY(-1, 1)
  val Down = XY(0, 1)
  val DownRight = XY(1, 1)

  def fromDirection45(index: Int): XY = index match {
    case Direction45.Right => Right
    case Direction45.RightUp => RightUp
    case Direction45.Up => Up
    case Direction45.UpLeft => UpLeft
    case Direction45.Left => Left
    case Direction45.LeftDown => LeftDown
    case Direction45.Down => Down
    case Direction45.DownRight => DownRight
  }

  def fromDirection90(index: Int): XY = index match {
    case Direction90.Right => Right
    case Direction90.Up => Up
    case Direction90.Left => Left
    case Direction90.Down => Down
  }

  def apply(array: Array[Int]): XY = XY(array(0), array(1))
}

object Direction45 {
  val Right = 0
  val RightUp = 1
  val Up = 2
  val UpLeft = 3
  val Left = 4
  val LeftDown = 5
  val Down = 6
  val DownRight = 7
}

object Direction90 {
  val Right = 0
  val Up = 1
  val Left = 2
  val Down = 3
}

// -------------------------------------------------------------------------------------------------

case class View(cells: String) {
  val size = math.sqrt(cells.length).toInt
  val center = XY(size / 2, size / 2)

  def apply(relPos: XY) = cellAtRelPos(relPos)

  def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
  def absPosFromIndex(index: Int) = XY(index % size, index / size)
  def absPosFromRelPos(relPos: XY) = relPos + center
  def cellAtAbsPos(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))

  def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))
  def relPosFromAbsPos(absPos: XY) = absPos - center
  def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
  def cellAtRelPos(relPos: XY) = cells.charAt(indexFromRelPos(relPos))

  def offsetToNearest(c: Char) = {
    val matchingXY = cells.view.zipWithIndex.filter(_._1 == c)
    if (matchingXY.isEmpty)
      None
    else {
      val nearest = matchingXY.map(p => relPosFromIndex(p._2)).minBy(_.length)
      Some(nearest)
    }
  }
}
