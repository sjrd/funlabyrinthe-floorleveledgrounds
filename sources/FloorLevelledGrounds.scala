package user.sjrd.floorlevelledgrounds

import scala.annotation.tailrec

import com.funlabyrinthe.core.*
import com.funlabyrinthe.core.graphics.*
import com.funlabyrinthe.mazes.*
import com.funlabyrinthe.mazes.std.*

object FloorLevelledGrounds extends Module:
  override protected def createComponents()(using Universe): Unit =
    val floorLevelledGroundCreator = new FloorLevelledGroundCreator
    val fullField = new FullField
    val emptyField = new EmptyField

    val tunnelCreator = new TunnelCreator
    val bridgeCreator = new BridgeCreator
  end createComponents

  def floorLevelledGroundCreator(using Universe): FloorLevelledGroundCreator =
    myComponentByID("floorLevelledGroundCreator")

  def fullField(using Universe): FullField = myComponentByID("fullField")
  def emptyField(using Universe): EmptyField = myComponentByID("emptyField")

  def tunnelCreator(using Universe): TunnelCreator = myComponentByID("tunnelCreator")
  def bridgeCreator(using Universe): BridgeCreator = myComponentByID("bridgeCreator")
end FloorLevelledGrounds

export FloorLevelledGrounds.*

final case class ClimbLevelUp(levelDiff: Int) extends Ability
final case class FallLevelDown(levelDiff: Int) extends Ability

final class FloorLevelledGroundCreator(using ComponentInit) extends ComponentCreator[FloorLevelledGround]:
  category = ComponentCategory("levelledgrounds", "Levelled Grounds")

  icon += "Creators/LevelledGroundCreator"
  icon += "Creators/Creator"
end FloorLevelledGroundCreator

class FloorLevelledGround(using ComponentInit) extends Ground derives Reflector:
  painter += "Fields/Grass"
  category = ComponentCategory("levelledgrounds", "Levelled Grounds")

  var level: Int = 0

  override def reflect() = autoReflect[FloorLevelledGround]

  override protected def doDrawCeiling(context: DrawSquareContext): Unit =
    Bridge.drawBridgesAbove(context)

  override protected def editMapAdd(ref: SquareRef)(using EditingServices): Unit =
    val map = ref.map
    val pos = ref.pos
    val isInside = ref.isInside

    if level < 0 || level >= map.dimensions.z then
      EditingServices.error(
        "This map does not have enough floors for this field."
      )
    else
      // Place this field at the floor specified by Level
      if isInside then
        map(pos.withZ(level)) = this
      else
        map.outside(level) = this
    
      // Place full fields below
      for z <- 0 until level if !map(pos.withZ(z)).field.isInstanceOf[FullField] do
        if isInside then
          map(pos.withZ(z)) = fullField
        else
          map.outside(z) = fullField
      end for

      // Place empty fields above
      for z <- (level + 1) until map.dimensions.z if !map(pos.withZ(z)).field.isInstanceOf[EmptyField] do
        if isInside then
          map(pos.withZ(z)) = emptyField
        else
          map.outside(z) = emptyField
      end for
    end if
  end editMapAdd
end FloorLevelledGround

sealed abstract class FullOrEmptyField(using ComponentInit) extends Field:
  category = ComponentCategory("levelledgrounds", "Levelled Grounds")

  override protected def editMapRedirect(pos: SquareRef, newComponent: SquareComponent): SquareRef =
    if newComponent.isInstanceOf[Field] then pos
    else findDestSquare(pos).getOrElse(pos)

  protected final def findDestSquare(pos: SquareRef): Option[SquareRef] =
    doFindDestSquare(pos).filter(_ != pos)

  protected def doFindDestSquare(pos: SquareRef): Option[SquareRef]

  override protected def doDraw(context: DrawSquareContext): Unit =
    import context.*
    
    where.flatMap(findDestSquare(_)) match
      case None =>
        gc.fill = Color.Black
        gc.fillRect(rect.minX, rect.minY, rect.width, rect.height)
      case Some(dest) =>
        dest().drawTo(context.withWhere(Some(dest)))
  end doDraw

  override protected def doDrawCeiling(context: DrawSquareContext): Unit =
    Bridge.drawBridgesAbove(context)

  protected def moveToOtherDest(context: MoveContext, dest: SquareRef): Unit = {
    val player = context.player
    val otherContext = MoveContext(player, Some(dest), context.keyEvent)
    otherContext.temporization = context.temporization

    if !player.testMoveAllowed(otherContext) then
      context.cancel()
    else
      if player.position == otherContext.src then
        player.moveTo(otherContext, execute = true)

      context.cancelled = otherContext.cancelled
      context.goOnMoving = otherContext.goOnMoving
      context.temporization = otherContext.temporization
  }
end FullOrEmptyField

class FullField(using ComponentInit) extends FullOrEmptyField:
  @tailrec
  protected final def doFindDestSquare(pos: SquareRef): Option[SquareRef] =
    val above = pos + (0, 0, 1)
    if above.pos.z >= above.map.dimensions.z then None
    else
      above().field match
        case _: FullField => doFindDestSquare(above)
        case _            => Some(above)
  end doFindDestSquare

  override def entering(context: MoveContext): Unit = {
    import context.*

    findDestSquare(pos) match
      case None =>
        cancel()

      case Some(above) =>
        val heightDiff = above.z - pos.z
        if player.cannot(ClimbLevelUp(heightDiff)) then
          cancel()
        else
          moveToOtherDest(context, above)
  }
end FullField

class EmptyField(using ComponentInit) extends FullOrEmptyField:
  @tailrec
  protected final def doFindDestSquare(pos: SquareRef): Option[SquareRef] =
    val below = pos - (0, 0, 1)
    if below.pos.z < 0 then None
    else
      below().field match
        case _: EmptyField => doFindDestSquare(below)
        case _             => Some(below)
  end doFindDestSquare

  override def entering(context: MoveContext): Unit = {
    import context.*

    findDestSquare(pos) match
      case None =>
        cancel()

      case Some(below) =>
        val heightDiff = pos.z - below.z
        if player.cannot(FallLevelDown(heightDiff)) then
          cancel()
          player.showMessage("C'est trop haut pour sauter ici !")
        else
          moveToOtherDest(context, below)
  }

  override def dispatch[A]: PartialFunction[SquareMessage[A], A] = {
    case PlankInteraction(PlankInteraction.Kind.PassOver, _, passOverPos, _, _) =>
      if passOverPos().obstacle != noObstacle then
        false
      else
        val below = passOverPos - (0, 0, 1)
        below.isOutside || below().obstacle == noObstacle
  }
end EmptyField

final class TunnelCreator(using ComponentInit) extends ComponentCreator[Tunnel]:
  category = ComponentCategory("tunnels", "Tunnels")

  icon += "Gates/Tunnel"
  icon += "Creators/Creator"
end TunnelCreator

class Tunnel(using ComponentInit) extends FullField derives Reflector:
  import Tunnel.*
  
  var openings: Set[Direction] = Direction.values.toSet
  
  @transient @noinspect
  val gatePainters: List[Painter] =
    Direction.values.toList.map(d => universe.EmptyPainter + s"Gates/Tunnel$d")

  category = ComponentCategory("tunnels", "Tunnels")

  override def reflect() = autoReflect[Tunnel]

  // Cancel the redirect of FullField
  override protected def editMapRedirect(pos: SquareRef, newComponent: SquareComponent): SquareRef =
    pos

  def hasOpening(dir: Direction): Boolean =
    openings.contains(dir)

  override protected def doDraw(context: DrawSquareContext): Unit =
    import context.*

    if drawModeFor(context) == DrawMode.Open then
      painter.drawTo(context)
  end doDraw

  override protected def doDrawCeiling(context: DrawSquareContext): Unit =
    import context.*
    
    drawModeFor(context) match
      case DrawMode.Open =>
        val aboveCanvas = universe.graphicsSystem.createCanvas(SquareSize, SquareSize)
        val aboveGC = aboveCanvas.getGraphicsContext2D()
    
        // super.doDraw on aboveCanvas
        val aboveContext = context.withGraphicsContext(
          aboveGC,
          Rectangle2D(0, 0, SquareSize, SquareSize),
        )
        where.flatMap(findDestSquare(_)) match
          case Some(dest) => dest().field.drawTo(aboveContext.withWhere(Some(dest)))
          case None       => super.doDraw(aboveContext)
    
        // Make some parts of aboveCanvas transparent (center and openings)
        def clearRect(rect: Rectangle2D): Unit =
          aboveGC.clearRect(rect.minX, rect.minY, rect.width, rect.height)
        clearRect(CenterRect)
        for dir <- Direction.values do
          if isActuallyOpened(where, dir) then
            clearRect(OpeningRects(dir.ordinal))
    
        // Draw aboveCanvas on the final context
        gc.drawImage(aboveCanvas, rect.minX, rect.minY)

      case DrawMode.Closed =>
        super.doDraw(context) // behavior of FullField

      case DrawMode.ClosedWithGates =>
        super.doDraw(context) // behavior of FullField
        for dir <- Direction.values do
          if isGate(where, dir) then
            gatePainters(dir.ordinal).drawTo(context)
  end doDrawCeiling

  protected def drawModeFor(context: DrawSquareContext): DrawMode =
    context.purpose match
      case purpose: DrawPurpose.PlayerView =>
        (context.where, purpose.player.position) match
          case (Some(pos), Some(playerPos)) =>
            if pos.z != playerPos.z then DrawMode.Closed
            else if playerPos().field.isInstanceOf[Tunnel] then DrawMode.Open
            else DrawMode.ClosedWithGates
          case _ =>
            DrawMode.Open
      case purpose: DrawPurpose.EditMap =>
        if context.where.forall(_.z == purpose.floor) then
          DrawMode.Open
        else
          DrawMode.Closed
      case _ =>
        DrawMode.Open
  end drawModeFor

  override def entering(context: MoveContext): Unit = {
    import context.*

    val dir = player.direction
    if isRegular && dir.isDefined && !hasOpening(dir.get.opposite) then
      /* If we cannot enter the tunnel from here, act as the full field:
       * maybe we can climb up on top of it.
       */
      super.entering(context)
  }

  override def exiting(context: MoveContext): Unit = {
    import context.*

    val dir = player.direction
    if isRegular && dir.isDefined && !hasOpening(dir.get) then
      // If we cannot exit the tunnel here, always cancel
      cancel()
  }

  /** Is this tunnel effectively open at a given position?
   *
   *  This is the case if it has an opening and one of
   *  the following conditions apply:
   *  - there is another connecting tunnel next to it,
   *  - there is a non-FullField next to it (which means there is a gate), or
   *  - it is nowhere (such as in the icon, to show openings).
   */
  def isActuallyOpened(where: Option[SquareRef], dir: Direction): Boolean =
    hasOpening(dir)
      && where.forall { pos =>
        (pos +> dir)().field match
          case otherField: Tunnel    => otherField.hasOpening(dir.opposite)
          case otherField: FullField => false
          case _                     => true
      }
  end isActuallyOpened

  /** Is there a gate for this tunnel at a given position?
   *
   *  This is the case if it has an opening and there
   *  is a non-Fullfield next to it.
   */
  def isGate(where: Option[SquareRef], dir: Direction): Boolean =
    hasOpening(dir)
      && where.exists { pos =>
        (pos +> dir)().field match
          case otherField: FullField => false
          case _                     => true
      }
  end isGate
end Tunnel

object Tunnel:
  private val SquareSize = 30
  private val BorderSize = 5
  private val AntiBorderSize = SquareSize - BorderSize
  private val CenterSize = SquareSize - 2*BorderSize
  
  private val CenterRect =
    Rectangle2D(BorderSize, BorderSize, CenterSize, CenterSize)

  private val OpeningRects =
    for dir <- Direction.values yield
      dir match
        case Direction.North =>
          Rectangle2D(BorderSize, 0, CenterSize, BorderSize)
        case Direction.East =>
          Rectangle2D(AntiBorderSize, BorderSize, BorderSize, CenterSize)
        case Direction.South =>
          Rectangle2D(BorderSize, AntiBorderSize, CenterSize, BorderSize)
        case Direction.West =>
          Rectangle2D(0, BorderSize, BorderSize, CenterSize)
  end OpeningRects

  enum DrawMode:
    case Open, Closed, ClosedWithGates
end Tunnel

final class BridgeCreator(using ComponentInit) extends ComponentCreator[Bridge]:
  category = ComponentCategory("bridges", "Bridges")

  @transient @noinspect
  val centerPainter: Painter =
    universe.EmptyPainter + "Bridges/BridgeCenter"
    
  @transient @noinspect
  val openingPainters: List[Painter] =
    Direction.values.toList.map(d => universe.EmptyPainter + s"Bridges/Bridge$d")

  icon ++= centerPainter.items ++ openingPainters.flatMap(_.items)
  icon += "Creators/Creator"
end BridgeCreator

class Bridge(using ComponentInit) extends Field derives Reflector:
  var openings: Set[Direction] = Direction.values.toSet
  
  category = ComponentCategory("bridges", "Bridges")

  override def reflect() = autoReflect[Bridge]

  override protected def doDraw(context: DrawSquareContext): Unit =
    import context.*

    // Draw the square below the bridge
    if isSomewhere then
      val below = where.get - (0, 0, 1)
      if below.isInside then
        below().drawTo(context.withWhere(Some(below)))

    // Draw the bridge itself
    doDrawBridge(context)
  end doDraw

  def doDrawBridge(context: DrawSquareContext): Unit =
    import context.*

    val creator = bridgeCreator
    creator.centerPainter.drawTo(context)
    for dir <- Direction.values do
      if isActuallyOpened(where, dir) then
        creator.openingPainters(dir.ordinal).drawTo(context)
  end doDrawBridge

  def hasOpening(dir: Direction): Boolean =
    openings.contains(dir)

  def isActuallyOpened(where: Option[SquareRef], dir: Direction): Boolean =
    hasOpening(dir)
      && where.forall { pos =>
        (pos +> dir)().field match
          case otherField: Bridge => otherField.hasOpening(dir.opposite)
          case otherField: Tunnel => otherField.hasOpening(dir.opposite)
          case otherField: Ground => true
          case _                  => false
      }
  end isActuallyOpened

  override def entering(context: MoveContext): Unit = {
    import context.*

    player.direction match
      case Some(dir) =>
        if isRegular && (src.get.pos +> dir) == dest.get.pos then
          if !isActuallyOpened(Some(pos), dir.opposite) then
            cancel()
      case None =>
        ()
  }

  override def exiting(context: MoveContext): Unit = {
    import context.*

    player.direction match
      case Some(dir) =>
        if isRegular && (src.get.pos +> dir) == dest.get.pos then
          if !isActuallyOpened(Some(pos), dir) then
            cancel()
      case None =>
        ()
  }
end Bridge

object Bridge:
  /** Draw the bridges that are above a given square. */
  def drawBridgesAbove(context: DrawSquareContext): Unit =
    if context.isSomewhere then
      val pos = context.where.get
      for z <- pos.z until pos.map.dimensions.z do
        val above = pos.withZ(z)
        val aboveSquare = above()
        aboveSquare.field match
          case bridge: Bridge =>
            val aboveContext = context.withWhere(Some(above))
            bridge.doDrawBridge(aboveContext)
            aboveSquare.effect.drawTo(aboveContext)
            aboveSquare.tool.drawTo(aboveContext)
            aboveSquare.obstacle.drawTo(aboveContext)
          case _ =>
            ()
      end for
    end if
  end drawBridgesAbove
end Bridge
