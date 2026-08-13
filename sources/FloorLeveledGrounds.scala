package user.sjrd.floorleveledgrounds

import scala.annotation.tailrec

import com.funlabyrinthe.core.*
import com.funlabyrinthe.core.scene.*
import com.funlabyrinthe.mazes.*
import com.funlabyrinthe.mazes.std.*

object FloorLeveledGrounds extends Module

@definition def floorLeveledGroundTemplate(using Universe) =
  new FloorLeveledGround().asTemplate("Creators/LeveledGroundCreator")
@definition def fullField(using Universe) = new FullField
@definition def emptyField(using Universe) = new EmptyField

@definition def tunnelTemplate(using Universe) =
  new Tunnel().asTemplate("Gates/Tunnel")
@definition def bridgeTemplate(using Universe) =
  new Bridge().asTemplate("Bridges/BridgeCenter", "Bridges/BridgeNorth", "Bridges/BridgeEast", "Bridges/BridgeSouth", "Bridges/BridgeWest")

final case class ClimbLevelUp(levelDiff: Int) extends Ability
final case class FallLevelDown(levelDiff: Int) extends Ability

class FloorLeveledGround(using ComponentInit) extends Ground:
  painter += "Fields/Grass"
  category = ComponentCategory("leveledgrounds", "Leveled Grounds")

  var level: Int = 0

  override protected def doPresentCeiling(context: PresentSquareContext): Batch[SceneNode] =
    Bridge.presentBridgesAbove(context)

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
end FloorLeveledGround

sealed abstract class FullOrEmptyField(using ComponentInit) extends Field:
  category = ComponentCategory("leveledgrounds", "Leveled Grounds")

  override protected def editMapRedirect(pos: SquareRef, newComponent: SquareComponent): SquareRef =
    if newComponent.isInstanceOf[Field] then pos
    else findDestSquare(pos).getOrElse(pos)

  protected final def findDestSquare(pos: SquareRef): Option[SquareRef] =
    doFindDestSquare(pos).filter(_ != pos)

  protected def doFindDestSquare(pos: SquareRef): Option[SquareRef]

  override protected def doPresent(context: PresentSquareContext): Batch[SceneNode] = {
    import context.*

    where.flatMap(findDestSquare(_)) match
      case None =>
        Batch(Shape.Box(Rectangle(Point.zero, cellSize), Fill.Color(RGBA.Black), Stroke.None, cellSize.centerPoint))
      case Some(dest) =>
        dest().present(context.withWhere(Some(dest)))
  }

  override protected def doPresentCeiling(context: PresentSquareContext): Batch[SceneNode] =
    Bridge.presentBridgesAbove(context)

  protected def moveToOtherDest(context: EnteringContext, dest: SquareRef): Unit = {
    val player = context.player
    val otherContext = EnteringContext(player, context.previousDirection, dest, context.keyEvent)
    otherContext.temporization = context.temporization

    if !player.testMoveAllowed(dest, context.previousDirection, context.keyEvent) then
      context.cancel()
    else
      context.cancel() // just in case some weird loop happens
      player.moveTo(dest, execute = true)
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

  override def entering(context: EnteringContext): Unit = {
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

  override def entering(context: EnteringContext): Unit = {
    import context.*

    findDestSquare(pos) match
      case None =>
        cancel()

      case Some(below) =>
        val heightDiff = pos.z - below.z
        if player.cannot(FallLevelDown(heightDiff)) then
          cancel()
          player.showMessageOnce("C'est trop haut pour sauter ici !")
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

class Tunnel(using ComponentInit) extends FullField:
  import Tunnel.*

  var openings: Set[Direction] = Direction.values.toSet

  @transient @noinspect
  val gatePainters: List[Painter] =
    Direction.values.toList.map(d => universe.EmptyPainter + s"Gates/Tunnel$d")

  category = ComponentCategory("tunnels", "Tunnels")

  // Cancel the redirect of FullField
  override protected def editMapRedirect(pos: SquareRef, newComponent: SquareComponent): SquareRef =
    pos

  def hasOpening(dir: Direction): Boolean =
    openings.contains(dir)

  override protected def doPresent(context: PresentSquareContext): Batch[SceneNode] =
    if drawModeFor(context) == DrawMode.Open then context.presentTiled(painter)
    else Batch.empty

  override protected def doPresentCeiling(context: PresentSquareContext): Batch[SceneNode] = {
    import context.*

    drawModeFor(context) match {
      case DrawMode.Open =>
        // Render what is above
        val presentedAbove: Batch[SceneNode] = where.flatMap(findDestSquare(_)) match
          case Some(dest) => dest().field.present(context.withWhere(Some(dest)))
          case None       => super.doPresent(context)

        // Build the mask to only display corners and walls from presentedAbove
        var mask: Batch[SceneNode] = CornersMasks
        for dir <- Direction.values do
          if !isActuallyOpened(where, dir) then
            mask :+= WallMasks(dir.ordinal)

        Batch(Masked(Group(mask), Group(presentedAbove)))

      case DrawMode.Closed =>
        super.doPresent(context) // behavior of FullField

      case DrawMode.ClosedWithGates =>
        var result = super.doPresent(context) // behavior of FullField
        for dir <- Direction.values do
          if isGate(where, dir) then
            result ++= context.presentTiled(gatePainters(dir.ordinal))
        result
    }
  }

  protected def drawModeFor(context: PresentSquareContext): DrawMode = {
    context.purpose match {
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
    }
  }

  override def entering(context: EnteringContext): Unit = {
    import context.*

    if !hasOpening(player.direction.opposite) then
      /* If we cannot enter the tunnel from here, act as the full field:
       * maybe we can climb up on top of it.
       */
      super.entering(context)
  }

  override def exiting(context: ExitingContext): Unit = {
    import context.*

    if !hasOpening(player.direction) then
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

  private val BlackFill = Fill.Color(RGBA.Black)

  private val CornersMasks: Batch[SceneNode] = {
    val cornerSize = Size(BorderSize, BorderSize)
    val topLefts = Batch(
      Point(0, 0),
      Point(AntiBorderSize, 0),
      Point(0, AntiBorderSize),
      Point(AntiBorderSize, AntiBorderSize),
    )
    val ref = Size(SquareSize, SquareSize).centerPoint
    for topLeft <- topLefts yield
      Shape.Box(Rectangle(topLeft, cornerSize), BlackFill, Stroke.None, ref)
  }

  private val WallMasks: Array[SceneNode] = {
    val ref = Size(SquareSize, SquareSize).centerPoint
    for dir <- Direction.values yield
      val rect = dir match {
        case Direction.North =>
          Rectangle(Point(BorderSize, 0), Size(CenterSize, BorderSize))
        case Direction.East =>
          Rectangle(Point(AntiBorderSize, BorderSize), Size(BorderSize, CenterSize))
        case Direction.South =>
          Rectangle(Point(BorderSize, AntiBorderSize), Size(CenterSize, BorderSize))
        case Direction.West =>
          Rectangle(Point(0, BorderSize), Size(BorderSize, CenterSize))
      }
      Shape.Box(rect, BlackFill, Stroke.None, ref)
  }

  enum DrawMode:
    case Open, Closed, ClosedWithGates
end Tunnel

class Bridge(using ComponentInit) extends Field:
  var openings: Set[Direction] = Direction.values.toSet

  category = ComponentCategory("bridges", "Bridges")

  @transient @noinspect
  val centerPainter: Painter =
    universe.EmptyPainter + "Bridges/BridgeCenter"

  @transient @noinspect
  val openingPainters: List[Painter] =
    Direction.values.toList.map(d => universe.EmptyPainter + s"Bridges/Bridge$d")

  override protected def doPresent(context: PresentSquareContext): Batch[SceneNode] = {
    import context.*

    // Draw the bridge itself
    val bridgeItself = doPresentBridge(context)

    // Draw the square *below* the bridge
    if isSomewhere then
      val below = where.get - (0, 0, 1)
      if below.isInside then
        below().present(context.withWhere(Some(below))) ++ bridgeItself
      else
        bridgeItself
    else
      bridgeItself
  }

  def doPresentBridge(context: PresentSquareContext): Batch[SceneNode] = {
    import context.*

    var result = context.presentTiled(centerPainter)
    for dir <- Direction.values do
      if isActuallyOpened(where, dir) then
        result ++= context.presentTiled(openingPainters(dir.ordinal))
    result
  }

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

  override def entering(context: EnteringContext): Unit = {
    import context.*

    if !isActuallyOpened(Some(pos), player.direction.opposite) then
      cancel()
  }

  override def exiting(context: ExitingContext): Unit = {
    import context.*

    if !isActuallyOpened(Some(pos), player.direction) then
      cancel()
  }
end Bridge

object Bridge:
  /** Present the bridges that are above a given square. */
  def presentBridgesAbove(context: PresentSquareContext): Batch[SceneNode] = {
    var result: Batch[SceneNode] = Batch.empty

    if context.isSomewhere then {
      val pos = context.where.get
      for z <- pos.z until pos.map.dimensions.z do
        val above = pos.withZ(z)
        val aboveSquare = above()
        aboveSquare.field match
          case bridge: Bridge =>
            val aboveContext = context.withWhere(Some(above))
            result = result
              ++ bridge.doPresentBridge(aboveContext)
              ++ aboveSquare.effect.present(aboveContext)
              ++ aboveSquare.tool.present(aboveContext)
              ++ aboveSquare.obstacle.present(aboveContext)
          case _ =>
            ()
      end for
    }

    result
  }
end Bridge
