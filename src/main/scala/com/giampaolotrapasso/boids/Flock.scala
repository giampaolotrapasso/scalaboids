package com.giampaolotrapasso.boids

import com.giampaolotrapasso.boids.utility.{Vector2D, WorldSize}

import scala.util.Random
import scalafx.scene.image.ImageView

case class Flock(boids: Seq[Boid], worldSize: WorldSize) {

  private val movementFactor = 1000
  private val boundingFactor = 10
  private val separationDistance = 10
  private val separationFactor = 200


  private val tendFactor = 5000


  private val maxVelocity = 1.0

  private val matchFlockFactor = 8.0


  private def setAngle(current: Vector2D, next: Vector2D): Double = {
    val dX = next.x - current.x
    val dY = next.y - current.y
    val theta = Math.atan2(dY, dX)
    // Change this.angle is into degrees
    theta * 180 / Math.PI
  }

  def updatedBoidsPosition(): Seq[Boid] = {
    val center: Vector2D = boids.map(_.position).fold(Vector2D(0.0, 0.0)) {
      (sum, element) => sum + element
    }.divide(boids.size)

    boids.map { boid =>
      val group = positionTend(boid, center, factor = movementFactor)
      //val bounds = bounding(boid)
      val avoid = collisionCheck2(boid)
      val overlap = collisionAvoidance(boid)
      val tend = positionTend(boid, worldSize.center, factor = tendFactor)
      val matchVelocity = matchFlockVelocity(boid)
      val sum = avoid * 0.4 + overlap *0.3 + tend * 0.2 + tend * 0.2

      // println(s"G $group B: $bounds")
      //+ collisionAvoidance(boid) + matchFlockVelocity(boid)  +

      val newVelocity: Vector2D = boid.velocity.add(sum).normalize
      val nextPosition = boid.position.add(newVelocity).add(worldSize.toVector2D).normalize(worldSize.width, worldSize.height)

      val angle = setAngle(boid.position, nextPosition)

      val display = boid.display
      display.setX(nextPosition.x)
      display.setY(nextPosition.y)
      display.setSmooth(true)
      display.setCache(true)
      display.setRotate(angle)

      Boid(
        position = nextPosition,
        velocity = newVelocity,
        angle = angle,
        display = display,
        worldSize = worldSize
      )
    }
  }

  def images = boids.map(_.display)


  private def groupFlock(cBoid: Boid, center: Vector2D, size: Int): Vector2D = {
    val c = (center - cBoid.position).divide(size)
    center.sub(cBoid.position).divide(movementFactor)
  }

  private def collisionAvoidance(cBoid: Boid): Vector2D = {
    var correction = new Vector2D
    var count = 0
    val cPosition = cBoid.nextPosition.copy()
    for (aBoid <- boids) {
      if (!(aBoid == cBoid)) {
        val aPosition = aBoid.nextPosition
        val xD = new Vector2D(aPosition.x - cPosition.x, aPosition.y - cPosition.y)
        if (Math.abs(xD.x) < separationDistance && Math.abs(xD.y) < separationDistance) {
          count = count + 1
          correction = correction.sub(xD).divide(separationDistance*4)
        }
      }
    }
    correction
  }

  private def collisionAvoidance2(cBoid: Boid): Vector2D = {
    var correction = new Vector2D
    var count = 0
    val cPosition = cBoid.nextPosition.copy()
    for (aBoid <- boids) {
      if (!(aBoid == cBoid)) {
        val aPosition = aBoid.nextPosition
        val xD = new Vector2D(aPosition.x - cPosition.x, aPosition.y - cPosition.y)
        if (Math.abs(xD.x) < separationDistance && Math.abs(xD.y) < separationDistance) {
          count = count + 1
          correction = cBoid.velocity.rotate(Math.PI/8.0)-cBoid.velocity
        }
      }
    }
    correction
  }


  private def collisionCheck(cBoid: Boid): Vector2D = {
    var correction = new Vector2D
    var count = 0
    val cPosition = cBoid.nextPosition.copy()
    for (aBoid <- boids) {
      if (!(aBoid == cBoid)) {
        if (cBoid.minDistance(aBoid).getOrElse(10000.0) < separationDistance * 2) {
          val newX = scala.util.Random.nextDouble() * 4.0 - 2.0
          val newY = scala.util.Random.nextDouble() * 4.0 - 2.0

          correction = correction + (Vector2D(newX, newX).divide(2.0))
        }
      }
    }
    correction.cap(maxVelocity, maxVelocity).negativeCap(maxVelocity, maxVelocity)
  }


  private def collisionCheck2(cBoid: Boid): Vector2D = {
    val closest = boids
      .map(b => (b, b.minDistance(cBoid)))
      .collect { case (x, Some(d)) => (x,d) }
      .sortWith((x, y) => x._2 < y._2)
      .headOption


    closest.flatMap{v =>



      val l = Range(0, 10, 1)
        .map( x =>  x * Math.PI / 180.0)
        .map(angle => (angle, cBoid.copy(velocity = cBoid.velocity.rotate(angle)).minDistance(v._1)))
        .collect { case (x, Some(d)) => (x,d) }
        .sortWith((x, y) => x._2 > y._2)






      for {
        (angle, distance) <- l.headOption
        newBoid = cBoid.copy(velocity = cBoid.velocity.rotate(angle))
      } yield
          cBoid.velocity - newBoid.velocity
      }
    .getOrElse(Vector2D.zero)
  }


  private def matchFlockVelocity(cBoid: Boid): Vector2D = {
    var perceivedVelocity = new Vector2D
    for (aBoid <- boids) {
      if (!(aBoid == cBoid)) perceivedVelocity = perceivedVelocity.add(aBoid.velocity)
    }
    perceivedVelocity = perceivedVelocity.divide(boids.size - 1)
    perceivedVelocity = perceivedVelocity.sub(cBoid.velocity)
    perceivedVelocity = perceivedVelocity.divide(matchFlockFactor)
    perceivedVelocity
  }


  private def bounding(cBoid: Boid): Vector2D

  = {
    var bound = new Vector2D
    val xMin = 0
    val xMax = worldSize.width
    val yMin = 0
    val yMax = worldSize.height
    val cPos = cBoid.position

    if (cPos.x < xMin)
      bound = bound.copy(x = bound.x + 1)
    else if (cPos.x > xMax)
      bound = bound.copy(x = bound.x - 1)

    if (cPos.y < yMin)
      bound = bound.copy(y = bound.y + 1)
    else if (cPos.y > yMax)
      bound = bound.copy(y = bound.y - 1)
    bound = bound.divide(boundingFactor)
    bound
  }


  private def positionTend(cBoid: Boid, position: Vector2D, factor: Double): Vector2D

  = {
    //Sample coordinates in the centre of the screen
    var tend = new Vector2D
    tend = position.sub(cBoid.position)
    tend = tend.divide(factor) //Movement factor moving the boid a percentage of the distance to the attration point
    tend
  }


}
