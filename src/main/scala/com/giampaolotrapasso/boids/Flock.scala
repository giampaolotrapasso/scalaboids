package com.giampaolotrapasso.boids

import com.giampaolotrapasso.boids.utility.Vector2D

import scalafx.scene.image.ImageView

case class Flock(boids: Seq[Boid], width: Double, height: Double) {

  private val movementFactor = 1000
  private val boundingFactor = 10
  private val separationDistance = 25
  private val separationFactor = 200


  private val tendFactor = 5000


  private val maxVelocity = 2.0

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
      val avoid = collisionAvoidance(boid)
      val tend = positionTend(boid, Vector2D(width/2, height/2), factor = tendFactor)
      val matchVelocity = matchFlockVelocity(boid)
      val sum = avoid

      // println(s"G $group B: $bounds")
      //+ collisionAvoidance(boid) + matchFlockVelocity(boid)  +
      val newVelocity = boid.velocity.add(sum).cap(maxVelocity, maxVelocity)
      val nextPosition = boid.position.add(newVelocity).add(Vector2D(width, height)).module(width.toInt, height.toInt)

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
        display = display
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
    val cPosition = cBoid.position.copy()
    for (aBoid <- boids) {
      if (!(aBoid == cBoid)) {
        val aPosition = aBoid.position
        val xD = new Vector2D(aPosition.x - cPosition.x, aPosition.y - cPosition.y)
        if (Math.abs(xD.x) < separationDistance && Math.abs(xD.y) < separationDistance) correction = correction.sub(xD).divide(separationFactor)
      }
    }
    correction
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


  private def bounding(cBoid: Boid): Vector2D = {
    var bound = new Vector2D
    val xMin = 0
    val xMax = width
    val yMin = 0
    val yMax = height
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


  private def positionTend(cBoid: Boid, position: Vector2D, factor: Double): Vector2D = {
    //Sample coordinates in the centre of the screen
    var tend = new Vector2D
    tend = position.sub(cBoid.position)
    tend = tend.divide(factor) //Movement factor moving the boid a percentage of the distance to the attration point
    tend
  }


}
