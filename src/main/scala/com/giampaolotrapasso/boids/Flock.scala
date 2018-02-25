package com.giampaolotrapasso.boids

import com.giampaolotrapasso.boids.utility.{Vector2D, WorldSize}

import scala.util.Random
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.ImageView
import scalafx.scene.shape.Shape

case class Flock(boids: Seq[Boid], worldSize: WorldSize) {

  private val movementFactor = 1000
  private val boundingFactor = 10
  private val separationDistance = 15
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


      val perceivedCenterOfMass = calculatePerceivedCenterOfMass(boids, boid)
      val avoidOthers = avoidOtherBoids(boids, boid)
      val matchVelocity = matchOthersVelocity(boids, boid)

      val newVelocity: Vector2D = boid.velocity + perceivedCenterOfMass + avoidOthers + matchVelocity
      val nextPosition = boid.position.add(newVelocity).add(worldSize.toVector2D).normalize(worldSize.width, worldSize.height)

      val angle = setAngle(boid.position, nextPosition)

      val display = boid.display
      display.setX(nextPosition.x)
      display.setY(nextPosition.y)
      display.setSmooth(true)
      display.setCache(true)
      display.setRotate(angle)

      val circle = boid.circle
      circle.setCache(true)
      circle.setRotate(angle)
      circle.layoutX = nextPosition.x
      circle.layoutY = nextPosition.y


      Boid(
        position = nextPosition,
        velocity = newVelocity,
        angle = angle,
        display = display,
        worldSize = worldSize,
        circle = circle
      )
    }
  }

  def calculatePerceivedCenterOfMass(boids: Seq[Boid], boid: Boid) = {
    val l = boids.filter(b => b != boid).map(_.position).fold(Vector2D.zero)((v1, v2) => v1 + v2)
    val m = l.divide(boids.size - 1)

    (m - boid.position) / 100
  }

  def avoidOtherBoids(boids: Seq[Boid], boid: Boid): Vector2D = {
    var start = Vector2D.zero

    boids.filter(b => b != boid).map(_.position).foreach { position =>
      if (((Math.abs(position.x - boid.position.x)) < separationDistance) &&
        ((Math.abs(position.y - boid.position.y)) < separationDistance))
        start = start - (boid.position - position)
    }
    start
  }

  def matchOthersVelocity(boids: Seq[Boid], boid: Boid) = {
    val l = boids.filter(b => b != boid).map(_.velocity).fold(Vector2D.zero)((v1, v2) => v1 + v2)
    val m = l.divide(boids.size - 1)
    (m - boid.velocity) / 8
  }

  def images = boids.map(_.display)

  def canvas = boids.map(_.circle)


}
