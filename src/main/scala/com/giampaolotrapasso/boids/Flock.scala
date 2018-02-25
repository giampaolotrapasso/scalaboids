package com.giampaolotrapasso.boids

import com.giampaolotrapasso.boids.utility.{Vector2D, WorldSize}

import scala.util.Random
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.ImageView
import scalafx.scene.shape.Shape

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



      val newVelocity: Vector2D = boid.velocity
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

  def images = boids.map(_.display)

  def canvas = boids.map(_.circle)




}
