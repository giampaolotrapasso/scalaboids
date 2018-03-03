package com.giampaolotrapasso.boids

import java.util.Date

import com.giampaolotrapasso.boids.utility.{Vector2D, WorldSize}

import scalafx.scene.Group

case class Flock(boids: Seq[Boid], worldSize: WorldSize, maxVelocity: Double, minVelocity: Double, avoidPoints: Seq[Vector2D]) {

  private val movementFactor = 1000
  private val boundingFactor = 10
  private val separationDistance = 20
  private val separationFactor = 200

  private val tendFactor = 5000


  private val matchFlockFactor = 8.0


  private def getNextAngle(current: Vector2D, next: Vector2D): Double = {
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
      val tend = tendToPlace(boid)
      val avoid = avoidPlaces(avoidPoints, boid)

      val unlimitedVelocity: Vector2D =
        boid.velocity +
          perceivedCenterOfMass * 0.0005 +
          avoidOthers * 0.01 +
          matchVelocity * 0.2 +
          tend * 0.001 +
          avoid * 0.6


      val limitedVelocity = limitVelocity(unlimitedVelocity)
      val nextPosition = boid.position.add(limitedVelocity)

      val angle = getNextAngle(boid.position, nextPosition)

      val circle = boid.circle
      circle.setCache(true)
      circle.setRotate(angle)
      circle.layoutX = nextPosition.x
      circle.layoutY = nextPosition.y


      Boid(position = nextPosition, velocity = unlimitedVelocity, angle = angle, worldSize = worldSize, circle = circle)
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
      if ((position - boid.position).norm < separationDistance)
        start = start - (position - boid.position) / 10.0
    }
    start
  }

  def matchOthersVelocity(boids: Seq[Boid], boid: Boid) = {
    val l = boids.filter(b => b != boid).map(_.velocity).fold(Vector2D.zero)((v1, v2) => v1 + v2)
    val m = l.divide(boids.size - 1)
    (m - boid.velocity) / 8
  }

  def tendToPlace(boid: Boid) = {
    val place = Vector2D(300, 300)
    (place - boid.position) / 100
  }

  def avoidPlaces(avoidPoints: Seq[Vector2D], boid: Boid) = {
    var center = Vector2D.zero

    avoidPoints.foreach(place =>
      center = center + avoidPlace(place, boid)
    )
    center
    /*
    val center: Vector2D = avoidPoints.fold(Vector2D.zero) {
      (sum, place) => sum + avoidPlace(place, boid)
    }
    center
    */
  }


  def avoidPlace(place: Vector2D, boid: Boid) = {
    var start = Vector2D.zero

    if ((place - boid.position).norm < separationDistance + 6) {
      start = start - ((place - boid.position) / 10.0)
    }
    start
  }


  def limitVelocity(velocity: Vector2D) = {
    if (velocity.norm < minVelocity)
      (velocity / velocity.norm) * minVelocity else if (velocity.norm > maxVelocity)
      (velocity / velocity.norm) * maxVelocity
    else velocity
  }


  def boundPosition(position: Vector2D) = {
    var v = position

    if (position.x < worldSize.minX + 100)
      v = v.copy(x = v.x + 1)

    if (position.x > worldSize.maxX - 100)
      v = v.copy(x = v.x - 1)


    if (position.y < worldSize.minY + 100)
      v = v.copy(y = v.y + 1)

    if (position.x > worldSize.maxY - 100)
      v = v.copy(y = v.y - 1)

    v
  }

  def canvas: Seq[Group] = boids.map(_.circle)


}
