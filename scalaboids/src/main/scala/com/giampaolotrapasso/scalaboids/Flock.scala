package com.giampaolotrapasso.scalaboids

import com.giampaolotrapasso.scalaboids.utility.{Vector2D, WorldSize}


case class Flock(boids: Seq[Boid],
                 tendPlace: Vector2D,
                 tend: Boolean,
                 worldSize: WorldSize,
                 maxVelocity: Double,
                 minVelocity: Double,
                 avoidPoints: Seq[Vector2D]) {

  private val separationDistance = 20

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
      val near = boids.filter(b => (b.position - boid.position).norm < 100)

      val perceivedCenterOfMass = calculatePerceivedCenterOfMass(near, boid)
      val avoidOthers = avoidOtherBoids(boids, boid)
      val matchVelocity = matchOthersVelocity(near, boid)
      val tend = tendToPlace(boid)
      val avoid = avoidPlaces(avoidPoints, boid)

      val unlimitedVelocity: Vector2D =
        boid.velocity +
          perceivedCenterOfMass * 0.005 +
          avoidOthers * 0.01 +
          matchVelocity * 0.2 +
          tend * 0.001 +
          avoid * 1.1


      val limitedVelocity = limitVelocity(unlimitedVelocity)
      val nextPosition = boundPosition(boid.position.add(limitedVelocity))

      val angle = getNextAngle(boid.position, nextPosition)


      Boid(position = nextPosition, velocity = unlimitedVelocity, angle = angle, worldSize = worldSize)
    }
  }

  def calculatePerceivedCenterOfMass(boids: Seq[Boid], boid: Boid) = {
    val l = boids.filter(b => b != boid).map(_.position).fold(Vector2D.zero)((v1, v2) => v1 + v2)
    val m = if (boids.size > 1) l.divide(boids.size - 1) else l

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
    val m = if (boids.size > 1) l.divide(boids.size - 1) else l
    (m - boid.velocity) / 8
  }

  def tendToPlace(boid: Boid) = {
    val p = (tendPlace - boid.position) / 100
    if (tend)
      p
    else
      p * -1
  }

  def avoidPlaces(avoidPoints: Seq[Vector2D], boid: Boid) = {
    avoidPoints.fold(Vector2D.zero)((sum, place) => sum + avoidPlace(place, boid))
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

    if (position.x < worldSize.minX + 200)
      v = v.copy(x = v.x + 1)

    if (position.x > worldSize.maxX - 200)
      v = v.copy(x = v.x - 1)


    if (position.y < worldSize.minY + 200)
      v = v.copy(y = v.y + 1)

    if (position.x > worldSize.maxY - 200)
      v = v.copy(y = v.y - 1)

    v
  }


}
