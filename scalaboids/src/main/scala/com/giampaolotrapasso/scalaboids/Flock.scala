package com.giampaolotrapasso.scalaboids

import com.giampaolotrapasso.scalaboids.utility.{Vector2D, WorldSize}


case class Flock(boids: Seq[Boid],
                 tendPlace: Vector2D,
                 config: Config,
                 tend: Boolean,
                 worldSize: WorldSize,
                 maxVelocity: Double,
                 minVelocity: Double,
                 avoidPoints: Seq[Vector2D]) {

  private def getNextAngle(current: Vector2D, next: Vector2D): Double = {
    val dX = next.x - current.x
    val dY = next.y - current.y
    val theta = Math.atan2(dY, dX)
    // Change this.angle is into degrees
    theta * 180 / Math.PI
  }


  def updatedBoidsPosition(): Seq[Boid] = {
    boids.map { boid =>
      val near = boids.filter(b => (b.position - boid.position).length < config.nearBoidDistance)

      val perceivedCenterOfMass = calculatePerceivedCenterOfMass(near, boid)
      val avoidOthers = avoidOtherBoids(boids, boid)
      val matchVelocity = matchOthersVelocity(near, boid)
      val tend = tendToPlace(boid)
      val avoid = avoidPlaces(avoidPoints, boid)

      val unlimitedVelocity: Vector2D =
        boid.velocity +
          perceivedCenterOfMass * (config.centerOfMass / 100000.0) +
          avoidOthers + // * (config.avoidOthers / 1000.0)
          matchVelocity  * (config.matchVelocity / 1000.0) +
          tend * (config.matchVelocity / 1000.0) +
          avoid //* (config.avoidObstacles / 1000.0)


      val limitedVelocity = limitVelocity(unlimitedVelocity)
      val nextPosition = wrapPosition(boid.position.add(limitedVelocity))

      val angle = getNextAngle(boid.position, nextPosition)


      Boid(position = nextPosition, velocity = limitedVelocity, angle = angle, worldSize = worldSize)
    }
  }

  def calculatePerceivedCenterOfMass(boids: Seq[Boid], boid: Boid) = {
    val l = boids
      .filter(b => b != boid)
      .filter(b => (b.position - boid.position).length < config.nearBoidDistance)
      .map(_.position)
      .fold(Vector2D.zero)((v1, v2) => v1 + v2)
    val m = if (boids.size > 1) l.divide(boids.size - 1) else l

    (m - boid.position)
  }

  def avoidOtherBoids(boids: Seq[Boid], boid: Boid): Vector2D = {
    var start = Vector2D.zero

    boids.filter(b => b != boid).map(_.position).foreach { position =>
      val distance = (position - boid.position).length
      if (distance < config.distanceBeetweenBoids) {
        val diff = (position - boid.position).divide(distance)
        start = start - diff
      }
    }
    start
  }

  def matchOthersVelocity(boids: Seq[Boid], boid: Boid) = {
    val l = boids
      .filter(b => b != boid)
      .filter(b => (b.position - boid.position).length < config.nearBoidDistance)
      .map(_.velocity).fold(Vector2D.zero)((v1, v2) => v1 + v2)

    val m = if (boids.size > 1) l.divide(boids.size - 1) else l
    (m - boid.velocity)
  }

  def tendToPlace(boid: Boid) = {
    val p = (tendPlace - boid.position)
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
    val distance = (place - boid.position).length
    if (distance < config.distanceBeetweenBoids) {
      val diff = place - boid.position
      start = start - diff.divide((distance))
    }
    start
  }


  def limitVelocity(velocity: Vector2D) = {
    if (velocity.length < minVelocity)
      (velocity / velocity.length) * minVelocity else if (velocity.length > maxVelocity)
      (velocity / velocity.length) * maxVelocity
    else velocity
  }


  def wrapPosition(position:Vector2D) = {
    var v = position

    if (position.x < worldSize.minX)
      v = v.copy(x = worldSize.maxX)

    if (position.x > worldSize.maxX)
      v = v.copy(x = worldSize.minX)
    
    if (position.y < worldSize.minY)
      v = v.copy(y = worldSize.maxY)

    if (position.y > worldSize.maxY)
      v = v.copy(y = worldSize.minY)

    v
  }



}
