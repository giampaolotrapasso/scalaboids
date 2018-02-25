package com.giampaolotrapasso.boids

import com.giampaolotrapasso.boids.utility.{Vector2D, WorldSize}

import scalafx.scene.image.{Image, ImageView}

case class Boid(position: Vector2D, velocity: Vector2D, angle: Double, display: ImageView, worldSize: WorldSize) {
  def nextPosition = position.add(velocity).add(Vector2D(worldSize.width, worldSize.height)).module(worldSize.width, worldSize.height)

  def minTimeOfCollision(anotherBoid: Boid) = {
    val xa0 = position.x
    val xat = velocity.x
    val ya0 = position.y
    val yat = velocity.y

    val xb0 = anotherBoid.position.x
    val xbt = anotherBoid.velocity.x
    val yb0 = anotherBoid.position.y
    val ybt = anotherBoid.velocity.y

    val mintime =
      -(xa0 * xat - xat * xb0 - (xa0 - xb0) * xbt + ya0 * yat - yat * yb0 - (ya0 - yb0) * ybt) /
        (xat * xat - 2 * xat * xbt + xbt * xbt + yat * yat - 2 * yat * ybt + ybt * ybt)
    mintime
  }

  def minDistance(anotherBoid: Boid) : Option[Double] = {

    val xa0 = position.x
    val xat = velocity.x
    val ya0 = position.y
    val yat = velocity.y

    val xb0 = anotherBoid.position.x
    val xbt = anotherBoid.velocity.x
    val yb0 = anotherBoid.position.y
    val ybt = anotherBoid.velocity.y

    val mt = minTimeOfCollision(anotherBoid)

    if (mt > 0 && mt < 20) {

      val w = (mt * xat - mt * xbt + xa0 - xb0)
      val v = (mt * yat - mt * ybt + ya0 - yb0)

      Some(Math.sqrt(w * w + v * v))
    } else None
  }

}


object Boid {

  val image = new Image("images/boid.png")


}
