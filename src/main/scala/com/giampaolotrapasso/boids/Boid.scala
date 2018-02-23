package com.giampaolotrapasso.boids

import com.giampaolotrapasso.boids.utility.Vector2D

import scalafx.scene.image.{Image, ImageView}

case class Boid(position: Vector2D, velocity: Vector2D, angle: Double, display: ImageView)


object Boid {

  val image = new Image("images/boid.png")


}
