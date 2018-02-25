package com.giampaolotrapasso.boids.utility

case class WorldSize(width: Int, height: Int) {
  def toVector2D = Vector2D(width, height)

  def center = Vector2D(width/2, height/2)
}
