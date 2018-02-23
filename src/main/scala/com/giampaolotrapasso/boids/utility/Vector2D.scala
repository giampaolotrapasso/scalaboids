package com.giampaolotrapasso.boids.utility

case class Vector2D(x: Double = 0.0, y: Double = 0.0) {

  def add(other: Vector2D) = Vector2D(this.x + other.x, this.y + other.y)

  def +(other: Vector2D) = add(other)

  def sub(other: Vector2D) = Vector2D(this.x - other.x, this.y - other.y)

  def -(other: Vector2D) = sub(other)

  def divide(divider: Double): Vector2D = {
    Vector2D(x / divider, y / divider)
  }

  def /(divider: Double) = divide(divider)

  def cap(maxX: Double, maxY: Double) = Vector2D(
    x = if (x < maxX) x else maxX,
    y = if (y < maxY) y else maxY
  )

  def module(xModule: Int, yModule: Int) = Vector2D(
    x = (x.toInt % xModule),
    y = (y.toInt % yModule)
  )

}

object Vector2D {
  val zero = Vector2D(0.0, 0.0)
}
