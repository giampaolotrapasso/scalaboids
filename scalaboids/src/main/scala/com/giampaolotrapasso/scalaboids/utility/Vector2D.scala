package com.giampaolotrapasso.scalaboids.utility

case class Vector2D(x: Double = 0.0, y: Double = 0.0) {

  def add(other: Vector2D) = Vector2D(this.x + other.x, this.y + other.y)

  def +(other: Vector2D) = add(other)

  def *(double: Double) = multiply(double)

  def sub(other: Vector2D) = Vector2D(this.x - other.x, this.y - other.y)

  def -(other: Vector2D) = sub(other)

  def divide(divider: Double): Vector2D = {
    Vector2D(x / divider, y / divider)
  }

  def multiply(multiplier: Double): Vector2D = {
    Vector2D(x * multiplier, y * multiplier)
  }


  def divide(divider: Vector2D): Vector2D = {
    Vector2D(x / divider.x, y / divider.y)
  }

  def /(divider: Double) = divide(divider)

  def cap(maxX: Double, maxY: Double) = Vector2D(
    x = if (x < maxX) x else maxX,
    y = if (y < maxY) y else maxY
  )

  def negativeCap(maxX: Double, maxY: Double) = Vector2D(
    x = if (x < -maxX) -maxX else x,
    y = if (y < -maxY) -maxY else y
  )

  def module(xModule: Int, yModule: Int) = Vector2D(
    x = (x.toInt % xModule),
    y = (y.toInt % yModule)
  )

  def rotate(angle: Double) = Vector2D(
    x = x * Math.cos(angle) - y * Math.sin(angle),
    y = x * Math.sin(angle) + y * Math.cos(angle)
  )

  def length = Math.sqrt(x * x + y * y)

  def normalize = this.divide(this.length)



}

object Vector2D {
  val zero = Vector2D(0.0, 0.0)
}
