package com.giampaolotrapasso.boids.utility

import scala.annotation.tailrec

case class Vector2D(x: Double = 0.0, y: Double = 0.0) {

  def add(other: Vector2D) = Vector2D(this.x + other.x, this.y + other.y)

  def +(other: Vector2D) = add(other)

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


  def normalize = {

    def isNormal(vector2D: Vector2D) = vector2D.x < 1.0 && vector2D.x >= 0 && vector2D.y < 1 && vector2D.y >= 0

    if (isNormal(this))
      this
    else {
      val xx = x.toInt
      val yy = y.toInt
      copy(x = this.x - xx, y = this.y - yy)
    }
  }

  def normalize(normX: Double = 1.0, normY: Double = 1.0) = {

    def isNormal(vector2D: Vector2D) = vector2D.x < normX && vector2D.x > -normX && vector2D.y < normY && vector2D.y > -normY

    if (isNormal(this)) {
      println("x,y= " + this.x + ", " + this.y)
      this
    }
    else {
      val xx = this.x % normX
      val yy = this.y % normY

      copy(x = xx, y = yy)
    }
  }

}

object Vector2D {
  val zero = Vector2D(0.0, 0.0)
}
