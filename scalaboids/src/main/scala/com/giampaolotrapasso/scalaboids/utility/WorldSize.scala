package com.giampaolotrapasso.scalaboids.utility

case class WorldSize(minX: Double, minY: Double, maxX: Double, maxY: Double) {

  def center = Vector2D((minX+maxY)/2, (minY+maxY)/2)

  def width = minX+maxX
  def height = minY+maxY
}
