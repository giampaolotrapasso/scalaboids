package com.giampaolotrapasso.scalaboids

import com.giampaolotrapasso.scalaboids.utility.{Vector2D, WorldSize}

case class Boid(position: Vector2D, velocity: Vector2D, angle: Double, worldSize: WorldSize)
