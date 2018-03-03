package com.giampaolotrapasso.boids

import com.giampaolotrapasso.boids.utility.{Vector2D, WorldSize}

import scalafx.scene.Group

case class Boid(position: Vector2D, velocity: Vector2D, angle: Double, worldSize: WorldSize, circle: Group)
