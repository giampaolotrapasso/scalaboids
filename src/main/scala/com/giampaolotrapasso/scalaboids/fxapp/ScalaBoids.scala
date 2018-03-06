package com.giampaolotrapasso.scalaboids.fxapp

import com.giampaolotrapasso.scalaboids.utility.{Vector2D, WorldSize}
import com.giampaolotrapasso.scalaboids.{Boid, Flock, fxapp}


import scala.util.Random
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.{Group, Scene}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line}



object ScalaBoids extends JFXApp {

  private val initialAngle = 0.0

  val width =800.0
  val height = 800.0
  val worldSize = WorldSize(0, 0, width, height)
  private val maxVelocity = 5.0
  private val minVelocity = 4.0

  private val point = Circle(width / 2, height / 2, 4)


  private def barrier = Range(1, 10).map { i =>
    Vector2D(
      x = 100 + i * 8, // scala.util.Random.nextDouble() * width,
      y = 200 // scala.util.Random.nextDouble() * height
    )
  }

  val avoid = barrier

  private def avoidCircles = avoid.map(v =>
    new Circle {
      centerX = v.x
      centerY = v.y
      radius = 6
      strokeWidth = 2
      fill = Color.IndianRed
    })


  private val centroid = new Circle {
    centerX = worldSize.center.x
    centerY = worldSize.center.y
    radius = 2
    strokeWidth = 2
    fill = Color.Green
  }

  private def center = flock.boids.map(_.position).fold(Vector2D(0.0, 0.0)) {
    (sum, element) => sum + element
  }.divide(flock.boids.size)

  private var flock = new Flock(
    Range(0, 20).map {
      _ =>
        val x = scala.util.Random.nextDouble * width + 1
        val y = scala.util.Random.nextDouble * height + 1

        val xVelocity = (scala.util.Random.nextDouble() * (maxVelocity - minVelocity) + minVelocity) * (if (scala.util.Random.nextBoolean()) 1 else -1)
        val yVelocity = (scala.util.Random.nextDouble() * (maxVelocity - minVelocity) + minVelocity) * (if (scala.util.Random.nextBoolean()) 1 else -1)

        val initialPosition = Vector2D(x, y)
        val initialVelocity = Vector2D(xVelocity, yVelocity)


        Boid(initialPosition, initialVelocity, initialAngle, worldSize)
    }, worldSize, maxVelocity, minVelocity, avoid)

  private var images = flock.boids.map{f =>
    boidShape(randomColor)
  }

  def randomColor(): Color = {
    Color.apply(Random.nextDouble, Random.nextDouble, Random.nextDouble, 1.0)
  }

  val elements = new Group {
    children = avoidCircles
  }

  def pongComponents: Group = new Group {
    flock = Flock(flock.updatedBoidsPosition(), worldSize, maxVelocity, minVelocity, avoid)

    images.zip(flock.boids).foreach{ case (image, boid) =>

      image.setCache(true)
      image.setRotate(boid.angle)
      image.layoutX = boid.position.x
      image.layoutY = boid.position.y

    }


    val c = center
    centroid.setCenterX(c.x)
    centroid.setCenterY(c.y)
    children = images :+ elements :+ point :+ centroid
  }


  def boidShape(color: Color): Group = {
    val g = new Group
    val c = Circle(7, color)

    val m = Line(7, 0, 10, 0)
    m.strokeWidth = 2
    m.stroke = color

    g.children = List(c, m)
    g
  }


  stage = new PrimaryStage {

    title = "B-O-I-D-S!"
    scene = new Scene(worldSize.width, worldSize.height) {
      content = pongComponents

      val timer = AnimationTimer(t => {
        content = pongComponents
      })

      timer.start()
    }


  }

}
