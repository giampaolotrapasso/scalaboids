package com.giampaolotrapasso.boids

import com.giampaolotrapasso.boids.utility.Vector2D

import scala.language.postfixOps
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.image.ImageView
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import scalafx.scene.{Group, Scene}

object ScalaBoids extends JFXApp {

  private val initialAngle = 0.0

  val width = 1200.0
  val height = 800.0

  private val point = scalafx.scene.shape.Circle(width/2, height /2, 4)

  private val centroid = new Circle {
    centerX = 600
    centerY = 400
    radius = 2
    strokeWidth = 2
    fill = Color.IndianRed
  }

  private def center =  flock.boids.map(_.position).fold(Vector2D(0.0, 0.0)){
    (sum,element) => sum +element
  }.divide(flock.boids.size)

  private var flock = new Flock(
    Range(0, 40).map { _ =>
      val x = scala.util.Random.nextDouble * width + 1
      val y = scala.util.Random.nextDouble * height + 1

      val xVelocity = scala.util.Random.nextDouble() * 4.0 - 2.0
      val yVelocity = scala.util.Random.nextDouble() * 4.0 - 2.0

      val initialPosition = Vector2D(x, y)
      val initialVelocity = Vector2D(xVelocity, yVelocity)

      val display: ImageView = new ImageView(Boid.image)
      display.setImage(Boid.image)
      display.setX(initialPosition.x)
      display.setY(initialPosition.y)
      display.setSmooth(true)
      display.setCache(true)
      display.setRotate(0.0)

      Boid(initialPosition, initialVelocity, initialAngle, display)
    }, width, height)


  def pongComponents: Group = new Group {
    flock = Flock(flock.updatedBoidsPosition(), width, height)
    val c = center
    centroid.setCenterX(c.x)
    centroid.setCenterY(c.y)
    children = flock.images :+ point :+ centroid
    //println(s" ${flock.images(0).getX}, ${flock.images(0).getY}")
  }


  stage = new PrimaryStage {

    title = "B-O-I-D-S!"
    scene = new Scene(width.toDouble, height.toDouble) {
      content = pongComponents

      val timer = AnimationTimer(t => {
        content = pongComponents
      })

      timer.start()
    }


  }

}
