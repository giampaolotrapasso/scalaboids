package com.giampaolotrapasso.boids

import com.giampaolotrapasso.boids.utility.{Vector2D, WorldSize}

import scala.language.postfixOps
import scala.util.Random
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.ImageView
import scalafx.scene.paint.{Color, Paint}
import scalafx.scene.shape.{Circle, Line}
import scalafx.scene.{Group, Scene}

object ScalaBoids extends JFXApp {

  private val initialAngle = 0.0

  val width = 600.0
  val height = 600.0
  val worldSize = WorldSize(width.toInt, height.toInt)

  private val point = scalafx.scene.shape.Circle(width/2, height /2, 4)

  private val centroid = new Circle {
    centerX = worldSize.center.x
    centerY = worldSize.center.y
    radius = 2
    strokeWidth = 2
    fill = Color.IndianRed
  }

  private def center =  flock.boids.map(_.position).fold(Vector2D(0.0, 0.0)){
    (sum,element) => sum +element
  }.divide(flock.boids.size)

  private var flock = new Flock(
    Range(0, 30).map { _ =>
      val x = scala.util.Random.nextDouble * width + 1
      val y = scala.util.Random.nextDouble * height + 1

      val xVelocity = (scala.util.Random.nextDouble()*0.8+0.2)*(if (scala.util.Random.nextBoolean()) 1 else -1)
      val yVelocity = (scala.util.Random.nextDouble()*0.8+0.2)*(if (scala.util.Random.nextBoolean()) 1 else -1)

      val initialPosition = Vector2D(x, y)
      val initialVelocity = Vector2D(xVelocity, yVelocity)


      val display: ImageView = new ImageView(Boid.image)
      display.setImage(Boid.image)
      display.setX(initialPosition.x)
      display.setY(initialPosition.y)
      display.setSmooth(true)
      display.setCache(true)
      display.setRotate(0.0)


      Boid(initialPosition, initialVelocity, initialAngle, display, worldSize, boidShape(randomColor))
    }, worldSize)

  def randomColor(): Color = {
    Color.apply(Random.nextDouble, Random.nextDouble, Random.nextDouble, 1.0)
  }

  def pongComponents: Group = new Group {
    flock = Flock(flock.updatedBoidsPosition(), worldSize)
    val c = center
    centroid.setCenterX(c.x)
    centroid.setCenterY(c.y)
    children = flock.canvas :+ point :+ centroid
    //println(s" ${flock.images(0).getX}, ${flock.images(0).getY}")
  }



  def boidShape(color: Color) = {
    val g = new Group
    val c = Circle(7, color)

    val m = Line(7, 0, 10, 0)
    m.strokeWidth = 2
    m.stroke = color

    g.children = List(c , m)
    g
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
