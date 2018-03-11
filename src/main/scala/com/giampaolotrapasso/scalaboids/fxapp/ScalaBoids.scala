package com.giampaolotrapasso.scalaboids.fxapp

import com.giampaolotrapasso.scalaboids.utility.{Vector2D, WorldSize}
import com.giampaolotrapasso.scalaboids.{Boid, Flock, fxapp}

import scala.util.Random
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{Button, CheckBox}
import scalafx.scene.layout._
import scalafx.scene.{Group, Scene}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line}
import scalafx.Includes._
import scalafx.event.ActionEvent
import scalafx.scene.input.MouseEvent


object ScalaBoids extends JFXApp {

  private val initialAngle = 0.0

  val width = 800.0
  val height = 800.0
  val worldSize = WorldSize(0, 0, width, height)
  private val maxVelocity = 5.0
  private val minVelocity = 4.0

  private var tendPoint = Circle(width / 2, height / 2, 4)
  private var tendPlace = Vector2D(width /2, height / 2)
  private var tend = true


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
    }, tendPlace, true, worldSize, maxVelocity, minVelocity, avoid)

  private var images = flock.boids.map { f =>
    boidShape(randomColor)
  }

  def randomColor(): Color = {
    Color.apply(Random.nextDouble, Random.nextDouble, Random.nextDouble, 1.0)
  }

  val elements = new Group {
    children = avoidCircles
  }

  def pongComponents: Group = new Group {
    flock = Flock(flock.updatedBoidsPosition(), tendPlace, tend, worldSize, maxVelocity, minVelocity, avoid)

    images.zip(flock.boids).foreach { case (image, boid) =>

      image.setCache(true)
      image.setRotate(boid.angle)
      image.layoutX = boid.position.x
      image.layoutY = boid.position.y

    }


    val c = center
    centroid.setCenterX(c.x)
    centroid.setCenterY(c.y)
    children = images :+ elements :+ tendPoint :+ centroid
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

  val innerPane = new Pane {}


  val pane = new Pane {
    // Add rectangle that will be updated with user interactions
    children = innerPane
  }

  val check = new CheckBox {
    text = "Tend"
    indeterminate = false
    selected = true
  }

  check.onAction = (event: ActionEvent) => {
    tend <== check.selected()
  }


  val topPane =
    new HBox {
      hgrow = Priority.Always
      alignmentInParent = Pos.CenterRight
      padding = Insets(10, 0, 10, 0)
      spacing = 10
      children = Seq(
        new Region {
          minWidth = 10
          maxWidth = Double.MaxValue
          hgrow = Priority.Always
        },
        check,
        new Button("View README") {
          prefWidth = 125
        },
        new Button {
          prefWidth = 125
          text = "Hello"
          defaultButton = true
        }
      )
    }


  topPane.setStyle("-fx-background-color: rgba(0, 100, 100, 1);")


  innerPane.children = pongComponents

  val borderPane = new BorderPane {
    center = innerPane
    top = topPane

  }


  innerPane.handleEvent(MouseEvent.Any) { me: MouseEvent => {
      me.eventType match {
        case MouseEvent.MousePressed => {
          tendPlace = Vector2D(me.x, me.y)
          // Reset the shape
          tendPoint = Circle(me.x, me.y, 4)
        }
        case _ => {}
      }
    }
  }

  val p = new Pane


  stage = new PrimaryStage {

    title = "B-O-I-D-S!"
    scene = new Scene(worldSize.width, worldSize.height) {

      root = borderPane

      val timer = AnimationTimer(t => {
        //updateFlock()
        innerPane.children = pongComponents
      })

      timer.start()
    }
  }


}
