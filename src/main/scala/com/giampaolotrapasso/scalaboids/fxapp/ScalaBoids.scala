package com.giampaolotrapasso.scalaboids.fxapp

import com.giampaolotrapasso.scalaboids.utility.{Vector2D, WorldSize}
import com.giampaolotrapasso.scalaboids.{Boid, Config, Flock}

import scala.util.Random
import scalafx.Includes._
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.collections.ObservableBuffer
import scalafx.event.ActionEvent
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{CheckBox, ChoiceBox, Label, Slider}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line}
import scalafx.scene.{Group, Scene}
import MouseOption._


object ScalaBoids extends JFXApp {

  private val initialAngle = 0.0

  val width = 800.0
  val height = 800.0
  val worldSize = WorldSize(0, 0, width, height)
  private val maxVelocity = 5.0
  private val minVelocity = 4.0

  private var tendPoint = Circle(width / 2, height / 2, 4)
  private var tendPlace = Vector2D(width / 2, height / 2)
  private var tend = true
  private var config = Config()

  private def barrier = Range(1, 100).map { i => Vector2D(x = 100 + i, y = 200) }

  val avoid = barrier

  private def avoidCircles = avoid.map(v =>
    new Circle {
      centerX = v.x
      centerY = v.y
      radius = 6
      strokeWidth = 2
      fill = Color.IndianRed
    })

  private def center = flock.boids.map(_.position).fold(Vector2D(0.0, 0.0)) {
    (sum, element) => sum + element
  }.divide(flock.boids.size)

  private def generateBoids(number: Int) = {
    Range(0, number).map {
      _ =>
        val x = scala.util.Random.nextDouble * width + 1
        val y = scala.util.Random.nextDouble * height + 1

        val xVelocity = (scala.util.Random.nextDouble() * (maxVelocity - minVelocity) + minVelocity) * (if (scala.util.Random.nextBoolean()) 1 else -1)
        val yVelocity = (scala.util.Random.nextDouble() * (maxVelocity - minVelocity) + minVelocity) * (if (scala.util.Random.nextBoolean()) 1 else -1)

        val initialPosition = Vector2D(x, y)
        val initialVelocity = Vector2D(xVelocity, yVelocity)


        Boid(initialPosition, initialVelocity, initialAngle, worldSize)
    }
  }

  private var flock = new Flock(generateBoids(20), tendPlace, config, true, worldSize, maxVelocity, minVelocity, avoid)

  private var images = makeImages(flock.boids.size)

  private def makeImages(number: Int) = {
    Range(0, number).map { f =>
      boidShape(randomColor)
    }
  }

  private def randomColor(): Color = {
    Color.apply(Random.nextDouble, Random.nextDouble, Random.nextDouble, 1.0)
  }

  val elements = new Group {
    children = avoidCircles
  }

  def pongComponents: Group = new Group {
    config = config
      .copy(distanceBeetweenBoids = boidDistanceSlider.value.toInt)
      .copy(centerOfMass = centerOfMassSlider.value.toInt)
      .copy(matchVelocity = matchVelocitySlider.value.toInt)
      .copy(nearBoidDistance = nearBoidDistance.value.toInt)
      .copy(tendToPlace = tendToPlaceSlider.value.toInt)

    label.text = config.toString.replace(",", ",\n")

    val currentNumberOfBoids = numberOfBoidsSlider.value.toInt

    if (flock.boids.size > currentNumberOfBoids) {
      flock = flock.copy(boids = flock.boids.take(currentNumberOfBoids))
      images = images.take(currentNumberOfBoids)
    }
    else if (flock.boids.size < currentNumberOfBoids) {
      val temp = flock.boids.size

      flock = flock.copy(boids = flock.boids ++ generateBoids(currentNumberOfBoids - temp))
      images = images ++ makeImages(currentNumberOfBoids - temp)
    }

    flock = Flock(flock.updatedBoidsPosition(), tendPlace, config, tend, worldSize, maxVelocity, minVelocity, avoid)
    images.zip(flock.boids).foreach { case (image, boid) =>

      image.setCache(true)
      image.setRotate(boid.angle)
      image.layoutX = boid.position.x
      image.layoutY = boid.position.y
    }

    val c = center
    children = images :+ elements :+ tendPoint
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
    text = "Tend to place"
    indeterminate = false
    selected = true
  }

  val choicebox = new ChoiceBox[String] {
    maxWidth = 80
    maxHeight = 50
    items = ObservableBuffer(Nothing, FollowPointer, AvoidPointer)
    selectionModel().selectFirst()
  }


  def slider(minValue: Double,
             maxValue: Double,
             defaultValue: Double) = new Slider() {
    prefWidth = 125
    min = minValue
    max = maxValue
    value = defaultValue
  }


  def label(labelText: String) = new Label {
    text = labelText
  }

  val boidDistanceSlider = slider(16, 50, 23)
  val centerOfMassSlider = slider(0, 100, 3)
  val matchVelocitySlider = slider(0, 1000, 5)
  val tendToPlaceSlider = slider(0, 1000, 500)
  val nearBoidDistance = slider(10, worldSize.width, 100)
  val numberOfBoidsSlider = slider(5, 100, 20)


  val label = new Label {
    wrapText = true
  }

  val leftPane =
    new VBox {
      hgrow = Priority.Always
      alignmentInParent = Pos.CenterRight
      padding = Insets(10, 10, 10, 10)
      spacing = 10
      children = Seq(
        new Region {
          minWidth = 10
          maxWidth = Double.MaxValue
          hgrow = Priority.Always
        },
        check,
        label("Boids distance"),
        boidDistanceSlider,
        label("Center of mass"),
        centerOfMassSlider,
        label("Match velocity"),
        matchVelocitySlider,
        label("Tend to place"),
        tendToPlaceSlider,
        label("Near boids"),
        nearBoidDistance,
        label("Number of boids"),
        numberOfBoidsSlider,
        label("Mouse function"),
        choicebox,
        label

      )
    }

  check.onAction = (event: ActionEvent) => {
    tend <== check.selected()
  }

  leftPane.setStyle("-fx-background-color: rgba(150, 150, 150, 1);")

  innerPane.children = pongComponents

  val borderPane = new BorderPane {
    center = innerPane
    left = leftPane
  }

  innerPane.handleEvent(MouseEvent.Any) { me: MouseEvent =>
    me.eventType match {
      case MouseEvent.MousePressed => {
        tendPlace = Vector2D(me.x, me.y)
        // Reset the shape
        tendPoint = Circle(me.x, me.y, 4)
      }
      case MouseEvent.MouseMoved => {
        if (choicebox.value.value == MouseOption.FollowPointer) {
          tend = true
          tendPlace = Vector2D(me.x, me.y)
        } else if (choicebox.value.value == MouseOption.AvoidPointer) {
          tend = false
          tendPlace = Vector2D(me.x, me.y)
        } else {
          tend = true
          tendPlace = Vector2D(tendPoint.centerX.value, tendPoint.centerY.value)
        }

      }
      case _ => {}
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

  stage.setResizable(false)
  stage.setMinWidth(945)


}
