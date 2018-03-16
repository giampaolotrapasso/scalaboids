package com.giampaolotrapasso.scalaboids

case class Config(distanceBeetweenBoids: Double = 20.0,
                  centerOfMass: Double = 5,
                  matchVelocity: Double = 40,
                  tendToPlace: Double = 40,
                  nearBoidDistance: Double = 100
                 )
