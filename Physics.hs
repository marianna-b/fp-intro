{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Physics where

import Base
import Algebra


-- Движение по прямой согласно законам Ньютона

type Mass = Double
type Coordinate = Double
type Velocity = Double
type Acceleration = Double
type Time = Double
type Force = Double

data Body = Body { m :: Mass, 
                   x :: Coordinate, 
                   v :: Velocity,
                   a :: Acceleration } 

-- Координата, в которой окажется тело b через время t
destination :: Body -> Time -> Coordinate
destination b t = (x b) + t * (v b) + t * t * (a b) * (inverted 2)

-- Скорость тела  b через время t
velocity :: Body -> Time -> Velocity
velocity b t = (v b) + (a b) * t

-- Ускорение тела b под действием силы f

acceleration :: Body -> Acceleration
acceleration b = a b 


-- Координата, в которой окажется тело b через время t под действием силы f
destinationWithForce :: Body -> Force -> Time -> Coordinate
destinationWithForce b f t = (x b) + t * (v b) + (a b + (getA f (m b))) * t * t * (inverted 2)

-- Скорость тела  b через время t под действием силы f
velocityWithForce :: Body -> Force -> Time -> Velocity
velocityWithForce b f t = (v b) + (a b + (getA f (m b))) * t

-- Ускорение тела b под действием силы f

accelerationWithForce :: Body -> Force -> Acceleration
accelerationWithForce b f = (a b + (getA f (m b)))


-- Увеличение ускорения тела с массой m под действием силы f
getA :: Force -> Mass -> Acceleration
getA f m = f * (inverted m)


--Тело b через время t
move :: Body -> Time -> Body
move b t = b { x = destination b t }

-- Тело b через время t при действии на него силы f
moveWithForce :: Body -> Force -> Time -> Body
moveWithForce b f t = b { x = destinationWithForce b f t, v = velocityWithForce b f t, a = accelerationWithForce b f }
