-- Дописать этот текст так, чтобы написанное ниже
-- компилировалось и работало

data Point = Point (Int, Int)

new = ($)
(->>) obj func = func obj
getX (Point (x, _)) = x
getY (Point (_, y)) = y
setX x (Point (_, y)) = Point (x, y)
setY y (Point (x, _)) = Point (x, y)

main = do
  let p0 = new Point (2, 3)  
      x  = p0->>getX
      y  = p0->>getY
      p1 = p0->>setX 8
      p2 = p1->>setY 9
  return ()
