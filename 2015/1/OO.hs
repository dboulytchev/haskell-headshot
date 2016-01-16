data Point = Point (Int, Int)

getX (Point (x, _)) = x
getY (Point (_, y)) = y

setX x' (Point (x, y)) = Point (x', y) 
setY y' (Point (x, y)) = Point (x, y')

infixl 1 ->>

x ->> f = f x

new = ($)

main = do
  let p0 = new Point (2, 3)  
      x  = p0->>getX
      y  = p0->>getY
      p1 = p0->>setX 8
      p2 = p1->>setY 9
  return ()