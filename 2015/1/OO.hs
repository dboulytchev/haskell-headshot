-- Дописать этот текст так, чтобы написанное ниже
-- компилировалось и работало

main = do
  let p0 = new Point (2, 3)  
      x  = p0->>getX
      y  = p0->>getY
      p1 = p0->>setX 8
      p2 = p1->>setY 9
  return ()