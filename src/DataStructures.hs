module DataStructures where


  -- pozycja na planszy (x, y)
  data Point = Point Int Int deriving (Show, Read)
  
  -- stan gry, czyli rozklad pozycji poszczegolnych pionkow
  -- pierwszy element listy - pozycja wilka
  -- potem pozycje owiec
  type State = [Point] 
  
  -- drzewo gry reprezentujące możliwe stany gry, składa się kolejno z:
  -- stanu gry dla danego węzła
  -- poddrzew danego węzła
  -- wartości funkcji heurystycznej oceniającej dany węzeł
  data GameTree = GameTree State [GameTree] Int deriving (Show)
  
  -- stan gry po wykonaniu ruchu przez wilka (o ile był on możliwy)
  -- oprócz stanu planszy zwraca 2 flagi
  -- czy wilk wygrał
  -- czy owce wygrały
  data FinalState = FinalState State Bool Bool deriving (Show)
  
  -- funkcje zwracające poszczególne współrzędne
  xPoint :: Point -> Int
  xPoint (Point x _) = x
  
  yPoint :: Point -> Int
  yPoint (Point _ y) = y
  
  
  -- -- board position
  -- data Point = Point Int Int deriving (Show)
  
  -- -- get cordinates info
  -- xPoint :: Point -> Int
  -- xPoint (Point x _) = x
  
  -- yPoint :: Point -> Int
  -- yPoint (Point _ y) = y
  
  -- -- state of the game
  -- -- first point is for wolf
  -- -- 2nd - 5th for sheeps
  -- type State = [Point]
  