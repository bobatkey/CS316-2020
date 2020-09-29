{- Questions:

   1. Multiple choice: in the declaration 'Direction = ...'
   'Direction' is (a) a datatype name; (b) a name of a defined value;
   (c) a constructor. Similar for other ones.

   1. Define flipHorizontally

   2. Define isHorizontal

   3. Define isClockwiseOf
-}

-- Solutions

flipHorizontally :: Direction -> Direction
flipHorizontally Up = Up
flipHorizontally Down = Down
flipHorizontally Left = Right
flipHorizontally Right = Left

isHorizontal :: Direction -> Bool
isHorizontal Left  = True
isHorizontal Right = True
isHorizontal Up    = False
isHorizontal Down  = False

isClockwiseOf :: Direction -> Direction -> Bool
isClockwiseOf Up    Right = True
isClockwiseOf Right Down  = True
isClockwiseOf Down  Left  = True
isClockwiseOf Left  Up    = True
isClockwiseOf _     _     = False
