module Main where

import Ex2

facePic :: Picture RGBA
facePic = face `cut` everywhere saffron `over` everywhere black
  where
    saffron = MkRGBA (244/255) (208/255) (63/255) 1.0

    smile   = circle 50 `pictureAND` pictureNOT (circle 50 `at` (0,10))
    eyebrow = flipTopBottom smile `scale` 0.5
    eye     = circle 50 `scale` 0.3

    face  = circle 100
      `pictureAND` pictureNOT (smile   `at` (0,-30))
      `pictureAND` pictureNOT (eyebrow `at` (-30,30))
      `pictureAND` pictureNOT (eyebrow `at` (30,30))
      `pictureAND` pictureNOT (eye     `at` (-30,25))
      `pictureAND` pictureNOT (eye     `at` (30,25))

main = do
  writeBMP "face.bmp" facePic
  putStrLn "'face.bmp' written"
