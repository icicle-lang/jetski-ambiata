import           Disorder.Core.Main

import qualified Test.Jetski

main :: IO ()
main =
  disorderMain [
      Test.Jetski.tests
    ]
