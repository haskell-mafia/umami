import           Disorder.Core.Main

import qualified Test.Umami.Pretty.StripWhite

main :: IO ()
main =
  disorderMain
  [ Test.Umami.Pretty.StripWhite.tests ]
