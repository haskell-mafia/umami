{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import              Umami.Pretty
import qualified    Umami.Pretty.Render  as PR

import qualified    Umami.Pretty.Simple  as PS
import qualified    Umami.Pretty.Tabular as PT

import              P
import qualified    Data.Text.IO as TIO
import System.IO (IO, putStrLn)

main :: IO ()
main
 = do   putStrLn (show $ toTabs doc)
        TIO.putStr (render doc)

toTabs :: Doc a -> [[Text]]
toTabs
 = PR.textTabsOfDoc defaultRenderOptions
 . PT.tabDocOfSimpleDoc
 . PS.simpleDocOfDoc

render :: Doc a -> Text
render = PR.renderTabularLayout defaultRenderOptions


doc :: Doc ()
doc
 = vcat
 [ "int main()"
 , "{"
 , indent
 $ vcat
   [ decl "int" "x" "123"
   , decl "double" "yo" "10"
   , decl "long" "zob" "500"
   , assign "zob" "500"
   , assign "x" "x"
   , line
   , assign "xipip" "123"
   , assign "yo" "10"
   , assign "zob" "500"
   , assign "zob" "500"
   , assign "x" "x"
   , pluseq "x" "y"
   ]
 , "}"
 ]
 where
  decl t l r
   = t <#> l <#> "=" <#> r <> ";"

  assign l r
   = l <#> "" <#> "=" <#> r <> ";"
  pluseq l r
   = l <#> "" <#> "+=" <#> r <> ";"
