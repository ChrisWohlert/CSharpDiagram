{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}


module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import CSParser
import Class
import Drawing
import System.IO
import Data.List.Split
import qualified System.IO.Strict as S

main = do
    print "Parsing"
    --solution <- parseFiles "D:/haskell/CSharpDiagram/CSharpDiagram"
    solution <- parseFiles "C:/Users/CWO/source/github/CSharpDiagram/CSharpDiagram/CSharpDiagram"
    print "Creating diagram"
    renderSVG "test.svg" (dims 1600) (draw solution)
    print "Updating index.html"
    handle <- openFile "test.svg" ReadMode
    contents <- hGetContents handle
    html <- S.readFile "index.html"
    let (start:svg:end:[]) = splitOn "<!-- SVG -->" html
    let result = (start ++ "<!-- SVG -->" ++ contents ++ "<!-- SVG -->" ++ end) 
    writeFile "index.html" result

