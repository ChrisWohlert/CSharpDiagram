{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}


module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Lib
import Class
import Drawing

main = do
    types <- parseFiles "C:/Users/CWO/source/github/CSharpDiagram/CSharpDiagram/CSharpDiagram"
    mainWith $ draw (Solution types)
