{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Drawing where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Lib
import Class
import Data.List
import Packer

class Drawing a where
    draw :: a -> Diagram B

instance Packable Type where
    packingDims (Class _ _ _ _ _ _ _ _ _ members _) = (30, 4 + (fromIntegral $ sum $ map getMemberHeight members))
    packingDims (Enum _ _ _ _ _ _) = (30, 10)

getMemberHeight m = 1 + (length $ getParameters m)

dashed  = dashingN [0.03,0.03] 0

instance Drawing Solution where
    draw (Solution types) = header === travelPacked (frame 2 . draw) (|||) (===) (pack types)
        where
            header = frame 5 $ fontSize (normalized 0.05) $ p $ "Number of classes: " ++ (show $ length types)

instance Drawing Type where
    draw c@(Class _ _ _ _ _ _ name _ _ members _) = contents <> bounds
        where 
            bounds = boundingRect (contents # frame 1) # lw 0.6
            contents = strutY 1 === vsep memberSpace ([draw name] ++ map draw members) 
    draw _ = strutY 10

instance Drawing ClassName where
    draw (ClassName name) = h1 name
    draw (GenericClassName name t) = h1 (name ++ "<" ++ t ++ ">")

instance Drawing Member where
    draw (Method method) = draw method
    draw _ = p "Name"

instance Drawing Method where
    draw (Concrete methodSignature _) = pInBox $ getSignature methodSignature
    draw (Abstract methodSignature) = pInBox $ getSignature methodSignature
    draw (Interface methodSignature) = pInBox $ getSignature methodSignature
    draw (External methodSignature) = pInBox $ getSignature methodSignature
    draw (ArrowFunction methodSignature _) = pInBox $ getSignature methodSignature
    draw (OperatorOverload _ _ _ parameters _ _ _) = p "Name"

getSignature (MethodSignature vis static modifier returnType name parameters _ _) = 
    showVis vis ++ " " ++
    showReturnType returnType ++ " " ++
    showMethodName name ++ " " ++
    showParameters parameters

showVis Public = "+"
showVis Private = "-"
showVis Internal = "o"
showVis Protected = "<>"
showVis Unset = "_"

showReturnType = showDatatype

showDatatype (Single t) = t
showDatatype (Generic t ts) = showDatatype t ++ "<" ++ (intercalate ", " $ map showDatatype ts) ++ ">"
showDatatype (Class.List t) = "List<" ++ showDatatype t ++ ">"

showMethodName (Class.MethodName name) = name 
showMethodName (GenericMethodName name t) = (name ++ "<" ++ t ++ ">")

showParameters ps = 
    "(" ++ (intercalate ",\n" (map showParameter ps)) ++ ")"

showParameter (Parameter ref params dataType name value extention) = showDatatype dataType ++ " " ++ name ++ showValue value
    where 
        showValue (Just v) = " = " ++ v
        showValue Nothing = ""

underline = strutY memberHeight === hrule memberWidth # lw 0.3 # alignL

h1 t = p t # scale 1.4 # bold === underline

p t = vsep 1 $ map (alignedText 0 0.5) $ lines t

textInBox e t = (e t ||| strutX memberWidth) === underline

pInBox = textInBox p

memberSpace = 1.2

memberHeight = 0.6

memberWidth = 30