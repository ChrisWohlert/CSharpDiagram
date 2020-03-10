{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}

module Drawing where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Lib
import Class
import Data.List
import Packer
import Data.List.Split
import Diagrams.Names

class Drawing a where
    draw :: a -> Diagram B

instance Packable (QDiagram B V2 Double Any) where
    packingDims d = (width d, height d)

instance IsName ClassName

layoutDiagramsAsGrid = maybe (strutX 0) (travelPacked (frame 1) (|||) (===)) . pack . map draw

dashed  = dashingN [0.03,0.03] 0

instance Drawing Solution where
    draw (Solution nss) = drawDependencies (concatMap getAllTypesFromNamespace nss) (layoutDiagramsAsGrid nss)

instance Drawing Namespace where
    draw (Namespace name nss types) = 
        let 
            headerSize = if not ('.' `elem` name) then 10 else 6
            header = splitOn "." name # intercalate ".\n"  # p # scale headerSize # frame 5 
            drawnNamespaces = layoutDiagramsAsGrid nss
            drawnTypes = layoutDiagramsAsGrid types
        in
            header === (drawnTypes === drawnNamespaces) # boundByRect 0.6 # svgClass "namespace "

boundByRect w contents = contents <> (boundingRect contents) # lw w

instance Drawing Type where
    draw c@(Class _ _ _ _ _ _ name _ _ members _ _) = contents # boundByRect 0.3 # svgClass "class " # named name
        where 
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

p t = vsep 1 $ map (frame 0.5) $ map (alignedText 0 0.5) $ lines t

textInBox e t = (e t ||| strutX memberWidth) === underline

pInBox = textInBox p

memberSpace = 1.2

memberHeight = 0.6

memberWidth = 30

drawDependencies :: [Type] -> (QDiagram B V2 Double Any -> QDiagram B V2 Double Any)
drawDependencies types = \ d -> (compose (concat (map drawDependenciesForType types)) d)
    where
        drawDependenciesForType :: Type -> [(QDiagram B V2 Double Any -> QDiagram B V2 Double Any)]
        drawDependenciesForType t = map (drawDependency $ class_name t) $ class_dependencies t

drawDependency :: ClassName -> ClassName -> (QDiagram B V2 Double Any -> QDiagram B V2 Double Any)
drawDependency c d = 
    withName c $ \rb ->
    withName d $ \cb ->
      atop (boundaryFrom rb unit_Y ~~ boundaryFrom cb unitY)

compose :: [a -> a] -> a -> a
compose fs v = foldl (flip (.)) id fs $ v