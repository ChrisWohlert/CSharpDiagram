{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}

module Drawing where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import CSParser
import PathNavigator
import qualified TwoDVector as TwoD
import Class
import Data.List
import Packer
import Data.List.Split
import Diagrams.Names
import Data.Colour.Palette.BrewerSet
import PathFinder
import Diagrams.TwoD.Arrow
import Data.Maybe
import Diagrams.Trail
import Safe


import qualified Debug.Trace as D

class Drawing a where
    draw :: a -> Diagram B

instance Packable (QDiagram B V2 Double Any) where
    packingDims d = (width d, height d)

instance IsName FullName

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
            header === (drawnTypes === drawnNamespaces) # boundByRect 0.6 # svgClass "namespace " # svgClass (filter (/= '.') name)

boundByRect w contents = contents <> (boundingRect contents) # lw w

instance Drawing Type where
    draw c@(Class _ ns _ _ _ _ name _ _ members _ _) = contents # boundByRect 0.3 # fc lightgrey # named (fullname c) # frame 7 # svgClass "class "
        where 
            contents = strutY 1 === vsep memberSpace ([draw name] ++ map draw members) # fc black
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

drawDependencies :: [Type] -> (QDiagram B V2 Double Any) -> (QDiagram B V2 Double Any)
drawDependencies types diag = compose (concatMap (drawDependenciesForType diag types) (take 1 $ types)) diag
    where
        drawDependenciesForType :: QDiagram B V2 Double Any -> [Type] -> Type -> [(QDiagram B V2 Double Any -> QDiagram B V2 Double Any)]
        drawDependenciesForType diag types c = map fromJust $ filter isJust $ map (\ (d,a) -> drawDependency (fullname c) d a (map fullname types) diag) $ zip (take 1 $ dependencies c) $ brewerSet Paired (length $ dependencies c)

drawDependency :: FullName -> FullName -> Colour Double -> [FullName] -> (QDiagram B V2 Double Any) -> Maybe (QDiagram B V2 Double Any -> QDiagram B V2 Double Any)
drawDependency r d a types diag = do
    rootClass <- lookupName r diag
    destinationClass <- lookupName d diag
    let rootToDestV = mkUnitV $ location destinationClass .-. location rootClass
    let destToRootV = turnAround rootToDestV
    let pointOnRootBoundary = boundaryFrom rootClass rootToDestV
    let pointOnDestBoundary = boundaryFrom destinationClass destToRootV
    let allClassDiagrams = map fromJust $ filter isJust $ map (flip lookupName diag) $ types \\ [r, d]
    let collidables = map mkCollidable allClassDiagrams
    let collidablesInProblemSpace = filterCollidableToProblemSpace (TwoD.Point (unp2 pointOnRootBoundary)) (TwoD.Point (unp2 pointOnDestBoundary)) collidables
    let fullpath = getPath (TwoD.Point (unp2 pointOnRootBoundary)) (TwoD.Point (unp2 pointOnDestBoundary)) collidablesInProblemSpace
    let trail = fromVertices $ map toDiagramsPoint ((D.trace (show fullpath)) fullpath)
    let arrow = (lw 0.3 . mconcat . zipWith lc colors . map strokeLocTrail . explodeTrail) trail
    let FullName cns _ = r
    let FullName dns _ = d
    return (atop (arrow # lw 0.3 
                        # (svgClass "dependency ") 
                        # (svgClass ("dependency-" ++ (filter (/= '.') cns) ++ " "))))

colors = cycle [aqua, orange, deeppink, blueviolet, crimson, darkgreen]

mkUnitV = fromDirection . direction

mkCollidable c = Collidable (unp2 (location c)) (width c) (height c)

toDiagramsPoint (TwoD.Point (x, y)) = p2 (x, y)

turnAround = rotate halfTurn

drawDArrow :: (Point V2 Double) -> (Point V2 Double) -> [QDiagram B V2 Double Any] -> [Point V2 Double]
drawDArrow start end allClassDiagrams = start : (drawFromTo start end) ++ [end]
    where
        drawFromTo start end = 
            let 
                dir = stepDir start end
                nextStep =  start .+^ dir
                (x :& _) = coords nextStep
                (xEnd :& _) = coords end
                continue = floor x /= floor xEnd
            in 
                if (continue) then
                    case (headMay $ filter (flip inquire nextStep) allClassDiagrams) of
                        Just d -> 
                            let 
                                adjustmentDir = around d end start
                                adjusted = adjustP start d end adjustmentDir
                            in
                                drawFromTo adjusted end ++ [adjusted, start]
                        Nothing -> drawFromTo nextStep end
                else
                    []

stepD = 3

stepDir s e = scale stepD $ mkUnitV (e .-. s)

keep xs@(x:_) f = x : (keep' xs f)
keep [] f = []

keep' [] _ = []
keep' (x:xx:xs) f = (keep (f x xx) f) ++ (keep (xx:xs) f)
keep' (x:[]) _ = [x]

top = flip (.+^) (0 ^& stepD)
left = flip (.+^) ((-stepD) ^& 0)
bottom = flip (.+^) (0 ^& (-stepD)) 
right = flip (.+^) (stepD ^& 0) 
around d destination b = head $ sortOn (\ a -> pointsDistance destination (a b)) $ filter (\ a -> not $ inquire d (a b)) [top, left, bottom, right]

adjustP p d end adjustmentDir = 
    let
        dir = stepDir p end
        nextStep = p .+^ dir
    in
        if inquire d nextStep then adjustP (adjustmentDir p) d end adjustmentDir else p

compose :: [a -> a] -> a -> a
compose fs v = foldl (flip (.)) id fs $ v

pointsDistance p1 p2 = 
    let
        (x1 :& y1) = coords p1
        (x2 :& y2) = coords p2
    in 
        sqrt $ (x2 - x1) ** 2 + (y2 - y1) ** 2