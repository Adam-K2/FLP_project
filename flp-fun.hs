-- FLP 1. Projekt - Rozhodovací stromy
-- Name: Adam Kučík
-- Login: xkucik00
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.List (minimumBy, group, nub, sort ,sortBy)

-- Used data types
data Tree = EmptyTree | Leaf String | Node Int Double Tree Tree 

type DatasTuple = ([Double], String)
type Split = (Int, Double, [DatasTuple], [DatasTuple])
type SplitWithWeight = (Int, Double, Double, [DatasTuple], [DatasTuple])

-- First task --------------------------------------------------------------------------
-- This function reads both input files and processes them
loadTree :: String -> String -> IO ()
loadTree treeFile dataFile = do
    content <- readFile treeFile
    let linesTree = lines content
    let tree = createTree linesTree
    contents <- classData dataFile
    let classes = map (classify tree) contents
    mapM_ putStrLn classes 

-- This function process the new data for classification
classData :: String -> IO [[Double]]
classData dataFile = do
    content <- readFile dataFile
    return $ map (map read . splitOn ",") (lines content)

-- Here the datas are classified
classify :: Tree -> [Double] -> String
classify (Leaf label) _ = label
classify (Node indx val l r) vec
    | vec !! indx <= val = classify l vec
    | otherwise = classify r vec
classify EmptyTree _ = error "Encountered EmptyTree during classification of Tree"

-- This function starts with the creation of the tree
-- It creates only the root and rest of the tree is created in createSubTree
createTree :: [String] -> Tree
createTree [] = EmptyTree
createTree (line : rest) = 
    let (root, _) = createSubTree 0 (trim line : rest)
    in root

-- This function parse a subtree starting at a given indentation level
createSubTree :: Int -> [String] -> (Tree, [String])
createSubTree _ [] = (EmptyTree, [])
createSubTree level (line:rest) =
    let trimmed = trim line
        indent  = length line - length trimmed
    in if indent < level
        then (EmptyTree, line:rest)  -- Backtrack to parent level
        else case parseLine trimmed of
            Left leaf -> (leaf, rest)
            Right node ->
                let (lChild, afterLeft) = createSubTree (indent + 2) rest
                    (rChild, afterRight) = createSubTree (indent + 2) afterLeft
                in (Node (getInt node) (getDouble node) lChild rChild, afterRight)

-- This function parse a line into Leaf or Node
parseLine :: String -> Either Tree Tree
parseLine line =
    let parts = splitOn ":" line
    in case head parts of
        "Node" -> Right (parseNode parts)
        "Leaf" -> Left (parseLeaf parts)
        _      -> error $ "Invalid line: " ++ line

-- Function to parse a Node line
parseNode :: [String] -> Tree
parseNode parts =
    let dataPart = parts !! 1     -- Second part is the data
        nodeParts = splitOn "," dataPart
        intVal = read (head nodeParts) :: Int  -- First part is the Int
        doubleVal = read (nodeParts !! 1) :: Double  -- Second part is the Double
    in Node intVal doubleVal EmptyTree EmptyTree

-- Function to parse a Leaf line
parseLeaf :: [String] -> Tree
parseLeaf parts =
    let dataPart = parts !! 1  -- Second part is the data
    in Leaf (trim dataPart)

-- Function to trim spaces at the beggining of line
trim :: String -> String
trim = dropWhile (== ' ')

-- Helper functions to extract Int and Double from a Node
getInt :: Tree -> Int
getInt (Node intVal _ _ _) = intVal
getInt _ = error "GetInt function failed"

getDouble :: Tree -> Double
getDouble (Node _ doubleVal _ _) = doubleVal
getDouble _ = error "GetDouble function failed"

-- Second task ----------------------------------------------------------------------
-- Function to print the final tree to output
printTree :: Tree -> String
printTree = helperPrintTree 0
  where
    helperPrintTree :: Int -> Tree -> String
    helperPrintTree level (Leaf label) = replicate level ' ' ++ "Leaf: " ++ label
    helperPrintTree level (Node idx thresh l r) =
        replicate level ' ' ++ "Node: " ++ show idx ++ ", " ++ show thresh ++ "\n" ++
        helperPrintTree (level + 2) l ++ "\n" ++
        helperPrintTree (level + 2) r
    helperPrintTree level EmptyTree = replicate level ' ' ++ "EmptyTree"

-- This function process the input file for second task, then build the tree which is then printed
trainTree :: String -> IO ()
trainTree treeFile = do
    content <- readFile treeFile
    let linesData = lines content
    let unfilterDatas = map parseData linesData
    let datas = filter (not . null . fst) unfilterDatas
    let tree = buildTree datas
    putStrLn (printTree tree)

-- This function parse the input datas devided by ","
parseData :: String -> DatasTuple
parseData line = 
    let parts = splitOn "," line
        features = map read (init parts) :: [Double]
        classes = trim (last parts)
    in (features, classes)

-- This function check if all elements have the same class
allSameCls :: [DatasTuple] -> Bool
allSameCls [] = True
allSameCls xs =
    let firstCls = snd $ head xs
    in all (\x -> snd x == firstCls) xs

-- Building of the tree for the second task
buildTree :: [DatasTuple] -> Tree
buildTree datas
    | allSameCls datas = Leaf (snd (head datas))
    | otherwise =
        let (bestFeatureIdx, bestThr, l, r) = findBestThr datas
        in Node bestFeatureIdx bestThr (buildTree l) (buildTree r)

-- Function to find best threshold
findBestThr :: [DatasTuple] -> Split
findBestThr datas =
    let numFeatures = length $ fst (head datas)
        splits = getSplits datas numFeatures
    in case splits of
        [] -> error "No valid splits found."
        _ -> selectBestSplit splits

-- Function select best split from all splits
selectBestSplit :: [SplitWithWeight] -> Split
selectBestSplit splits =
    let (bestFeatureIdx, bestThr, _, bestLeft, bestRight) = 
            minimumBy (comparing getWeiGini) splits
    in (bestFeatureIdx, bestThr, bestLeft, bestRight)

-- Get all splits with their weights
getSplits :: [DatasTuple] -> Int -> [SplitWithWeight]
getSplits datas numFeatures =
    concatMap processFeature [0 .. numFeatures - 1]
  where
    processFeature featureIdx =
        let splits = evalFeature featureIdx datas
        in map (splitRes featureIdx) splits

    splitRes featureIdx (threshold, l, r) =
        (featureIdx, threshold, weiGini l r, l, r)

-- Evaluates feauture to find potential splits based on threshold
evalFeature :: Int -> [DatasTuple] -> [(Double, [DatasTuple], [DatasTuple])]
evalFeature featureIdx datas =
    let sortData = sortByFeature featureIdx datas
        featureVals = map ((!! featureIdx) . fst) sortData
        uniqueVals = nub featureVals
        thresholds = getThr uniqueVals
        allSplits = map (computeSplit sortData) thresholds
    in filter isValidSplit allSplits
  where
    sortByFeature :: Int -> [DatasTuple] -> [DatasTuple]
    sortByFeature idx = sortBy (\(feature1, _) (feature2, _) -> compare (feature1 !! idx) (feature2 !! idx))
    
    computeSplit sortedData threshold =
        let (l, r) = splitAtThr featureIdx threshold sortedData
        in (threshold, l, r)
    isValidSplit (_, l, r) = not (null l) && not (null r)

-- This counts the threshold value
getThr :: [Double] -> [Double]
getThr vals =
    let sortedVals = sort vals
    in zipWith (\x y -> (x + y) / 2) sortedVals (tail sortedVals)

-- Function splits the datas according to the treshold
splitAtThr :: Int -> Double -> [DatasTuple] -> ([DatasTuple], [DatasTuple])
splitAtThr featureIdx threshold datas =
    let l = takeWhile (\(feature, _) -> feature !! featureIdx <= threshold) datas
        r = drop (length l) datas
    in (l, r)

-- Helper function to get weightedGini
getWeiGini :: SplitWithWeight -> Double
getWeiGini (_, _, g, _, _) = g

-- Counting of weightedGini
weiGini :: [DatasTuple] -> [DatasTuple] -> Double
weiGini lPart rPart =
    let total = fromIntegral $ length lPart + length rPart
        gLeft = gini $ map snd lPart
        gRight = gini $ map snd rPart
        lWeight = fromIntegral (length lPart) / total
        rWeight = fromIntegral (length rPart) / total
    in lWeight * gLeft + rWeight * gRight

-- Gini index
gini :: [String] -> Double
gini [] = 0.0
gini vals = 
    let allVals = fromIntegral $ length vals
        sortVals = group $ sort vals
        count = map (fromIntegral . length) sortVals
        props = map (/ allVals) count
        sqrVals = sum $ map (** 2.0) props
        impurity = 1 - sqrVals
    in impurity

-- Main --------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> error "Arguments should be -1 or -2. For -1 <tree file> <new_data file>. For -2 <training_data file>."
        ["-1", treeFile, dataFile] -> do 
            loadTree treeFile dataFile
        ["-2", treeFile] -> do 
            trainTree treeFile
        _ -> error "Invalid argument (use -1 or -2) or too many files. For -1 <tree file> <new_data file>. For -2 <training_data file>."