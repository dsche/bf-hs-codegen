import Data.List

data Command = Nop | Loop [(Int, Command)]

isLoop :: Command -> Bool
isLoop (Loop _) = True
isLoop _ = False

type Program = ([(Int, Structure)], [(Int, Command)])

data Structure = BoundedTape | Register | InfBoundedTapes | Tape
     deriving (Ord, Enum, Eq)

reduce :: Structure -> [Structure]
reduce Register = [BoundedTape]
reduce InfBoundedTapes = [BoundedTape, BoundedTape, BoundedTape, BoundedTape, Register]
reduce Tape = [InfBoundedTapes]

freshTapes :: Program -> Int -> [Structure] -> (Program, [Int])
freshTapes p _ [] = (p, [])
freshTapes (structs, cmds) oldIdx (s:ss) = (p, oldIdx:is)
  where (p, is) = freshTapes (structs', cmds) idx ss
        structIndexes = sort $ (map fst structs)
        structs' = (oldIdx, s) : (case oldIdx `elem` structIndexes of
                                     True -> filter ( (/= oldIdx) . fst ) structs
                                     False -> structs )
        idx = head $ [1..] \\ structIndexes

compileSingle :: Structure -> Command -> [Int] -> [(Int, Command)]
compileSingle = undefined -- TODO: Specify

mapSingle :: Structure -> Int -> Program -> Program
mapSingle struct idx p@(structs, cmds) = (structs', cmds')
  where newStructs = reduce struct
        ((structs', _), idxs) = freshTapes p idx newStructs
        cmds' = concat $ map (\(i, c) -> 
                               case i == idx of
                                 True -> compileSingle struct c idxs
                                 False -> [(i, c)] ) cmds

reorderStep :: Program -> Program
reorderStep (structs, cmds) = (structs, untangledInit ++ ( case restCommands of
                                                                       [] -> []
                                                                       ((i, Loop l):rs) -> (i, Loop (snd $ reorderStep (structs, l))) : (snd $ reorderStep (structs, rs))
                                                                       _ -> [] ) )
  where initCommands = takeWhile (not . isLoop . snd) cmds
        restCommands = dropWhile (not . isLoop . snd) cmds
        struct idx = snd $ head $ filter ( (==idx) . fst) structs
        reorderedInit = sortBy (\ (ix, cmd) (ix', cmd') ->
                                 case (compare (struct ix) (struct ix'), compare ix ix') of
                                   (GT, _) -> LT
                                   (LT, _) -> GT
                                   (EQ, x) -> x) initCommands
        zippedInit = map ( \(i,c) -> (i, (struct i), c) ) reorderedInit
        partitionedInit = partitioned' [] zippedInit
        partitioned' acc [] = [acc]
        partitioned' [] (x:xs) = partitioned' [x] xs
        partitioned' (y@(_, s, _):ys) (x@(_, s', _):xs) = case s == s' of
          True -> partitioned' (y:ys ++ [x]) xs
          False -> (y:ys) : partitioned' [x] xs
        optimizedInit = concat $ map optimize partitionedInit
        untangledInit = map ( \(i, _, c) -> (i, c) ) optimizedInit
        
compileStep :: Program -> Program
compileStep p@(structs, cmds) = case maxLevelStructs of
  (x:xs) -> mapSingle (snd x) (fst x) p
  [] -> p
  where maxLevelStructs = filter ( (==maxLevel) . snd ) structs
        maxLevelCommands = filter ( (flip elem) (map fst maxLevelStructs) . fst ) $ cmds
        maxLevel = maximum $ map snd structs
        struct idx = snd $ head $ filter ( (==idx) . fst) structs
--        computedInit = map (\(idx, cmd) -> (idx, struct idx, cmd) ) initCommands


optimize :: [(Int, Structure, Command)] -> [(Int, Structure, Command)]
optimize = id