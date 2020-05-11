import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Monad (replicateM)

-- 個体
data Entity = Entity String deriving (Eq, Ord)

instance Show Entity where
  show (Entity name) = name

-- 領域
type Domain = Set Entity

-- 変項
data Variable = Variable String deriving (Eq, Ord)

-- 定項
data Constant = Constant String deriving (Eq, Ord)

-- タイプ
data Type
  = TypeE
  | TypeT
  | TypePair Type Type
  | TypePairS Type

data Typed
  = E Entity
  | T Bool
  | Pair (Map Typed Typed)
  | PairS (Map World Typed)
  deriving (Eq, Ord)

instance Show Typed where
  show (E e) = show e
  show (T t) = if t then "1" else "0"
  show (Pair m) = "[" ++ List.intercalate ", " [ show a ++ " -> " ++ show b | (a, b) <- Map.toList m] ++ "]"
  show (PairS m) = "[" ++ List.intercalate ", " [ show s ++ " -> " ++ show a | (s, a) <- Map.toList m] ++ "]"

-- 解釈領域
discourse :: Type -> Domain -> Set World -> Set Typed
discourse t d ws = case t of
  TypeE -> Set.map E d
  TypeT -> Set.fromList [T False, T True]
  TypePair a b ->
    let
      da = Set.toList (discourse a d ws)
      db = Set.toList (discourse b d ws)
    in
      Set.fromList [Pair (Map.fromList [(da !! i, l !! i) | i <- [0 .. length l - 1 ]]) | l <- replicateM (length da) db]
  TypePairS a ->
    let
      da = Set.toList (discourse a d ws)
      wl = Set.toList ws
    in
      Set.fromList [ PairS (Map.fromList [ (wl !! i, l !! i) | i <- [0 .. length l - 1 ]]) | l <- replicateM (length wl) da]

-- 解釈関数
type Interpretation = Map Constant (Map World Typed)

-- モデル
type Model = (Set World, Domain, Interpretation)

-- 世界
data World = World Int deriving (Eq, Ord)

instance Show World where
  show (World n) = "w" ++ show n

-- 割当関数
type Assignment = Map Variable Typed

-- 表現
data ME
  = VAR Variable
  | CON Constant
  | Lambda Type Variable ME
  | Apply ME ME
  | Up ME
  | Down ME

-- 解釈
interpret :: Model -> World -> Assignment -> ME -> Typed
interpret m w g e =
  let (ws, d, i) = m
  in
  case e of
    CON α ->
      i ! α ! w
    VAR α ->
      g ! α
    Lambda t u α ->
      Pair (Map.fromList [(d, interpret m w (Map.insert u d g) α) | d <- Set.toList (discourse t d ws)])
    Apply α β ->
      let
        Pair α' = interpret m w g α
      in
        α' ! (interpret m w g β)
    Up α ->
      PairS (Map.fromList [(w', interpret m w' g α) | w' <- Set.toList ws])
    Down α ->
      let
        PairS α' = interpret m w g α
      in
        α' ! w

main :: IO ()
main = do
  {-
  let frank = Entity "Frank"
  let george = Entity "George"
  let helen = Entity "Helen"
  -}
  let ivan = Entity "Ivan"
  let john = Entity "John"
  {-
  let f = E frank
  let g = E george
  let h = E helen
  -}
  let i = E ivan
  let j = E john
  let w0 = World 0
  let w1 = World 1
  {-
  let w2 = World 2
  let w3 = World 3
  -}
  let worlds = Set.fromList [w0, w1 {-, w2, w3-}]
  let domain = Set.fromList [{-frank, george, helen,-} ivan, john]
  let set list = Pair (Map.fromList [(E e, T (E e `elem` list)) | e <- Set.toList domain])
  let interpretation =
        Map.fromList
          [ ( Constant "j"
            , Map.fromList
                [ (w0, j)
                , (w1, j)
                {-
                , (w2, j)
                , (w3, j)
                -}
              ])
          , ( Constant "RUN"
            , Map.fromList
                [ (w0, set [{-f, g,-} j])
                , (w1, set [{-f, h,-} i, j])
                {-
                , (w2, set [g, i])
                , (w3, set [f, i, j])
                -}
              ])
          {-
          , ( Constant "FAT"
            , Map.fromList
                [ (w0, set [g, i, j])
                , (w1, set [f, g, h, i])
                , (w2, set [f, g, h])
                , (w3, set [g, h, i])
              ])
          -}
          ]
  let model = (worlds, domain, interpretation)
  let g = Map.empty

  -- 問題 7.6 i
  let xT = TypePairS (TypePair TypeE TypeT)
  let john = Lambda xT (Variable "X") (Apply (Down (VAR (Variable "X"))) (CON (Constant "j")))
  putStr "John = "
  print $ interpret model w0 g john

  -- 問題 7.6 ii
  let run = CON (Constant "RUN")
  putStr "run = "
  print $ interpret model w0 g run

  putStr "John runs = "
  print $ interpret model w0 g (Apply john (Up run))

  return ()
