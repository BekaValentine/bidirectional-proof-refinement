module TypeChecker where

data Type = Nat | Prod Type Type | Arr Type Type
  deriving (Show,Eq)

data Program = Var String | Ann Program Type
             | Pair Program Program | Fst Program | Snd Program
             | Lam String Program | App Program Program
  deriving (Show)

data Judgment = Equal Type Type
              | Check [(String,Type)] Program Type
              | Synth [(String,Type)] Program
  deriving (Show)

decomposeEqual :: Type -> Type -> Maybe ([Judgment], [Type] -> Maybe Type)
decomposeEqual Nat Nat =
  Just ([], \as -> Just undefined)
decomposeEqual (Prod a1 b1) (Prod a2 b2) =
  Just ([Equal a1 a2, Equal b1 b2], \as -> Just undefined)
decomposeEqual (Arr a1 b1) (Arr a2 b2) =
  Just ([Equal a1 a2, Equal b1 b2], \as -> Just undefined)
decomposeEqual _ _ = Nothing

decomposeCheck
  :: [(String,Type)]
  -> Program
  -> Type
  -> Maybe ([Judgment], [Type] -> Maybe Type)
decomposeCheck g (Pair m n) (Prod a b) =
  Just ([Check g m a, Check g n b], \as -> Just undefined)
decomposeCheck g (Lam x m) (Arr a b) =
  Just ([Check ((x,a):g) m b], \as -> Just undefined)
decomposeCheck g m a =
  Just ([Synth g m], \[a2] -> if a == a2 then Just undefined else Nothing)

decomposeSynth
  :: [(String,Type)] -> Program -> Maybe ([Judgment], [Type] -> Maybe Type)
decomposeSynth g (Var x) =
  case lookup x g of
    Nothing -> Nothing
    Just a -> Just ([], \as -> Just a)
decomposeSynth g (Ann m a) =
  Just ([Check g m a], \as -> Just a)
decomposeSynth g (Fst p) =
  Just ( [Synth g p]
       , \[t] -> case t of
                   Prod a b -> Just a
                   _ -> Nothing
       )
decomposeSynth g (Snd p) =
  Just ( [Synth g p]
       , \[t] -> case t of
                   Prod a b -> Just b
                   _ -> Nothing
       )
decomposeSynth g (App f x) =
  Just ( [Synth g f, Synth g x]
       , \[s,t] -> case s of
                     Arr a b | a == t -> Just b
                     _ -> Nothing)
decomposeSynth _ _ = Nothing

decompose :: Judgment -> Maybe ([Judgment], [Type] -> Maybe Type)
decompose (Equal a b) = decomposeEqual a b
decompose (Check g m a) = decomposeCheck g m a
decompose (Synth g m) = decomposeSynth g m

data ProofTree = ProofTree Judgment [ProofTree]
  deriving (Show)

findProof :: Judgment -> Maybe (ProofTree, Type)
findProof j =
  case decompose j of
    Nothing -> Nothing
    Just (js,f) -> case sequence (map findProof js) of
      Nothing -> Nothing
      Just tsas ->
        let (ts,as) = unzip tsas
        in case f as of
             Nothing -> Nothing
             Just a -> Just (ProofTree j ts, a)

main :: IO ()
main =
  do print (findProof (Synth [("p",Prod Nat Nat)] (Fst (Var "p"))))
