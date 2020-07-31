{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Opaleye.Test.Rectangular where

import Opaleye.Test.Fields

import Control.Arrow
import Control.Monad
import Data.Functor.Identity

type ChoiceT = Choice () () ()

data Unknown a = Unknown | Known a deriving Show

deriving instance Show (Choices Unknown () () ())

inferHaskells :: Haskells -> Choices Unknown () () ()
inferHaskells hs = Choices $ flip map (unChoices hs) $ \case
  Left l -> Left $ case l of
    CInt    _ -> CInt ()
    CBool   _ -> CBool ()
    CString _ -> CString ()
  Right r -> Right $ case r of
    Nothing -> Unknown
    Just hs' -> Known (inferHaskells hs')

unify :: Choices Unknown () () ()
      -> Choices Unknown () () ()
      -> Either String (Choices Unknown () () ())
unify as bs = do
  zipped <- case zipSameLength (unChoices as) (unChoices bs) of
    Nothing -> Left "Different length"
    Just j -> pure j

  cs <- flip mapM zipped $ \case
    (Left l1, Left l2)   -> Left <$> case (l1, l2) of
      (CInt _,    CInt _)    -> pure (CInt ())
      (CBool _,   CBool _)   -> pure (CBool ())
      (CString _, CString _) -> pure (CString ())
      _                      -> Left "Left type mismatch"
    (Right r1, Right r2) -> Right <$> case (r1, r2) of
      (Unknown, Unknown)   -> pure Unknown
      (Known a, Known b)   -> Known <$> unify a b
      (Unknown, k@Known{}) -> pure k
      (k@Known{}, Unknown) -> pure k
    _                    -> Left "Left/Right type mismatch"

  pure (Choices cs)

zipSameLength :: [a] -> [b] -> Maybe [(a, b)]
zipSameLength as bs = case (as, bs) of
  ([],    [])    -> Just []
  (a:as', b:bs') -> do
    as'_bs' <- zipSameLength as' bs'
    pure ((a, b):as'_bs')
  _ -> Nothing


infer :: [Haskells] -> Either String (Unknown (Choices Unknown () () ()))
infer = flip foldM Unknown $ \a b -> do
  let t = inferHaskells b

  u <- case a of
    Unknown -> pure t
    Known k -> unify k t

  pure (Known u)
