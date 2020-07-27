> {-# LANGUAGE AllowAmbiguousTypes #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> {-# OPTIONS_GHC -Wno-missing-signatures #-}
>
> module TutorialCool where
>
> import           Opaleye ((.===), (.++))
> import qualified Opaleye as O
> import qualified Opaleye.Join as J
> import qualified Database.PostgreSQL.Simple as PGS
> import Database.Postgres.Temp
> import Data.Profunctor.Product.Default
> import Data.Profunctor.Product.TH
>
>
> example1 = pure (O.toFieldsI "Hello")
>
> example2 = pure (O.toFieldsI ("Hello", 1 :: Int))
>
> rows = [ (1 :: Int, "Hello")
>        , (2, "Goodbye")
>        , (3, "Bonjour")
>        ]
>
> rows2 = [ (1 :: Int, "world")
>         , (2, "cruel world") ]
>
> example3 = O.valuesSafe (map O.toFieldsI rows)
>
> example4 = do
>   (i1, s1) <- example3
>   (i2, s2) <- O.valuesSafe (map O.toFields rows2)
>   O.viaLateral O.restrict (i1 .=== i2)
>   pure (s1 .++ O.sqlString " " .++ s2)
>
> example5__1 = J.optional (O.valuesSafe (map O.toFieldsI [ "Hello", "world" ]))
>
> example5__2 :: O.Select (O.MaybeFields ())
> example5__2 = J.optional (O.valuesSafe (map O.toFieldsI []))
>
> example5 = do
>   (i1, s1) <- example3
>   s2 <- J.optional $ do
>     (i2, s2) <- O.valuesSafe (map O.toFieldsI rows2)
>     O.viaLateral O.restrict (i1 .=== i2)
>     pure s2
>   pure (s1, s2)
>
> example5_1 = pure (O.justFields (O.sqlString "Hello"))
>
> example5_2 :: O.Select (O.MaybeFields ())
> example5_2 = pure O.nothingFields
>
> example6 = do
>   (i1, s1) <- example3
>   s2 <- J.optional $ do
>     (i2, s2) <- O.valuesSafe (map O.toFieldsI rows2)
>     O.viaLateral O.restrict (i1 .=== i2)
>     pure s2
>   pure (s1
>         .++ O.sqlString " "
>         .++ O.fromMaybeFields (O.sqlString "et c'est tout" ) s2)
>
> example7 = O.viaLateral O.restrict (O.sqlBool True)
>
> example8 = O.viaLateral O.restrict (O.sqlBool False)
>
> data Rec a b = Rec a b deriving Show
>
> $(makeAdaptorAndInstanceInferrable "pRec" ''Rec)
>
> example9 = pure (O.toFieldsI (Rec "Hello" "world"))
>
> example10 = O.union example4 example6
>
> example11 = do
>   k <- O.distinct $ do
>       (k, _) <- as_bs
>       pure k
>
>   let foo2 :: O.Select (O.Field O.SqlInt4)
>       foo2 = fmap (O.unsafeCast "int4") $ O.aggregate O.sum $ do
>         (k', i) <- as_bs
>         O.viaLateral O.restrict (k .=== k')
>         pure i
>
>   r <- foo2
>   pure (k, r)
>
> as_bs = O.valuesSafe (map O.toFieldsI [ ("a", 1 :: Double)
>                                       , ("a", 2)
>                                       , ("b", 10)
>                                       , ("b", 20)
>                                       , ("c", 100)
>                                       , ("c", 200)
>                                       ])
>
> groupBy_ :: (Default O.EqPP k k,
>              Default O.Distinctspec k k)
>          => (a -> k) -> O.Select a -> O.Select (k, O.Select a)
> groupBy_ f s = do
>   k <- O.distinct $ do
>       t <- s
>       pure (f t)
>
>   let group = do
>         t <- s
>         O.viaLateral O.restrict (k .=== f t)
>         pure t
>
>   pure (k, group)
>
> example11_1 = do
>   (k, s) <- groupBy_ fst as_bs
>   sum' <- O.laterally (O.aggregate O.sum) (fmap snd s)
>   avg' <- O.laterally (O.aggregate O.avg) (fmap snd s)
>   str' <- O.laterally (O.aggregate (O.stringAgg (O.sqlString ","))) (fmap fst s)
>   pure (k, sum', avg', str')
>
> --run
> -- :: (Default (O.Wrap O.FromFields) fields a, Show a)
> -- => O.Select fields -> IO (Either StartError [a])
> run s =
>   do { putStr "Starting DB..."
>      ; r <- with (\db -> do { putStrLn "done."
>                             ; let { connectString = toConnectionString db }
>                             ; conn <- PGS.connectPostgreSQL connectString
>                             ; ts <- O.runSelectI conn s; pure ts })
>      ; case r of
>        { Left  e  -> fail ("Database error: " ++ show e)
>        ; Right ts -> pure ts
>        }
>      }
