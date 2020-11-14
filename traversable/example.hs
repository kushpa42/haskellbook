data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- before
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
    a <- fetchFn query
    case sequence (map decodeFn a) of
      (Left err)  -> return $ Left $ err
      (Right res) -> do
          a <- makeIoOnlyObj res
          return $ Right a

-- Suspicious:
-- 1. The use of sequence and map
-- 2. Manually casing on the result of the sequence and the map
-- 3. Binding monadically over the Either only to perform another
--    monadic IO action inside of that

pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
    a <- fetchFn query
    traverse makeIoOnlyObj (mapM decodeFn a)

pipelineFn'' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'' = (traverse makeIoOnlyObj . mapM decodeFn =<<) . fetchFn

pipelineFn''' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn''' = (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn
