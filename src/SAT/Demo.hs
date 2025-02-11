satisfy :: BooleanFormula -> Env -> CList (Bool, Env)
satisfy formula env = case formula of
  Var var -> case env !? var of
    Just stored -> pure (stored, env)
    Nothing ->
      cons'
        (True, Map.insert var True env)
        (cons' (False, Map.insert var False env) nil')
  Not inner ->
    first not <$> satisfy inner env
  And left right -> do
    (result, env') <- satisfy left env
    if result
      then satisfy right env'
      else pure (False, env')
  Or left right -> do
    (result, env') <- satisfy left env
    if result
      then pure (True, env')
      else satisfy right env'
