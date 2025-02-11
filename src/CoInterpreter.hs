{- stack
  ghci
  --package containers
  --package megaparsec
  --package parser-combinators
  --package mtl
  --package lifted-base
  --package transformers-base
  --package pretty-simple
  --package pqueue
  --package clock
  --ghc-options "-Wall -Wno-name-shadowing"
-}

-- start snippet imports
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module CoInterpreter where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar.Lifted
import Control.Monad (foldM, unless, void, when)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Cont (ContT, MonadCont, callCC, runContT)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT)
import Control.Monad.State.Strict qualified as State
import Data.Foldable (for_, traverse_)
import Data.IORef.Lifted
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min qualified as PQ
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Void (Void)
import System.Clock (Clock (Monotonic), TimeSpec, fromNanoSecs, getTime)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Pretty.Simple
  ( CheckColorTty (..),
    OutputOptions (..),
    defaultOutputOptionsNoColor,
    pPrintOpt,
  )

-- end snippet imports

-- start snippet ast-expression
data Expr
  = LNull
  | LBool Bool
  | LStr String
  | LNum Integer
  | Variable Identifier
  | Binary BinOp Expr Expr
  | Call Expr [Expr]
  | Lambda [Identifier] [Stmt]
  | Receive Expr
  deriving (Show, Eq)

type Identifier = String

-- end snippet ast-expression

-- start snippet ast-binop
data BinOp
  = Plus
  | Minus
  | Slash
  | Star
  | Equals
  | NotEquals
  | LessThan
  | GreaterThan
  deriving (Show, Eq)

-- end snippet ast-binop

-- start snippet ast-statement
data Stmt
  = ExprStmt Expr
  | VarStmt Identifier Expr
  | AssignStmt Identifier Expr
  | IfStmt Expr [Stmt]
  | WhileStmt Expr [Stmt]
  | FunctionStmt Identifier [Identifier] [Stmt]
  | ReturnStmt (Maybe Expr)
  | YieldStmt
  | SpawnStmt Expr
  | SendStmt Expr Expr
  deriving (Show, Eq)

type Program = [Stmt]

-- end snippet ast-statement

-- start snippet basic-parsers
type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

reserved :: String -> Parser ()
reserved w = (lexeme . try) $ string w *> notFollowedBy alphaNumChar

parens, braces :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")

semi, identifier, stringLiteral :: Parser String
semi = symbol ";"
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)
stringLiteral = char '"' >> manyTill L.charLiteral (char '"') <* sc

integer :: Parser Integer
integer = lexeme (L.signed sc L.decimal)

-- end snippet basic-parsers

-- start snippet parser-utils
runParser :: Parser a -> String -> Either String a
runParser parser code = do
  case parse parser "" code of
    Left err -> Left $ errorBundlePretty err
    Right prog -> Right prog

pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint =
  pPrintOpt CheckColorTty $
    defaultOutputOptionsNoColor
      { outputOptionsIndentAmount = 2,
        outputOptionsCompact = True,
        outputOptionsCompactParens = True
      }

-- end snippet parser-utils

-- start snippet expr-op
operators :: [[Operator Parser Expr]]
operators =
  [ [Prefix $ Receive <$ symbol "<-"],
    [ binary Slash $ symbol "/",
      binary Star $ symbol "*"
    ],
    [ binary Plus $ symbol "+",
      binary Minus $ try (symbol "-" <* notFollowedBy (char '>'))
    ],
    [ binary LessThan $ symbol "<",
      binary GreaterThan $ symbol ">"
    ],
    [ binary Equals $ symbol "==",
      binary NotEquals $ symbol "!="
    ]
  ]
  where
    binary op symP = InfixL $ Binary op <$ symP

-- end snippet expr-op

-- start snippet expr-term
term :: Parser Expr
term = primary >>= call
  where
    call e =
      ( lookAhead (symbol "(")
          >> symbol "("
          >> Call e <$> sepBy expr (symbol ",") <* symbol ")"
          >>= call
      )
        <|> pure e

    primary =
      LNull
        <$ reserved "null"
          <|> LBool True
        <$ reserved "true"
          <|> LBool False
        <$ reserved "false"
          <|> LStr
        <$> stringLiteral
          <|> LNum
        <$> integer
          <|> Lambda
        <$> (reserved "function" *> parens (sepBy identifier $ symbol ","))
        <*> braces (many stmt)
          <|> Variable
        <$> identifier
          <|> parens expr

-- end snippet expr-term

-- start snippet expr
expr :: Parser Expr
expr = makeExprParser term operators

-- end snippet expr

-- start snippet stmt
stmt :: Parser Stmt
stmt =
  IfStmt
    <$> (reserved "if" *> parens expr)
    <*> braces (many stmt)
      <|> WhileStmt
    <$> (reserved "while" *> parens expr)
    <*> braces (many stmt)
      <|> VarStmt
    <$> (reserved "var" *> identifier)
    <*> (symbol "=" *> expr <* semi)
      <|> YieldStmt
    <$ (reserved "yield" <* semi)
      <|> SpawnStmt
    <$> (reserved "spawn" *> expr <* semi)
      <|> ReturnStmt
    <$> (reserved "return" *> optional expr <* semi)
      <|> FunctionStmt
    <$> try (reserved "function" *> identifier)
    <*> parens (sepBy identifier $ symbol ",")
    <*> braces (many stmt)
      <|> try (AssignStmt <$> identifier <*> (symbol "=" *> expr <* semi))
      <|> try (SendStmt <$> expr <*> (symbol "->" *> expr <* semi))
      <|> ExprStmt
    <$> expr
    <* semi

-- end snippet stmt

-- start snippet program
program :: Parser Program
program = sc *> many stmt <* eof

-- end snippet program

-- start snippet value
data Value
  = Null
  | Boolean Bool
  | Str String
  | Num Integer
  | Function Identifier [Identifier] [Stmt] Env
  | BuiltinFunction Identifier Int ([Expr] -> Interpreter Value)
  | Chan Channel

-- end snippet value

-- start snippet value-instances
instance Show Value where
  show :: Value -> String
  show = \case
    Null -> "null"
    Boolean b -> show b
    Str s -> s
    Num n -> show n
    Function name _ _ _ -> "function " <> name
    BuiltinFunction name _ _ -> "function " <> name
    Chan Channel {} -> "Channel"

instance Eq Value where
  (==) :: Value -> Value -> Bool
  Null == Null = True
  Boolean b1 == Boolean b2 = b1 == b2
  Str s1 == Str s2 = s1 == s2
  Num n1 == Num n2 = n1 == n2
  _ == _ = False

-- end snippet value-instances

-- start snippet coroutine
data Coroutine a = Coroutine
  { corEnv :: Env,
    corCont :: a -> Interpreter (),
    corReady :: MVar TimeSpec
  }

newCoroutine :: Env -> (a -> Interpreter ()) -> Interpreter (Coroutine a)
newCoroutine env cont = do
  ready <- newMVar =<< currentSystemTime
  return $ Coroutine env cont ready

-- end snippet coroutine

-- start snippet delayed-coroutine
newDelayedCoroutine ::
  Integer -> Env -> (a -> Interpreter ()) -> Interpreter (Coroutine a)
newDelayedCoroutine millis env cont = do
  ready <- newEmptyMVar
  void $ liftIO $ forkIO $ do
    threadDelay $ fromIntegral millis * 1000
    now <- currentSystemTime
    putMVar ready now
  return $ Coroutine env cont ready

-- end snippet delayed-coroutine

-- start snippet env
type Env = Map.Map Identifier (IORef Value)

-- end snippet env

-- start snippet queue
type Queue a = IORef (PQ.MinPQueue TimeSpec a, TimeSpec)

newQueue :: (MonadBase IO m) => m (Queue a)
newQueue = do
  now <- liftBase currentSystemTime
  newIORef (PQ.empty, now)

queueSize :: (MonadBase IO m) => Queue a -> m Int
queueSize = fmap (PQ.size . fst) . readIORef

-- end snippet queue

-- start snippet state
data InterpreterState = InterpreterState
  { isEnv :: Env,
    isCoroutines :: Queue (Coroutine ())
  }

initInterpreterState :: IO InterpreterState
initInterpreterState = InterpreterState <$> builtinEnv <*> newQueue

-- end snippet state

-- start snippet builtin-env
builtinEnv :: IO Env
builtinEnv =
  Map.fromList
    <$> traverse
      (traverse newIORef)
      [ ("print", BuiltinFunction "print" 1 executePrint),
        ( "newChannel",
          BuiltinFunction "newChannel" 0 $ fmap Chan . const (newChannel 0)
        ),
        ( "newBufferedChannel",
          BuiltinFunction "newBufferedChannel" 1 executeNewBufferedChannel
        ),
        ("sleep", BuiltinFunction "sleep" 1 executeSleep),
        ( "getCurrentMillis",
          BuiltinFunction "getCurrentMillis" 0 executeGetCurrentMillis
        )
      ]

-- end snippet builtin-env

-- start snippet channel
data Channel = Channel
  { channelCapacity :: Int,
    channelBuffer :: Queue Value,
    channelSendQueue :: Queue (Coroutine (), Value),
    channelReceiveQueue :: Queue (Coroutine Value)
  }

newChannel :: Int -> Interpreter Channel
newChannel size = Channel size <$> newQueue <*> newQueue <*> newQueue

-- end snippet channel

-- start snippet exception
data Exception
  = Return Value
  | RuntimeError String
  | CoroutineQueueEmpty

-- end snippet exception

-- start snippet interpreter
newtype Interpreter a = Interpreter
  { runInterpreter ::
      ExceptT
        Exception
        ( ContT
            (Either Exception ())
            (StateT InterpreterState IO)
        )
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase IO,
      MonadState InterpreterState,
      MonadError Exception,
      MonadCont
    )

-- end snippet interpreter

-- start snippet define-env
defineVar :: Identifier -> Value -> Interpreter ()
defineVar name value = do
  env <- State.gets isEnv
  if Map.member name env
    then throw $ "Variable already defined: " <> name
    else do
      valueRef <- newIORef value
      setEnv $ Map.insert name valueRef env

setEnv :: Env -> Interpreter ()
setEnv env = State.modify' $ \is -> is {isEnv = env}

-- end snippet define-env

-- start snippet lookup-assign-env
lookupVar :: Identifier -> Interpreter Value
lookupVar name =
  State.gets isEnv >>= findValueRef name >>= readIORef

assignVar :: Identifier -> Value -> Interpreter ()
assignVar name value =
  State.gets isEnv >>= findValueRef name >>= flip writeIORef value

-- end snippet lookup-assign-env

-- start snippet find-value-ref
findValueRef :: Identifier -> Env -> Interpreter (IORef Value)
findValueRef name env =
  case Map.lookup name env of
    Just ref -> return ref
    Nothing -> throw $ "Unknown variable: " <> name

throw :: String -> Interpreter a
throw = throwError . RuntimeError

-- end snippet find-value-ref

-- start snippet evaluate
evaluate :: Expr -> Interpreter Value
evaluate = \case
  LNull -> pure Null
  LBool bool -> pure $ Boolean bool
  LStr str -> pure $ Str str
  LNum num -> pure $ Num num
  Variable v -> lookupVar v
  Lambda params body -> Function "<lambda>" params body <$> State.gets isEnv
  binary@Binary {} -> evaluateBinaryOp binary
  call@Call {} -> evaluateFuncCall call
  Receive expr ->
    evaluate expr >>= \case
      Chan channel -> channelReceive channel
      val -> throw $ "Cannot receive from a non-channel: " <> show val

-- end snippet evaluate

-- start snippet evaluate-binary
evaluateBinaryOp :: Expr -> Interpreter Value
evaluateBinaryOp ~(Binary op leftE rightE) = do
  left <- evaluate leftE
  right <- evaluate rightE
  let errMsg msg = msg <> ": " <> show left <> " and " <> show right
  case (op, left, right) of
    (Plus, Num n1, Num n2) -> pure $ Num $ n1 + n2
    (Plus, Str s1, Str s2) -> pure $ Str $ s1 <> s2
    (Plus, Str s1, _) -> pure $ Str $ s1 <> show right
    (Plus, _, Str s2) -> pure $ Str $ show left <> s2
    (Plus, _, _) -> throw $ errMsg "Cannot add or append"
    (Minus, Num n1, Num n2) -> pure $ Num $ n1 - n2
    (Minus, _, _) -> throw $ errMsg "Cannot subtract non-numbers"
    (Slash, Num n1, Num n2) -> pure $ Num $ n1 `div` n2
    (Slash, _, _) -> throw $ errMsg "Cannot divide non-numbers"
    (Star, Num n1, Num n2) -> pure $ Num $ n1 * n2
    (Star, _, _) -> throw $ errMsg "Cannot multiply non-numbers"
    (LessThan, Num n1, Num n2) -> pure $ Boolean $ n1 < n2
    (LessThan, _, _) -> throw $ errMsg "Cannot compare non-numbers"
    (GreaterThan, Num n1, Num n2) -> pure $ Boolean $ n1 > n2
    (GreaterThan, _, _) -> throw $ errMsg "Cannot compare non-numbers"
    (Equals, _, _) -> pure $ Boolean $ left == right
    (NotEquals, _, _) -> pure $ Boolean $ left /= right

-- end snippet evaluate-binary

-- start snippet evaluate-call
evaluateFuncCall :: Expr -> Interpreter Value
evaluateFuncCall ~(Call callee argEs) =
  evaluate callee >>= \case
    BuiltinFunction name arity func -> do
      checkArgCount name argEs arity
      func argEs
    func@Function {} -> evaluateFuncCall' func argEs
    val -> throw $ "Cannot call a non-function: " <> show callee <> " is " <> show val

checkArgCount :: Identifier -> [Expr] -> Int -> Interpreter ()
checkArgCount funcName argEs arity =
  when (length argEs /= arity) $
    throw $
      funcName
        <> " call expected "
        <> show arity
        <> " argument(s) but received "
        <> show (length argEs)

executePrint :: [Expr] -> Interpreter Value
executePrint argEs =
  evaluate (head argEs) >>= liftIO . print >> return Null

-- end snippet evaluate-call

-- start snippet execute-new-buffered-channel
executeNewBufferedChannel :: [Expr] -> Interpreter Value
executeNewBufferedChannel argEs =
  evaluate (head argEs) >>= \case
    Num capacity | capacity >= 0 -> Chan <$> newChannel (fromIntegral capacity)
    _ -> throw "newBufferedChannel call expected a positive number argument"

-- end snippet execute-new-buffered-channel

-- start snippet execute-sleep
executeSleep :: [Expr] -> Interpreter Value
executeSleep argEs =
  evaluate (head argEs) >>= \case
    Num n | n >= 0 -> sleep n >> return Null
    Num n -> throw $ "Sleep time must be non-negative: " <> show n
    _ -> throw "sleep call expected a number argument"

executeGetCurrentMillis :: [Expr] -> Interpreter Value
executeGetCurrentMillis _ =
  Num . fromIntegral . floor . (* 1000) <$> liftIO getPOSIXTime

-- end snippet execute-sleep

-- start snippet evaluate-func-call
evaluateFuncCall' :: Value -> [Expr] -> Interpreter Value
evaluateFuncCall'
  ~func@(Function funcName params body funcDefEnv)
  argEs = do
    checkArgCount funcName argEs (length params)
    funcCallEnv <- State.gets isEnv
    setupFuncEnv
    retVal <- executeBody funcCallEnv
    setEnv funcCallEnv
    return retVal
    where
      setupFuncEnv = do
        args <- traverse evaluate argEs
        env <- overrideVar funcDefEnv funcName func
        env' <- foldM (uncurry . overrideVar) env $ zip params args
        setEnv env'

      overrideVar env name value =
        Map.insert name <$> newIORef value <*> pure env

      executeBody funcCallEnv =
        (traverse_ execute body >> return Null) `catchError` \case
          Return val -> return val
          err -> setEnv funcCallEnv >> throwError err

-- end snippet evaluate-func-call

-- start snippet execute
execute :: Stmt -> Interpreter ()
execute = \case
  ExprStmt expr -> void $ evaluate expr
  VarStmt name expr -> evaluate expr >>= defineVar name
  AssignStmt name expr -> evaluate expr >>= assignVar name
  IfStmt expr body -> do
    cond <- evaluate expr
    when (isTruthy cond) $
      traverse_ execute body
  while@(WhileStmt expr body) -> do
    cond <- evaluate expr
    when (isTruthy cond) $ do
      traverse_ execute body
      execute while
  ReturnStmt mExpr -> do
    mRet <- traverse evaluate mExpr
    throwError . Return . fromMaybe Null $ mRet
  FunctionStmt name params body -> do
    env <- State.gets isEnv
    defineVar name $ Function name params body env
  YieldStmt -> yield
  SpawnStmt expr -> spawn expr
  SendStmt expr chan ->
    evaluate chan >>= \case
      Chan channel -> do
        val <- evaluate expr
        channelSend val channel
      v -> throw $ "Cannot send to a non-channel: " <> show v
  where
    isTruthy = \case
      Null -> False
      Boolean b -> b
      _ -> True

-- end snippet execute

-- start snippet queue-ops
enqueueAt :: TimeSpec -> a -> Queue a -> Interpreter ()
enqueueAt time val queue = atomicModifyIORef' queue $ \(q, maxWakeupTime) ->
  ( ( PQ.insert time val q,
      if time > maxWakeupTime then time else maxWakeupTime
    ),
    ()
  )

enqueue :: a -> Queue a -> Interpreter ()
enqueue val queue = do
  now <- currentSystemTime
  enqueueAt now val queue

currentSystemTime :: (MonadIO m) => m TimeSpec
currentSystemTime = liftIO $ getTime Monotonic

dequeue :: Queue a -> Interpreter (Maybe a)
dequeue queue = atomicModifyIORef' queue $ \(q, maxWakeupTime) ->
  if PQ.null q
    then ((q, maxWakeupTime), Nothing)
    else
      let ((_, val), q') = PQ.deleteFindMin q
       in ((q', maxWakeupTime), Just val)

-- end snippet queue-ops

-- start snippet schedule-delayed-coroutine
scheduleDelayedCoroutine :: TimeSpec -> Coroutine () -> Interpreter ()
scheduleDelayedCoroutine wakeupTime coroutine = do
  State.gets isCoroutines >>= enqueueAt wakeupTime coroutine

-- end snippet schedule-delayed-coroutine

-- start snippet coroutine-ops
scheduleCoroutine :: Coroutine () -> Interpreter ()
scheduleCoroutine coroutine =
  State.gets isCoroutines >>= enqueue coroutine

runNextCoroutine :: Interpreter ()
runNextCoroutine =
  State.gets isCoroutines >>= dequeue >>= \case
    Nothing -> throwError CoroutineQueueEmpty
    Just Coroutine {..} -> do
      void $ takeMVar corReady
      setEnv corEnv
      corCont ()

-- end snippet coroutine-ops

-- start snippet yield
yield :: Interpreter ()
yield = do
  env <- State.gets isEnv
  callCC $ \cont -> do
    newCoroutine env cont >>= scheduleCoroutine
    runNextCoroutine

-- end snippet yield

-- start snippet spawn
spawn :: Expr -> Interpreter ()
spawn expr = do
  env <- State.gets isEnv
  coroutine <- newCoroutine env (const $ evaluate expr >> runNextCoroutine)
  scheduleCoroutine coroutine

-- end snippet spawn

-- start snippet sleep
sleep :: Integer -> Interpreter ()
sleep millis = do
  now <- currentSystemTime
  let wakeupTime = now + fromNanoSecs (fromIntegral millis * 1000000)
  env <- State.gets isEnv
  callCC $ \cont -> do
    scheduleDelayedCoroutine wakeupTime =<< newDelayedCoroutine millis env cont
    runNextCoroutine

-- end snippet sleep

-- start snippet await-term
awaitTermination :: Interpreter ()
awaitTermination = do
  (coroutines, maxWakeupTime) <- readIORef =<< State.gets isCoroutines
  dur <- calcSleepDuration maxWakeupTime
  unless (PQ.null coroutines) $
    if dur > 0
      then sleep dur >> awaitTermination
      else yield >> awaitTermination

-- end snippet await-term

-- start snippet calc-sleep-duration
calcSleepDuration :: TimeSpec -> Interpreter Integer
calcSleepDuration maxWakeupTime = do
  now <- currentSystemTime
  return $ 1 + fromIntegral (maxWakeupTime - now) `div` 1000000

-- end snippet calc-sleep-duration

-- start snippet channel-send
channelSend :: Value -> Channel -> Interpreter ()
channelSend value Channel {..} = do
  bufferSize <- queueSize channelBuffer
  sendQueueSize <- queueSize channelSendQueue

  dequeue channelReceiveQueue >>= \case
    -- there are pending receives
    Just coroutine@Coroutine {..} ->
      scheduleCoroutine $ coroutine {corCont = const $ corCont value}
    -- there are no pending receives and the buffer is not full
    Nothing
      | channelCapacity > 0 && bufferSize < channelCapacity ->
          enqueue value channelBuffer
    -- there are no pending receives and
    -- (the buffer is full or the channel is unbuffered)
    Nothing | sendQueueSize < maxSendQueueSize -> do
      env <- State.gets isEnv
      callCC $ \cont -> do
        coroutine <- newCoroutine env cont
        enqueue (coroutine, value) channelSendQueue
        runNextCoroutine

    -- the send queue is full
    Nothing -> throw "Channel send queue is full"
  where
    maxSendQueueSize = 4

-- end snippet channel-send

-- start snippet channel-receive
channelReceive :: Channel -> Interpreter Value
channelReceive Channel {..} = do
  mSend <- dequeue channelSendQueue
  mBufferedValue <- dequeue channelBuffer
  recieveQueueSize <- queueSize channelReceiveQueue

  case (mSend, mBufferedValue) of
    -- the channel is unbuffered and there are pending sends
    (Just (sendCoroutine, sendValue), Nothing) -> do
      scheduleCoroutine sendCoroutine
      return sendValue

    -- the buffer is full and there are pending sends
    (Just (sendCoroutine, sendValue), Just bufferedValue) -> do
      scheduleCoroutine sendCoroutine
      enqueue sendValue channelBuffer
      return bufferedValue

    -- the buffer is empty and there are no pending sends
    (Nothing, Nothing) | recieveQueueSize < maxReceiveQueueSize -> do
      env <- State.gets isEnv
      callCC $ \receive -> do
        coroutine <- newCoroutine env receive
        enqueue coroutine channelReceiveQueue
        runNextCoroutine
        return Null

    -- the receive queue is full
    (Nothing, Nothing) -> throw "Channel receive queue is full"
    -- the buffer is not empty and there are no pending sends
    (Nothing, Just bufferedValue) -> return bufferedValue
  where
    maxReceiveQueueSize = 4

-- end snippet channel-receive

-- start snippet interpret
interpret :: Program -> IO (Either String ())
interpret program = do
  state <- initInterpreterState
  retVal <-
    flip evalStateT state
      . flip runContT return
      . runExceptT
      . runInterpreter
      $ (traverse_ execute program >> awaitTermination)
  case retVal of
    Left (RuntimeError err) -> return $ Left err
    Left (Return _) -> return $ Left "Cannot return from outside functions"
    Left CoroutineQueueEmpty -> return $ Right ()
    Right _ -> return $ Right ()

-- end snippet interpret

-- start snippet run-file
runFile :: FilePath -> IO ()
runFile file = do
  code <- readFile file
  case runParser program code of
    Left err -> hPutStrLn stderr err
    Right program ->
      interpret program >>= \case
        Left err -> hPutStrLn stderr $ "ERROR: " <> err
        _ -> return ()

-- end snippet run-file

-- start snippet main
main :: IO ()
main =
  getArgs >>= \case
    [file] -> runFile file
    _ -> do
      prog <- getProgName
      hPutStrLn stderr $ "Usage: " <> prog <> " <file>"

-- end snippet main
