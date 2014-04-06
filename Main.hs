module Main where

import           Conduit
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Attoparsec.Text as Atto
import           Data.Functor.Foldable (Fix)
import qualified Data.Functor.Foldable as R
import           Data.Monoid
import           Data.Text
import           Language.Haskell.Interpreter
import           Options.Applicative as OA
import           Prelude hiding (log)
import           System.IO

version :: String
version = "0.0.1"

copyright :: String
copyright = "2014"

summary :: String
summary = "magit-helper v" ++ version ++ ", (C) John Wiegley " ++ copyright

data Options = Options
    { verbose  :: Bool
    }

options :: OA.Parser Options
options = Options
    <$> switch (long "verbose" <> help "Display statistics")
    -- <*> strOption (long "file" <> help "Active timelog file to use")
    -- <*> strOption (long "period" <> help "Period to report for" <> value "")

    -- <*> strOption (long "category"
    --                <> help "Account/category to query from timelog"
    --                <> value "")

    -- <*> strOption (long "archive" <> help "Archival timelog" <> value "")
    -- <*> option (long "gratis" <> help "Hours given free each month" <> value 0)

    -- <*> option (long "moment" <> help "Set notion of the current moment"
    --             <> value (unsafePerformIO $
    --                       (zonedTimeToLocalTime <$>) getZonedTime)
    --             <> reader (ReadM . Right . flip LocalTime midday . fromJust .
    --                        Atto.maybeResult .
    --                        Time.parseWithDefaultOptions Time.defaultDay .
    --                        B.pack))

type App = ReaderT Options IO

main :: IO ()
main = execParser opts >>= const commandLoop
  where
    opts = info (helper <*> options)
                (fullDesc
                 <> progDesc "Show hours worked so far"
                 <> header "hours - show hours worked so far")

data Atom = ANil | AString String | ASymbol String
    deriving Show

data LispF f = LAtom Atom
             | LCons f f
             | LQuote f
    deriving (Show, Functor)

type Lisp = Fix LispF

class ToLisp a where
    toLisp :: a -> Text

class FromLisp a where
    fromLisp :: Text -> a

evalLisp :: LispF (Interpreter String) -> Interpreter String
evalLisp (LAtom ANil) = return "nil"
evalLisp (LAtom (AString s)) = return $ "\"" ++ s ++ "\""
evalLisp (LAtom (ASymbol s)) = return s
evalLisp (LQuote a)  = a
evalLisp (LCons a b) = do
    fun  <- a
    args <- b
    eval $ fun ++ " " ++ args

commandLoop :: IO ()
commandLoop = sourceHandle stdin $= linesUnboundedC $$ mapM_C $ \line ->
    case parseOnly parseLisp line of
        Left err -> error err
        Right l -> do
            print l
            eres <- runInterpreter $ do
                setImportsQ [("Prelude", Nothing)]
                R.cata evalLisp l
            print eres
            case eres of
                Left e    -> throwM e
                Right res -> yield res $$ sinkHandle stdout
  where
    parseLisp :: Atto.Parser Lisp
    parseLisp =
        ("nil" *> pure (R.Fix $ LAtom ANil))
        <|>
        ("\"" *> (R.Fix . LAtom . AString <$> manyTill anyChar (try "\"")))
        <|>
        ("(" *> liftA2 ((R.Fix .) . LCons) (parseLisp <* space) parseLisp <* ")")
        <|>
        (R.Fix . LAtom . ASymbol <$> many letter)
