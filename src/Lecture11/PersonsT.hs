{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lecture11.PersonsT where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Prelude hiding (id)
import Lecture10.Reader (Person (..), PersonId, Sex (..), persons, processSingle, processPair)


-- <Задачи для самостоятельного решения>

{-
  В этом задании нужно адаптировать код из 10 лекции к новым требованиям
  при помощи трансформеров.

  1. Необходимо собирать статистику найденных людей:
    - количество одиноких персон
    - количество замужних персон
  2. В функцию `findById` добавить логгирование с помощью монады Write.
    Нужно сообщать была ли найдена персона по данному id или нет.

  Вы можете переиспользовать функции, которые остались без изменения из Lecture10.Reader
-}

data PersonSearchStats = PersonSearchStats
  { marriedPersonsCount :: Integer
  , singlePersonsCount :: Integer
  } deriving (Show)

emptyStats :: PersonSearchStats
emptyStats = PersonSearchStats 0 0

newtype PersonsT a = PersonsT
  { runPersonsT ::  (ReaderT [Person] (StateT PersonSearchStats (Writer [String])) a) }
  deriving
    ( Functor
    , Applicative
    , Monad    
    , MonadWriter [String]
    , MonadState PersonSearchStats
    , MonadReader [Person]
    )

runPersons :: PersonsT a -> ((a, PersonSearchStats), [String])
runPersons = runWriter . flip runStateT emptyStats . flip runReaderT persons . runPersonsT 

findById :: PersonId -> PersonsT (Maybe Person)
findById pId = do
  persons <- ask
  let foundPerson = find (\p -> id p == pId) persons
  case foundPerson of
    Just p -> tell ["Found " ++ show p]
    _ -> tell ["Couldn't find person with id " ++ show pId]
  return foundPerson

getMarried :: Maybe Person -> PersonsT (Maybe Person)
getMarried Nothing = return Nothing
getMarried (Just p) = do
  case marriedBy p of
    Nothing -> return Nothing
    Just pId -> findById pId

processPerson :: PersonId -> PersonsT (Maybe String)
processPerson pId =  do
    put emptyStats
    person <- findById pId
    married <- getMarried person
    case (person, married) of 
      (Nothing, _) -> return Nothing
      (Just p, Nothing) -> return $ Just $ processSingle p
      (Just h@(Person _ _ _ _ Male _), Just w) -> return $ Just $ processPair h w
      (Just w, Just h) -> return $ Just $ processPair h w

{-
  Функция должна выводить на экран:
  - общую поисковую статистику по всем найденым персонам.
  - результат каждого поиска

  Записывать в "persons.log" общий лог всех поисков.
-}

processWithId :: PersonId -> PersonsT (PersonId, Maybe String)
processWithId pId = do
  processed <- processPerson pId
  return $ (pId, processed)

printResultWithId :: (PersonId, Maybe String) -> IO ()
printResultWithId (pId, result) = putStrLn $ "Id: " ++ show pId ++ " - " ++ show result

processPersons :: [PersonId] -> IO ()
processPersons personIds = do
  let ((results, (PersonSearchStats married single)), logs) = runPersons $ mapM processWithId personIds

  mapM_ printResultWithId results

  putStrLn $ "Total count: \n" ++ show (married + single)
  putStrLn $ "Married count: \n" ++ show married
  putStrLn $ "Single count: \n" ++ show single

  writeFile "persons.log" $ show logs

-- </Задачи для самостоятельного решения>
