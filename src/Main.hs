module Main where

import Data.String.Utils
import Text.Read (readMaybe)

type Name = String
type Phone = String
data PhoneRecord = PhoneRecord Name Phone
  deriving (Show)

getName :: PhoneRecord -> Name
getName (PhoneRecord n _) = n

getPhone :: PhoneRecord -> Phone
getPhone (PhoneRecord _ p) = p

addNewPhoneRecord :: (Name, Phone) -> [PhoneRecord] -> [PhoneRecord]
addNewPhoneRecord (name, phone) old =
  old ++ [PhoneRecord name phone]

addNewPhoneRecordUI :: [PhoneRecord] -> IO [PhoneRecord]
addNewPhoneRecordUI base = do
  putStrLn "Введите запись в следующем формате \"Имя Телефон\""
  input <- readFromConsole
  case input of
    Just (x, y) -> return $ addNewPhoneRecord (x, y) base
    Nothing -> do
      putStrLn "Ошибка добавления новой записи"
      return base

parse :: [String] -> Maybe (Name, Phone)
parse [x, y] = Just (x, y)
parse _ = Nothing

readFromConsole :: IO (Maybe (Name, Phone))
readFromConsole = do
  line <- getLine
  let phr = split " " line
  return $ parse phr

-- Сортировочку <- от пакета Data.List

find :: [PhoneRecord] -> Name -> Maybe Phone
find [] _ = Nothing
find list name 
  | name == getName h = Just $ getPhone h
  | otherwise = find (tail list) name
  where h = head list

findUI :: [PhoneRecord] -> IO ()
findUI list = do
  putStrLn "Введите имя"
  input <- getLine
  let result = find list input
  case result of
    Just phone -> putStrLn $ "Найденый телефон: " ++ phone
    Nothing -> putStrLn "Ничего не найдено"

deleteRecord :: [PhoneRecord] -> Name -> [PhoneRecord]
deleteRecord [] _ = []
deleteRecord base name 
  | name == getName h = tail base
  | otherwise = h:deleteRecord (tail base) name
  where h = head base

deleteRecordUI :: [PhoneRecord] -> IO [PhoneRecord]
deleteRecordUI base = do
  putStrLn "Введите имя для удаления записи"
  input <- getLine
  let phone = find base input
  case phone of
    Just _ -> return $ deleteRecord base input
    _ -> do
      putStrLn "Запись не найдена"
      return base

editRecord :: [PhoneRecord] -> Name -> Phone -> [PhoneRecord]
editRecord [] _ _ = []
editRecord base name phone
  | name == getName h = PhoneRecord name phone : tail base
  | otherwise = h:editRecord(tail base) name phone
  where h = head base

editRecordUI :: [PhoneRecord] -> IO [PhoneRecord]
editRecordUI base = do
  putStrLn "Введите запись в следующем формате \"Имя Телефон\""
  input <- readFromConsole
  case input of
    Just (x, y) -> return $ editRecord base x y
    Nothing -> do
      putStrLn "Ошибка изменения записи"
      return base

showBase :: [PhoneRecord] -> IO ()
showBase [] = putStrLn "На этом все..."
showBase (x:xs) = do
  putStrLn $ getName x ++ " " ++ getPhone x
  showBase xs


uiLoop :: [PhoneRecord] -> IO ()
uiLoop base = do
  putStrLn ""
  putStrLn "Выберите пункт меню:"
  putStrLn "1) Добавление телефона в книгу"
  putStrLn "2) Поиск телефона по имени"
  putStrLn "3) Просмотреть базу"
  putStrLn "4) Удаление записи из адресной книги"
  putStrLn "5) Изменения записи"
  putStrLn "6) Выход"
  input <- getLine
  let option = readMaybe input
  case option of
    Just 1 -> do
      base <- addNewPhoneRecordUI base
      uiLoop base
    Just 2 -> do
      findUI base
      uiLoop base
    Just 3 -> do
      showBase base
      uiLoop base
    Just 4 -> do
      base <- deleteRecordUI base
      uiLoop base
    Just 5 -> do
      base <- editRecordUI base
      uiLoop base
    Just 6 -> putStrLn "Всего доброго"
    _ -> do
      putStrLn "Ошибка выбора пункта меню"
      uiLoop base


main :: IO ()
main = uiLoop []
