module Main where

import           Data.String.Utils
import           Text.Read         (readMaybe)

data PhoneRecord = PhoneRecord {
  name,
  phone :: String
} deriving (Show)

addNewPhoneRecord :: (String, String) -> [PhoneRecord] -> [PhoneRecord]
addNewPhoneRecord (name, phone) old =
  old ++ [PhoneRecord {
    name = name,
    phone = phone
  }]

addNewPhoneRecordUI :: [PhoneRecord] -> IO [PhoneRecord]
addNewPhoneRecordUI base = do
  putStrLn "Введите запись в следующем формате \"Имя Телефон\""
  input <- readFromConsole
  case input of
    Just (x, y) -> return $ addNewPhoneRecord (x, y) base
    Nothing -> do
      putStrLn "Ошибка добавления новой записи"
      return base

parse :: [String] -> Maybe (String, String)
parse [x, y] = Just (x, y)
parse _      = Nothing

readFromConsole :: IO (Maybe (String, String))
readFromConsole = do
  line <- getLine
  let phr = split " " line
  return $ parse phr

-- Сортировочку <- от пакета Data.List

find :: [PhoneRecord] -> String -> Maybe String
find [] _ = Nothing
find list iName
  | iName == name h = Just $ phone h
  | otherwise = find (tail list) iName
  where h = head list

findUI :: [PhoneRecord] -> IO ()
findUI list = do
  putStrLn "Введите имя"
  input <- getLine
  let result = find list input
  case result of
    Just phone -> putStrLn $ "Найденый телефон: " ++ phone
    Nothing    -> putStrLn "Ничего не найдено"

deleteRecord :: [PhoneRecord] -> String -> [PhoneRecord]
deleteRecord [] _ = []
deleteRecord base iName
  | iName == name h = tail base
  | otherwise = h:deleteRecord (tail base) iName
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

editRecord :: [PhoneRecord] -> String -> String -> [PhoneRecord]
editRecord [] _ _ = []
editRecord base iName iPhone
  | iName == name h = PhoneRecord iName iPhone : tail base
  | otherwise = h:editRecord(tail base) iName iPhone
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
  putStrLn $ name x ++ " " ++ phone x
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
