
--Вариант 1
--Написать функцию addWord :: String -> Dictionary -> Dictionary,
--которая добавляет заданное слово в словарь,
--представленный в виде префиксного дерева, и выдает модифицированный словарь в качестве результата.
--Если такое слово уже было в дереве, то ничего не меняется.

--взято с уловия, добавлено извлечение show
data Trie = Empty | Node Char [Trie]
  deriving(Show)
--переименнование
type Dictionary = [Trie]

addWord :: String -> Dictionary -> Dictionary
addWord [] [Empty] = [Empty]
addWord [] dictionary = dictionary++[Empty]
addWord (x:xs) [] = (addWord1 (x:xs) [Empty]) 
addWord (x:xs) [Empty] = (addWord1 (x:xs) [Empty])++[Empty]
addWord (x:xs) (y@(Node char list):ys)
  | x == char = (Node x (addWord xs list)) : ys
  | otherwise = y : addWord (x:xs) ys
--основная функция. логично, что если мы добавляем пустое слово,
--то ничего менять не надо
--если добавляем в уже существующий словарь, то
--ищем место 1) если совпало, то идем вглубь
--на место чара, вставляем начало передонного слова(совпало же)
--запускаем вставку от остатка слова и нужного нам дерева,
--оставшиеся деревья просто присоединяем
--2)если на этом дереве не нашли, то прицепляем его вершину впереди
--сами начинаем искать это же слово, в  оставшихся деревьях
--когда настал момент, что больше деревьев нет, то это значит
--что мы добавляем что-то новое: строим цепочку из
--чаров + в конце сслыку на пустое место

addWord1 [] dictionary = dictionary
addWord1 (x:xs) [Empty] = (Node x (addWord1 xs [Empty])):[]
--функция, которая строит цепочку чаров
--начало слова пишем в нод, в первое дерево пишем начало
--нового слова, в дерево, созданное первым деревом пишем
--начало нового-нового слова ... и тд, рекурсия

--Вариант 3
--Написать функцию listWords :: Dictionary -> [String], которая выдает список всех слов в заданном словаре.
listWords :: Dictionary -> [String]
listWords [Empty] = []
listWords dictionary = words $ listWords1 dictionary ""
--если надо узнать, какие слова есть в пустом списке, то их там нет.
--иначе, разделим на слова строку, которую создаст наша вспомогательная функция
listWords1 [Empty] string = string ++ " "
listWords1 [] _ =  ""
listWords1 ((Node char list):ys) string = listWords1 list (string++[char]) ++ listWords1 ys string
--пройдем все дерево, запуская нашу функция от вершины деревья вглубь(добавляя вершину к накопленному), и от соседней
--вершины вширь(передавая накопленное)
--если дошли до того момента, что соседей уже нет, но и конца слова нет, то ничего не делаем
--иначе, идем до конца слова, когда дошли, печатаем нашу накопительную строку с пробелом
--он нужен, чтобы легко превратить строку слов в массив слов(по заданию)


--Вариант 2
--Написать функцию removeWord :: String -> Dictionary -> Dictionary, которая удаляет заданное слово из словаря,
--представленного в виде префиксного дерева, и выдает модифицированный словарь в качестве результата.
--Если такого слова не было в дереве, то ничего не меняется.
addWords :: [String] -> Dictionary
addWords strings = foldr (addWord) [] strings
--вспомогательная функция, которая добавляет много слов в пустой словарь
deleteWord ::String -> [String] -> [String]
deleteWord word xs = [ x | x <- xs, x /= word]
--функция, удаляющая слово из массива слов

removeWord :: String -> Dictionary -> Dictionary
removeWord word dictionary = addWords $ deleteWord word $ listWords dictionary
--чтобы удалить слово, нам бы все равно пришлось бы перезаписывать бор
--так что мы можем узнать какие слова в нем есть
--удалить оттуда нужное,если есть
--построить новый бор по оставшимся словам
