(* Author: Susan Huang *)

let stringList = [["a";"h";"i"]; ["k";"a";"h";"i"];
["u";"a";"h";"i"]; ["a";"l";"o";"h";"a"]; 
["h";"u";"a";"l";"i"];["k";"a";"k";"a";"h";"i"]; 
["u";"a";"i"]; ["u";"h";"a";"i"];
["k";"u";"a";"i"]; ["w";"a";"w";"a";"i"]; 
["h";"a";"w";"a";"i";"g";"i"];["m";"e";"l";"e"];
["k";"a";"l";"i";"k";"i";"m";"a";"k";"a"];
["k";"l";"i";"k";"m";"a";"k"];["k";"r";"I";"m";"c";"s"];
["a";"a";"a";"a";"a";"a";"a"]];;

let stringList2 = [["ahi"]; ["kahi"]; ["uahi"]; ["aloha"]; ["huali"]; ["kakahi"];
["uai"]; ["uhai"]; ["kuai"]; ["wawai"]; ["hawaigi"]; ["mele"]; ["kalikimaka"];
["klikmak"]; ["krIsmcs"]; ["aaaaaaa"]];;

(* Function that checks if the string is in the string list *)

let rec findString searchString in_list =
match in_list with
| [] -> false
| h1::t1 -> if (searchString = h1) then true 
else (findString searchString t1);;

(* Test Cases *)
(* strings in the Lexicon *)
let findStringTest1 = findString ["ahi"] stringList2;;
let findStringTest2 = findString ["kuai"] stringList2;;
(* string that is not in the Lexicon *)
let findStringTest3 = findString [] stringList2;;
let findStringTest4 = findString ["melek"] stringList2;;

(* Convert strings to char list *)
let stringToChar s = 
let rec exp i l =
if i < 0 then l else exp (i-1) (s.[i]::l) in
exp (String.length s - 1) [];;

(* Helper Function #1 to has_prefix function *)
let rec stringIter prefix string1 =
match (prefix, string1) with
| ([],[]) -> true
| (h1::t1, h2::t2) -> if (h1 = h2) then (stringIter t1 t2) else false
| ([],_) -> true
| (_,[]) -> false;;

(* Helper function #2 to has_prefix function *)
let rec findPrefix prefix stringList = 
match stringList with
| [] -> false
| h1::t1 -> if (stringIter (stringToChar prefix) (stringToChar (List.hd h1))) then true 
else findPrefix prefix t1;;

(* Function that checks for prefixes *)
let has_prefixList prefix stringList=
match prefix with 
| [] -> true
| _ -> findPrefix (List.hd prefix) stringList;;

(* Helper Function #1 to has_prefix function *)
let rec stringIter prefix string1 =
match (prefix, string1) with
| ([],[]) -> true
| (h1::t1, h2::t2) -> if (h1 = h2) then (stringIter t1 t2) else false
| ([],_) -> true
| (_,[]) -> false;;

(* Helper function #2 to has_prefix function *)
let rec findPrefix prefix stringList = 
match stringList with
| [] -> false
| h1::t1 -> if (stringIter (stringToChar prefix) (stringToChar (List.hd h1))) then true 
else findPrefix prefix t1;;

(* Function that checks for prefixes *)
let has_prefixList prefix stringList =
match prefix with 
| [] -> true
| _ -> findPrefix (List.hd prefix) stringList;;

let has_prefix prefix stringList = 
match prefix with
| [] -> true
| _ -> stringIter (stringToChar (List.hd prefix)) (stringToChar (List.hd stringList));;

(* Test Cases for Prefix Finding in all string lists *) 
let prefixListTest1 = has_prefixList [] stringList2;;
let prefixListTest1 = has_prefixList ["ahi"] stringList2;;
let prefixListTest2 = has_prefixList ["le"] stringList2;;

(* Test cases for prefix finding in one string *)
let testString1 = ["mele"];;
let prefixTest2 = has_prefix [] ["mele"];;
let prefixTest3 = has_prefix ["me"] ["mele"];;
let prefixTest4 = has_prefix ["le"] ["mele"];;
let prefixTest5 = has_prefix ["ahi"] ["ahi"];;

(* Function that checks to see if an n-gram is in a string list *)

let rec findNGram ngram word =
match (ngram, word) with 
| ([],_) -> true
| (_,[]) -> false
| (h1::t1, h2::t2) -> if (h1 = h2) then (if (findNGram t1 t2) then true else false) else findNGram (h1::t1) t2;;

let has_ngram ngram word  =
match ngram with
| [] -> true
| h1::t1 -> findNGram (stringToChar h1) (stringToChar(List.hd word));;

(* Test Cases for has_ngram *)
let ngramTest1 = has_ngram ["le"] ["mele"];;
let ngramTest2 = has_ngram ["mak"] ["aloha"];;


