type Window =  //windows can be closed or active (i.e. open)
//| Closed
| Active of (int*int*int*int) //x y w h

type List<'a> = //a sentence is a sequence of the parts and it cannot be empty -- a list will hold the parts
| Empty
| V of 'a*List<'a>

type Presence<'b> = 
| Nothing 
| Something of 'b

//general helper functions 
let push w lst = V(w,lst) // to add an V to a list

let rec fold f state x = //to go through a list and accumulate
  (function
  | Empty -> state
  | V (w, nested) -> fold f (f state w) nested
  )x

let rec map f originalList = //to go through a list and transform each V
  fold (fun state digit -> state |> push (f digit)) Empty originalList

let filter condition originalList = //return Vs of a list that meet the conditions of the predicate
  fold (fun state digit -> if (condition digit) then state |> push digit else state) Empty originalList

//start off with an empty  list
let openWindows = Empty
//specific functions
//checker functions:
//A. check with screen dimensions when opening, resizing, moving
let check x y w h sw sh = 
  (x>=0) && (y>=0) && ((x+w)<sw) && ((y+h)<sh)

//B. check if list at coordinates exists
let checkExists x y list = 
  if list = Empty then 
    false
  else 
    let testL = filter (fun (Active(a,b,_,_))-> a=x && b=y) (list)
    if (testL = Empty) then 
      false
    else 
      true

let findWinInList x y list = //returns a list with only the window matching coordinates
  filter (fun (Active(a,b,_,_))-> a=x && b=y) list
  
  
let findWH list = //returns a tuple with windows width and height
  (function
  | V (Active (_, _, w, h), Empty) -> (w,h)
  | _ -> (0,0)
  )list

//get the components of a window in the list
let getX2 win = 
  (function
  |Active (x2,_,_,_) -> x2
  | _ -> 0
  )win
let getY2 win = 
  (function
  |Active (_,y2,_,_) -> y2
  | _ -> 0
  )win
let getW2 win = 
  (function
  |Active (_,_,w2,_) -> w2
  | _ -> 0
  )win
let getH2 win = 
  (function
  |Active (_,_,_,h2) -> h2
  | _ -> 0
  )win

//C. check if window encroaches on another window
let checkEncroach x y w h win = 
  let x2 = getX2 win
  let y2 = getY2 win
  let w2 = getW2 win
  let h2 = getH2 win
  //x is between x2 and (x2 +w2) and y is between y2 and (y2+h2)
  if ((x>=x2 && x<=(x2+w2)) || ((x+w)>=x2 && x<=(x2+w2))) && ((y>=y2 && y<=(y2+h2)) || ((y+h)>=y2 && h<=(y2+h2))) then 
    true
  else false

let nextEncroach x y w h list = 
  fold (fun state digit -> if checkEncroach x y w h digit then state|>push digit else state) Empty list |> (fun a -> if a=Empty then Nothing else Something(a))
//state|> push

//test nextEncroach 
//let xl = push (Active(2, 3,10, 40)) Empty
//let c = nextEncroach 4 5 6 7 xl

let noEncroachment list = 
  if list = Nothing then
    true
  else 
    false

//1. Function for opening windows
let append x y w h list sw sh = 
  //let listEn = nextEncroach x y w h list
  //let c = noEncroachment listEn
  let c = nextEncroach x y w h list |> noEncroachment
  let b = check x y w h sw sh
  if b && c then
    Something (push (Active (x,y,w,h)) list)
  else
    Nothing 

//2. Function for closing windows
let remove x y list sw sh = 
  if ((checkExists x y list) = false) then 
    Nothing
  else
    //filter windows, remove the one with matching x y
    Something (filter (fun (Active(a,b,_,_))-> a<>x || b <> y) (list))
   
//example: remove 5 7 (append 4 5 6 7 openWindows |> append 5 7 8 9) //part in brackets is example list

//3. Two functions for resizing windows
//first create a new window with new size spec and then swap it out with the old one
let swapAWindow condition newWin originalList = //similar to filter function, except swaps a window in a list (if it meets condition) for a new window
  fold (fun state digit -> if (condition digit) then state |> push newWin else state |> push digit) Empty originalList

let changeSizeOfOpenWindow x y w h list sw sh = 
  let wz = Active (x,y,w,h)
  if (check x y w h sw sh) && (checkExists x y list) && (nextEncroach x y w h list |> noEncroachment) then 
    Something(swapAWindow (fun (Active(a,b,_,_))-> a=x && b=y) wz list)
  else 
    Nothing
//example: changeSizeOfOpenWindow 5 7 69 83 (append 4 5 6 7 openWindows |> append 5 7 8 9)


//d. functions for getting max movement allowed without overlapping screen boundaries
let moveMaxAmount x y dx dy list sw sh = 
  let oneWinList = findWinInList x y list
  let whtup = findWH oneWinList
  //check if overlap if move 
  let width = (fun (a,b) -> a)whtup
  let height = (fun (a,b) -> b)whtup
  if (check (x+dx) (y+dy) width height sw sh) then 
    (dx, dy)
  else
    if (x+dx+width)>=sw then 
      ((sw-width-x-1),dy)
    else
      (dx,(sh-height-y-1)) 

  
//4. Two functions for moving windows 
let moveHorizontally win dist = 
  (function 
  //| Closed -> Closed
  | Active (a,b,c,d) -> Active (a+dist,b,c,d) //add to the x value
  )win
//example: moveHorizontally (Active(2,3,4,5)) 10
 
let moveVertically win dist = 
  (function 
  //| Closed -> Closed
  | Active (a,b,c,d) -> Active (a,b+dist,c,d) //add to the y value
  )win
//example: moveVertically (Active(2,3,4,5)) 10

let first list = 
  (function
  | Something(V(v,nested))-> v
  |_-> Empty
  )

let moveWindow x y dx dy list sw sh = 
  if (dx <> 0 && dy <> 0) then 
    Nothing
  else
    let condition = (fun (Active(a,b,_,_))-> a=x && b=y)
    let maxMoves = moveMaxAmount x y dx dy list sw sh //problem line
    let maxDX = (fun(a,b)->a)maxMoves
    let maxDY = (fun(a,b)->b)maxMoves
    //let maxDX = dx
    //let maxDY = dy
    if dx<>0 then 
      Something(fold (fun state digit -> if (condition digit) then state |> push (moveHorizontally digit maxDX) else state |> push digit) Empty list)
    else
      Something (fold (fun state digit -> if (condition digit) then state |> push (moveVertically digit maxDY) else state |> push digit) Empty list)


let moveWindowInList x y dx dy list sw sh = 
  if dx = 0 && dy = 0 then
    Something(list)
  else if dx <> 0 && dy <> 0 then
    Nothing
  else
      let listOfWindow = findWinInList x y list
      //get w and h
      let widthHeightTuple = findWH listOfWindow 
      let w = (fun (a,_)-> a) widthHeightTuple + dx
      let h = (fun (_,a)-> a) widthHeightTuple + dy
      //let windowsToBump = nextEncroach x y w h list
      if dy=0 then
        let newLst = (fold (fun state (Active(a,b,c,d)) -> if (checkEncroach x y w h (Active(a,b,c,d))) && (a<>x || b<>y) then state|> push (Active(x+w+1,b,c,d)) else state|>push (Active(a,b,c,d))) Empty list )
        moveWindow x y dx dy newLst sw sh
      else
        let newLst = (fold (fun state (Active(a,b,c,d)) -> if (checkEncroach x y w h (Active(a,b,c,d))) && (a<>x || b<>y) then state|> push (Active(y+h+1,b,c,d)) else state|>push (Active(a,b,c,d))) Empty list )
        moveWindow x y dx dy newLst sw sh

    //move
//example: moveWindowInList 5 7 10 0 (append 4 5 6 7 openWindows |> append 5 7 8 9)

//Handling the commands
type Command = 
| Open of int*int*int*int
| Close of int*int
| Move of int*int*int*int
| Resize of int*int*int*int

let doCommand cmd list sw sh = 
  (function 
  | Open (x,y,w,h) -> (append x y w h list sw sh)
  | Close (x,y) -> (remove x y list sw sh)
  | Resize (x,y,w,h) -> (changeSizeOfOpenWindow x y w h list sw sh)
  | Move (x,y,dx,dy) -> (moveWindowInList x y dx dy list sw sh)
  )cmd
//test doCommand
//doCommand (Close(2,3))
//doCommand (Open(8,9,10,20))

//functions taken from the warmup to parse the string that the user inputs
let stringFold f initial (s : string) = 
  Seq.fold (fun state ch -> f state (string ch)) initial s 

type PartOfSentence = //everything a sentence can have
| Word of string
| Space
| Number of string
| Negative

type Input = //a sentence could be valid or invalid -- need to give that back unchanged
| Valid of List<PartOfSentence>
| Invalid of string

let stringLength = stringFold (fun n _ -> n + 1) 0
//if stringlength is 1, could we quit?

//check if characters are in a set
let isInSet set s = 
  stringFold (fun state item -> state || item = s) false set

//list of valid input characters
let onlyValidChars = 
  stringFold (fun state item -> state && isInSet "MOVERSIZCLPNQUT1234567890 -" item) true //characters in move, resize, quit, close, open, numbers, and spaces will be valid

//check if we begin with a word
let stringReverse = stringFold (fun state item -> item + state) ""

//10. first character is last character of reversed string
let stringLast = stringFold (fun _ item -> item) ""

//11. first function 
let stringFirst = stringReverse >> stringLast

let stringToList = 
  stringReverse >> stringFold (fun state item -> V (item, state)) Empty

//create a map function out of 3 functions
let listFold f initial = 
  let rec listFold state = 
    function 
    | Empty -> state
    | V (item, rest) -> listFold (f state item) rest
  listFold initial

let listReverse list = 
  listFold (fun state item -> V (item, state)) Empty list

let listMap f list = 
  listReverse list
  |> listFold (fun state item -> V (f item, state)) Empty

//function to categorise different things
let toPartOfSentence s = 
  if s = " " then 
    Space
  elif s = "-" then 
    Negative
  elif s = "0"|| s="1" ||s="2"||s="3"||s="4"||s="5"||s="6"||s="7"||s="8"||s="9" then 
    Number s
  else 
    Word s

let rec merge = //merge adjacent letters into words and merge adjacent numbers into bigger numbers
  function 
  | Empty -> Empty
  | V (Number c, V(Number d, rest)) -> merge (V(Number(c+d), rest))
  | V (Word a, V (Word b, rest)) -> merge (V (Word (a+b), rest)) 
  | V (x, rest) -> V (x, merge rest)

//example: "OPEN 2 5 67 80" |> stringToList |> listMap toPartOfSentence |> merge //testing a possible user input string

// the aim of stringListToCommand is to convert strings like "OPEN 8 9 10 20" --> doCommand (Open(8,9,10,20)) using a pattern matching function 
let stringListToCommand listA workingList sw sh =
  (function 
  | V (Word "OPEN", V (Space, V (Number a, V (Space, V (Number b,V (Space, V (Number c, V (Space, V (Number d, Empty))))))))) -> doCommand(Open((int a),(int b),(int c),(int d))) workingList sw sh
  | V (Word "CLOSE", V(Space, V (Number a, V(Space, V (Number b, Empty))))) -> doCommand (Close((int a),int b)) workingList sw sh
  | V (Word "RESIZE", V (Space, V (Number a, V (Space, V (Number b,V (Space, V (Number c, V (Space, V (Number d, Empty))))))))) -> doCommand(Resize((int a),(int b),(int c),(int d))) workingList sw sh
  | V (Word "MOVE", V (Space, V (Number a, V (Space, V (Number b,V (Space, V (Number c, V (Space, V (Number d, Empty))))))))) -> doCommand(Move((int a),(int b),(int c),(int d))) workingList sw sh
  | V (Word "MOVE", V (Space, V (Number a, V (Space, V (Number b,V (Space, V (Negative, V(Number c, V (Space, V (Number d, Empty)))))))))) -> doCommand(Move((int a),(int b),(-1*(int c)),(int d))) workingList sw sh
  | V (Word "MOVE", V (Space, V (Number a, V (Space, V (Number b,V (Space, V (Number c, V (Space, V (Negative, V (Number d, Empty)))))))))) -> doCommand(Move((int a),(int b),(int c),(-1*(int d)))) workingList sw sh
  | _ -> Nothing
  )listA

let makeTheList input list sw sh = 
  stringListToCommand (input |> stringToList |> listMap toPartOfSentence |> merge) list sw sh

//working on making a string printer function 
let rec toString myList =
  (function
  | Empty -> " "
  | V (Active (a,b,c,d), Empty) -> string a+" "+string b+" "+string c+" "+string d
  //| Element (v, nested) -> string v + ", " + toString nested
  | V (Active (a,b,c,d), nested) -> string a+" "+string b+" "+string c+" "+string d+"\n"+toString nested
  ) myList

let listString list = 
  (function
  | Nothing -> toString Empty
  | Something (l) -> toString l
  )list


let rec evaluate io list screenWidth screenHeight = 
  if io = "QUIT" then 
    Nothing
  else
    let result = makeTheList io list screenWidth screenHeight
    if result = Nothing then
      System.Console.WriteLine("Enter next command: ")
    else 
      let strRes = listString result
      System.Console.WriteLine($"{strRes}")
      System.Console.WriteLine("Enter next command: ")
    let userInput = System.Console.ReadLine()
    (function
    | Nothing -> evaluate userInput list screenWidth screenHeight
    | Something(r) -> evaluate userInput r screenWidth screenHeight
    )result

let simulate x y = //this function will be the main one 
  System.Console.SetWindowSize(x,y)
  System.Console.WriteLine($"The dimensions of the screen are {System.Console.WindowWidth} by {System.Console.WindowHeight} pixels") 
  let firstCommand = "HOPE"
  ignore(evaluate firstCommand openWindows x y)

simulate 500 100
//changes to make:
//move --if encroaches onto another window, all touching windows need to be transformed
