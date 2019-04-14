(* NAME AND HONOR PLEDGE:
 * I pledge my honor that I have abided by the Stevens Honor System.
 * -Daniel Kramer *)
type  program = int list;;
let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1];;
let squareEq : program = [2; 2; 0; 3; 3; 4; 4; 5; 5; 2; 2; 1];;
let squareEq2 : program = [0; 3; 3; 2; 2; 5; 5; 4; 4; 1];;
let notSquare : program = [0; 3; 3; 2; 2; 5; 5; 1];;
(*Additional programs added for testing purposes *)

let get_1of2 (a,_) = a
let get_2of2 (_,a) = a
(* Found out this is a way to get elements of tuples from a list item
 * after a lot of experimentation and searching online.
 * Used in decompress.
 *)

let rec coloredList : int*int -> int -> program -> (int*int) list =
	fun coord status xs ->
		match coord,status,xs with
		| (a,b),0,[] -> [(a,b)]
		| (a,b),1,[] -> []
		| (a,b),0,0::xs -> (a,b) :: (coloredList (a,b) 0 xs)
		| (a,b),0,1::xs -> (a,b) :: (coloredList (a,b) 1 xs)
		| (a,b),0,2::xs -> (a,b) :: (coloredList (a,(b+1)) 0 xs)
		| (a,b),0,3::xs -> (a,b) :: (coloredList ((a+1),b) 0 xs)
		| (a,b),0,4::xs -> (a,b) :: (coloredList (a,(b-1)) 0 xs)
		| (a,b),0,5::xs -> (a,b) :: (coloredList ((a-1),b) 0 xs)
		| (a,b),1,0::xs -> (coloredList (a,b) 0 xs)
		| (a,b),1,1::xs -> (coloredList (a,b) 1 xs)
		| (a,b),1,2::xs -> (coloredList (a,(b+1)) 1 xs)
		| (a,b),1,3::xs -> (coloredList ((a+1),b) 1 xs)
		| (a,b),1,4::xs -> (coloredList (a,(b-1)) 1 xs)
		| (a,b),1,5::xs -> (coloredList ((a-1),b) 1 xs)

(*Cases (note to self): 
Last and drawing
Last and not drawing
not last and drawing->drawing 2-5
not last and drawing->not drawing 0,1
not last and not drawing->drawing 1,0
not last and not drawing->not drawing 2-5
*)

let colored : int*int -> program -> (int*int) list = fun coord p ->
	match coord,p with
	| (_,_),[] -> []
	| (a,b),0::xs -> List.sort_uniq compare (coloredList (a,b) 0 xs)
	| (a,b),1::xs -> List.sort_uniq compare (coloredList (a,b) 1 xs)
	| (a,b),2::xs -> List.sort_uniq compare (coloredList (a,(b+1)) 1 xs)
	| (a,b),3::xs -> List.sort_uniq compare (coloredList ((a+1),b) 1 xs)
	| (a,b),4::xs -> List.sort_uniq compare (coloredList (a,(b-1)) 1 xs)
	| (a,b),5::xs -> List.sort_uniq compare (coloredList ((a-1),b) 1 xs)

(* (List.sort_uniq compare LIST) sorts list in ascending order
 * and removes duplicate elements (in Ocaml version >4.02)
 * AND, matches with (_,_),x>6::xs ignored intentionally here and in
 * coloredList helper function. Given warning but still compiles
 * Finally, a I assumed a program starts with pen up (not drawing) 
*)

let equivalent : program -> program -> bool = fun a b ->
	(colored (0,0) a) = (colored (0,0) b)
	(* True -> True, False -> False *)

let  rec  mirror_image: program -> program = function
	[] -> []
	| (2::xs) -> 4 :: (mirror_image xs)
	| (3::xs) -> 5 :: (mirror_image xs)
	| (4::xs) -> 2 :: (mirror_image xs)
	| (5::xs) -> 3 :: (mirror_image xs)
	| (x::xs) -> x :: (mirror_image xs)
	(* Switch 2 and 4, 3 and 5 *)

let  rec  rotate_90: program -> program = function
	[] -> []
	| (2::xs) -> 3 :: (rotate_90 xs)
	| (3::xs) -> 4 :: (rotate_90 xs)
	| (4::xs) -> 5 :: (rotate_90 xs)
	| (5::xs) -> 2 :: (rotate_90 xs)
	| (x::xs) -> x :: (rotate_90 xs)
	(* 2-4 -> x+1, 5 -> 2 *)

let rec repeat: int -> 'a -> 'a list = fun n x ->
	match n with
		0 -> []
		| n -> x :: (repeat (n-1) x)

let rec pantograph : program -> int -> program = fun p n ->
	match p with
	[] -> []
	| (x::xs) -> 
		if (x >= 2)
		then (repeat n x) @ (pantograph xs n)
		else x :: (pantograph xs n)

(* compress is further below because it was more difficult for me *)
let rec decompress : (int*int) list -> program = function
	| [] -> []
	| (x::xs) -> repeat (get_2of2(x)) (get_1of2(x)) @ decompress xs


let rec compressH : int*int -> program -> (int*int) list = fun t xs ->
	match t,xs with
	| (m,n),[] -> [(m,n)] (* [wrapped] so returns int*int list *)
	| (m,n),xs::xss -> 
		if (m = xs)
		then compressH (m,(n+1)) xss
		else (m,n) :: compressH (xs, 1) xss

let compress : program -> (int*int) list = function
	| [] -> []
	| (x::xs) -> compressH (x,1) xs

	(*CompressH (defined above) is the compress Helper function *)