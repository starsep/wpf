(* Origami *)
(* Filip Czaplicki 359081 *)

type point = float * float

type kartka = point -> int

let sqr x = x *. x

let eps = 1.0e-3

(* operatory porównywania floatów *)
let (=.) x y = 
	abs_float(x -. y) <= eps 
	
let (<=.) x y =
	(x =. y) || (x < y)

(* x >> y zwraca wektor z x do y *)
let (>>) (x1, y1) (x2, y2) =
	(x1 -. x2, y1 -. y2)
	
(* x +> y zwraca sumę wektorów/ punktów *)
let (+>) (x1, y1) (x2, y2) =
	(x1 +. x2, y1 +. y2)

(* zwraca iloczyn wektorowy wektorów (x1, y1) oraz (x2, y2) *) 
let vec (x1, y1) (x2, y2) =
	x1 *. y2 -. x2 *. y1
	
(* zwraca kwadrat odległości pomiędzy punktami p i q *)
let odleglosc2 (x1, y1)  (x2, y2) =
	sqr(x1 -. x2) +. sqr(y1 -. y2)

(* zwraca długość wektora (x, y) *)
(* notabene równe sqrt (odleglosc2 (x, y) (0., 0.) )*)
let dlugosc (x, y) =
	hypot x y

(* zwraca wektor prostopadły do danego o długości ~1. *)
let prostWek (x, y) = 
	let dl = dlugosc (x, y) in
	(y /. dl, -.(x /. dl))

(* zwraca "odległość" punktu (x, y) *)
(* od prostej przechodzącej przez punkty (x1, y1) i (x2, y2) *)
let odlegloscP (x1, y1) (x2, y2) (x, y) =
	let a = y1 -. y2 in
	let b = x2 -. x1 in
	let c = -.(y1 *. b) +. -.(x1 *. a) in
	let d = a *. x +. b *. y +. c in
	d /. (hypot a b)

(* konwersja bool na int *)
let boolToInt x =
	match x with 
	|true -> 1
	|false -> 0

let prostokat p q =
	fun x ->
		boolToInt( (fst p) <=. (fst x) && (fst x) <=. (fst q) &&
		   (snd p) <=. (snd x) && (snd x) <=. (snd q) )
		   
let kolko p r =
	fun x ->
		boolToInt( (odleglosc2 p x) <=. (sqr r) )
		
let wzgledemProstej p q x =
	let z = vec (p >> x) (p >> q) in
	if z =. 0. then `NaProstej
	else if z <=. 0. then `PoLewej
	else `PoPrawej

let odbicie p q x =
	let (a, b) = prostWek(p >> q) in
	let odl = odlegloscP p q x in
	let (da, db) = (-2. *. odl *. a, -2. *. odl *. b) in
	x +> (da, db)
	
let zloz p q f =
	fun x ->
	match wzgledemProstej p q x with
	|`NaProstej -> f x
	|`PoLewej -> (f x) + (f (odbicie p q x))
	|`PoPrawej -> 0
	
let skladaj l f =
	List.fold_left (fun ak el -> zloz (fst el) (snd el) ak) f l;; 
