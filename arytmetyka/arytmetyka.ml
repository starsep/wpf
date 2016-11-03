(************************************************)
(* Zadanie o arytmetyce niedokładnych wartości. *)
(************************************************)

(* Typ reprezentujący niedokładne wartości. *)
type wartosc = (float * float) list

(* wartosc_dokladnosc x p = x +/- p% *)
(* war.pocz.: p > 0                  *)
let pusta = []

let wartosc_dokladnosc x p = 

(* wartosc_od_do x y = [x;y]         *)
(* war.pocz.: x <= y                 *)
let wartosc_od_do x y =                         

(* wartosc_dokladna x = [x;x]        *)
let wartosc_dokladna x =

(* in_wartosc w x = x \in w *)
let in_wartosc w x = 

(* min_wartosc w = najmniejsza możliwa wartość w,   *)
(* lub neg_infinity jeśli brak dolnego ograniczenia.*)

let min_wartosc w =
	

(* max_wartosc w = największa możliwa wartość w,    *)
(* lub infinity jeśli brak górnego ograniczenia.    *)

let max_wartosc w =

(* środek przedziału od min_wartosc do max_wartosc, *)
(* lub nan jeśli min i max_wartosc nie są określone.*)
let sr_wartosc w =
	let (x,y) = (min_wartosc w, max_wartosc w) in
		match (x,y) with
			(neg_infinity,_) -> nan
			
    
(* Operacje arytmetyczne na niedokładnych wartościach. *)
let plus a b =
let minus a b =
let razy a b =
let podzielic a b =

