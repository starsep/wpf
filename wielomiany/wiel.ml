type wielomian = float list

let oblicz w x =
	let rec pom l pot akk =
		match l with
		| [] -> akk
		| h :: t -> pom t (pot *. x) (akk +. (h *. pot) ) 
	in pom w 1. 0.;;
	
let suma a b =
	let rec pom l1 l2 akk =
		match (l1, l2) with
		| ([], _) -> l2 @ akk
		| (_, []) -> l1 @ akk
		| (h1 :: t1, h2 :: t2) -> pom t1 t2 ((h1 + h2)::akk)
	in List.rev(pom a b []);;

let (+) a b = suma a b;;

(* zwraca wielomian [w] * stałą [c] *)
let razyStala w c = List.map (fun x -> x *. c) w;; 

(* zwraca wielomian [w] * x^[n] *)
let razyXn w n =
	let rec nZer n akk =
		match n with
		| 0 -> []
		| _ -> nZer (n - 1) (0. :: akk)
	in nZer n w;;
	
let stopien w = List.length w;;

let iloczyn a b = 
	let rec pom w n akk =
		match w with
		| [] -> akk
		| h::t -> pom t (n - 1) (suma akk (razyXn (razyStala a h) n))
	in pom b 0 [];;

let pochodna w =
	let 
	match w with
	| [] -> []
	| h::t -> 
	
(*
let pochodna w =
let stopien w =
let calka w =
*)
