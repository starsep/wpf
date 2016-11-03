type 'a queue = Null | Node of 'a * 'a queue * 'a queue * int

let empty = Null

let height q =
	match q with
	| Null -> -1
	| Node (_, _, _, h) -> h

let rec join q1 q2 = 
	match (q1, q2) with
	| (Null, _) -> q2
	| (_, Null) -> q1
	| ( Node (v1, left, right, _), Node (v2, _, _, _) ) ->
		if v1 < v2 then
			join q2 q1
		else
			let q3 = join right q2 in
			if (height left) < (height q3) then 
				Node (v1, q3, left, (height left) + 1)
			else
				Node (v1, left, q3, (height q3) + 1)

let add x q = join ( Node(x, Null, Null, 0) ) q

exception Empty
 
let delete_min q =
	match q with
	| Null -> raise Empty
	| Node (v, left, right, _) -> (v, join left right)
	
let is_empty q = q = empty

