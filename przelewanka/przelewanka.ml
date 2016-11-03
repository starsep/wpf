(* Filip Czaplicki 359081 *)
(* Przelewanka *)

let przelewanka arr =
	if arr=[||] then 0 else
	let n = Array.length arr in 
	let capacity = Array.init n (fun i -> fst arr.(i)) in
	let result = Array.init n (fun i -> snd arr.(i)) in
	let hash = Hashtbl.create 0 in
	let queue = Queue.create () in
	let res = ref max_int in
	let rec gcd a b =
		if b=0 then a else gcd b (a mod b) in
	(* check sprawdza warunki brzegowe tj.
		w flag będzie czy nwd się zgadza
		w flag2 będzie prawda tylko wtedy kiedy wszystkie są
			puste (poza tymi, które mają 0 objętość) lub niepełne *)
	let check =
		if capacity=result then true else
		let k = Array.fold_left (fun a e -> gcd a e) 0 capacity in
		let flag = ref true in
		let flag2 = ref true in
		begin
			Array.iter (fun e -> flag := !flag &&
						((gcd k e)=k || (gcd k e)=0)) result;
			Array.iteri (fun i e -> flag2 := !flag2 &&
						((e=0 && capacity.(i)=0) || (e>0 && e<capacity.(i)))) result;
			!flag && (not !flag2)
		end
	in
	(* dodaje stan tab z wynikiem x *)
	let add_el tab x =
		Queue.push (Array.copy tab) queue;
		Hashtbl.add hash (Array.copy tab) x
	in
	(* kończy sprawdzanie z wynikiem x *)
	let end_search x =
		Queue.clear queue;
		res := min !res x;
	in
	(* sprawdza kandydata u z wynikiem dist *)
	let check_next u dist =
		let tab = Array.copy u in
		if tab = result then 
			end_search dist
		else if not (Hashtbl.mem hash tab) then
			add_el tab dist
	in
	let bfs () =
		let zeroes = Array.make n 0 in
		check_next zeroes 0;
		while not (Queue.is_empty queue) do
			let u = Queue.take queue in
			let dist = (Hashtbl.find hash (Array.copy u)) + 1 in
			(* wylejmy, z któregoś *)
			for i=0 to (n-1) do
				if u.(i) <> 0 then
					begin
						let x = u.(i) in
						u.(i) <- 0;
						check_next u dist;
						u.(i) <- x
					end
			done;
			(* dolejmy, do któregoś do pełna *)
			for i=0 to (n-1) do
				if u.(i) <> capacity.(i) then
					begin
						let x = u.(i) in
						u.(i) <- capacity.(i);
						check_next u dist;
						u.(i) <- x
					end
			done;
			(* przelejmy, z któregoś do któregoś (z i do j) *)
			for i=0 to (n-1) do
			begin
				for j=0 to (n-1) do
				begin
					if i <> j && (u.(i) <> 0) && (u.(j) <> capacity.(j)) then
					begin
						let x = u.(i) in
						let y = u.(j) in
						begin
							if u.(i) + u.(j) <= capacity.(j) then
								begin
									u.(j) <- u.(i) + u.(j);
									u.(i) <- 0
								end
							else
								begin
									u.(i) <- u.(i) - (capacity.(j) - u.(j));
									u.(j) <- capacity.(j)
								end
						end;
						check_next u dist;
						u.(i) <- x;
						u.(j) <- y;
					end
				end
				done;
			end
			done;
		done;
	in
	if not check then -1 else
	begin
		bfs ();
		!res
	end
