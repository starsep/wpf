let przelewanka arr =
	if arr=[||] then 0 else
	let n = Array.length arr in 
	let capacity = Array.init n (fun i -> fst arr.(i)) in
	let result = Array.init n (fun i -> snd arr.(i)) in
	let hash = Hashtbl.create 0 in
	let queue = Queue.create () in
	let infinity = max_int in
	let res = ref infinity in
	let rec gcd a b =
		if b=0 then a else gcd b (a mod b) in
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
	let debug u s =
		Printf.printf s;
		for i=0 to (n-1) do
			Printf.printf "%d " u.(i)
		done;
		Printf.printf "\n";
	in
	let debug_queue () =
		Printf.printf "--QUEUE BEGIN--\n";
		Queue.iter (fun x -> (debug x "")) queue;
		Printf.printf "--QUEUE END--\n";
	in
	let add_el tab x =
		(*debug t "t: ";*)
		(*Printf.printf "%d " x;
		debug tab "insert: ";*)
		(*debug_queue ();
		Printf.printf "PUSH:\n";*)
		Queue.push (Array.copy tab) queue;
		(*debug_queue ();*)
		Hashtbl.add hash (Array.copy tab) x
	in
	let end_search x =
		(*Printf.printf "END!!! :D\n";*)
		Queue.clear queue;
		res := min !res x;
		(*Printf.printf "QUEUE.CLEARED! \n";*)
	in
	let equal_tabs a b =
		let res = ref true in
		for i=0 to (n-1) do
			res := !res && (a.(i) = b.(i)) 
		done;
		!res
	in
	let check_next u dist =
		let tab = Array.copy u in
		if equal_tabs tab result then 
			end_search dist
		else if not (Hashtbl.mem hash tab) then
			add_el tab dist
	in
	let bfs () =
		let zeroes = Array.make n 0 in
		(*debug zeroes;*)
		(*debug result "result: ";
		debug capacity "capacity: ";*)
		check_next zeroes 0;
		while not (Queue.is_empty queue) do
			let u = Queue.take queue in
			let q = ref zeroes in
			let dist = (Hashtbl.find hash (Array.copy u)) + 1 in
			(*Printf.printf "---- BEGIN WHILE ----\n"; *)
			(*debug (Queue.take queue) "LOL: ";*)
			(*Printf.printf "********UUUU: %d " (dist-1);
			debug u "u: ";
			debug_queue ();*)
			(*debug capacity "capacity: ";*)
			(* wylejmy, z któregoś *)
			(*Printf.printf "--BEGIN FOR1--:\n";*)
			q := Array.copy u;
			for i=0 to (n-1) do
				if u.(i) <> 0 then
					begin
						let x = u.(i) in
						u.(i) <- 0;
						check_next u dist;
						u.(i) <- x
					end
			done;
			assert (equal_tabs !q u);
			(*Printf.printf "--END FOR1--:\n";*)
			(*Printf.printf "--BEGIN FOR2--:\n";*)
			(* dolejmy, do któregoś do pełna *)
			q := Array.copy u;
			for i=0 to (n-1) do
				if u.(i) <> capacity.(i) then
					begin
						let x = u.(i) in
						u.(i) <- capacity.(i);
						check_next u dist;
						u.(i) <- x
					end
			done;
			assert (equal_tabs !q u);
			(*Printf.printf "--END FOR2--:\n";
			Printf.printf "---- END WHILE ----\n"*)
			(* przelejmy, z któregoś do któregoś (z i do j) *)
			q := Array.copy u;
			for i=0 to (n-1) do
			begin
				for j=0 to (n-1) do
				begin
					(*debug capacity;*)	
					(*(Printf.printf "lel: %d\n" (if i <> j && (u.(i) <> 0) && (u.(j) <> capacity.(j)) then 1 else 0));*)
					if i <> j && (u.(i) <> 0) && (u.(j) <> capacity.(j)) then
					begin
						let x = u.(i) in
						let y = u.(j) in
						(*Printf.printf "%d %d\n" i j;*)
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
			assert (equal_tabs !q u);
		done;
	in
	if not check then -1 else
	begin
		bfs ();
		!res
	end
