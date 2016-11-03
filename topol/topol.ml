(* Filip Czaplicki 359081 *)
(* Sortowanie topologiczne *)

exception Cykliczne

(* n = liczba wierzchołków *)
let n = ref 0;;

(*
set = dostaje wskaźnik do mapy, element oraz wartość:
	jeżeli na mapie był dany element
		to jego stopień zwiększamy o (value)
		wpp. jego stopień będzie wynosił (value)
*)
let set map el value =
	if PMap.exists el !map then
		map := PMap.add el ((PMap.find el !map) + value) !map
	else
		map := PMap.add el value !map
(*
make_maps = tworzy odpowiednie mapy 
	in_degree_map = wskaźnik do mapy, która przechowuje 
		dla danego elementu jego stopień wejściowy
	adjacent_vertices_map = wskaźnik do mapy, która przechowuje
		dla danego elementu listę jego następników
	add_in_degrees = dodaje do in_degree_map
		(dla każdego elementu na liście inkrementuje jego stopień)
	add_adjacent = dodaje do adjacent_vertices
	add_empty_adjacent = dodaje puste listy sąsiadów
	foo = wywołuje odpowiednie funkcje, aby zbudować mapy
zwracamy wskaźniki do stworzonych map
*)		 
let make_maps l =
	let in_degree_map = ref (PMap.create compare) in
	let adjacent_vertices_map = ref (PMap.create compare) in
	let add_in_degrees lst =
		List.iter (fun el -> set in_degree_map el 1) lst in
	let add_adjacent u vs =
		(*Printf.printf "ASD: %d\n" u;
		assert (not (PMap.mem u !adjacent_vertices_map));*)
		adjacent_vertices_map := PMap.add u vs !adjacent_vertices_map(*;
		List.iter (Printf.printf "%d ") (PMap.find u !adjacent_vertices_map);
		Printf.printf "\n"*) in
	let add_empty_adjacent vs =
		let f el =
			if not (PMap.mem el !adjacent_vertices_map) then
				incr n;
				adjacent_vertices_map := 
					PMap.add el [] !adjacent_vertices_map in 
		List.iter f vs in
	let foo (u, vs) =
		add_in_degrees vs;
		add_adjacent u vs in
	List.iter foo l;
	(*Printf.printf "----------------------\n";
	List.iter (Printf.printf "%d ") (PMap.find 3 !adjacent_vertices_map);
	Printf.printf "\n----------------------\n";*)
	List.iter (fun (_, vs) -> add_empty_adjacent vs) l;
	(in_degree_map, adjacent_vertices_map)

(*
topol = główna funkcja
	in_degree_map, adjacent_vertices_map = jak w make_maps.
	decrement_degrees = dekrementuje stopnie wierzchołków z listy,
		jeżeli jakiś wierzchołek ma teraz stopień 0 to dodaje do queue
	result = wskaźnik do listy wynikowej
	queue = kolejka wierzkołków o stopniu wejściowym 0
	init_queue = inicjalizuje początkową kolejkę
*)
let topol l =
	(*let () = Printf.printf "123123123\n" in*)
	let () = n := (List.length l) in
	let (in_degree_map, adjacent_vertices_map) = make_maps l in
	(*let () = List.iter (Printf.printf "LOL%d ") (PMap.find 3 !adjacent_vertices_map) in*)
	let queue = Queue.create () in
	let decrement_degrees lst =
		let f el =
			set in_degree_map el (-1);
			if (PMap.find el !in_degree_map) = 0 then
				Queue.push el queue in
		List.iter f lst in
	let result = ref [] in
	let init_queue () =
		let f (u, _) =
			if not (PMap.exists u !in_degree_map) then
				Queue.push u queue in
		List.iter f l in
	(*let () = List.iter (Printf.printf "LOL2%d ") (PMap.find 3 !adjacent_vertices_map) in*) 
	(*Printf.printf "----------------------\n";
	List.iter (Printf.printf "%d ") (PMap.find 3 !adjacent_vertices_map);
	Printf.printf "\n----------------------\n";*)
	init_queue ();
	while not (Queue.is_empty queue) do
		let el = Queue.take queue in
		result := el :: (!result);
		decrement_degrees (PMap.find el !adjacent_vertices_map);
		(*Printf.printf "%d\n" el;
		Printf.printf "QNeighbours:\n";
		List.iter (Printf.printf "LOL %d ") (PMap.find 3 !adjacent_vertices_map);
		Printf.printf "\n"*)
	done;
	(*if (List.length !result) = !n then*)
		List.rev (!result)
	(*else
		raise Cykliczne*)
