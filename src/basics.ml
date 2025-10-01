let abs x =
  if x >= 0 then x
  else (-x)

(* ================= *)
(* Part 1: Non-Recursive Functions *)
(* ================= *)

let rev_tup (a, b) = (b, a)

let rev_triple (a, b, c) = (c, b, a)

let is_odd x = x mod 2 <> 0

let is_older (y1, m1, d1) (y2, m2, d2) =
  if y1 < y2 then true
  else if y1 > y2 then false
  else if m1 < m2 then true
  else if m1 > m2 then false
  else d1 < d2

let to_us_format (y, m, d) = (m, d, y)

(* ================= *)
(* Part 2: Recursive Functions *)
(* ================= *)

let rec pow x p =
  if p = 0 then 1
  else x * pow x (p - 1)

let rec fac x =
  if x = 1 then 1
  else x * fac (x - 1)

(* ================= *)
(* Part 3: Lists *)
(* ================= *)

let rec get_nth idx lst =
  match lst with
  | [] -> failwith "Index out of bounds"
  | h :: t -> if idx = 0 then h else get_nth (idx - 1) t

let larger lst1 lst2 =
  let len1 = List.length lst1 in
  let len2 = List.length lst2 in
  if len1 > len2 then lst1
  else if len2 > len1 then lst2
  else []

let sum lst1 lst2 =
  let rec helper l =
    match l with
    | [] -> 0
    | h :: t -> h + helper t
  in
  (helper lst1) + (helper lst2)
