(* Question 1 *)
(* TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
         Remember that you should NOT test cases for n < 0.
*)
(* TODO: Correct these tests for the fact function. *)
let fact_tests = [
  (0, 1.);
  (1, 1.);
  (2, 2.);
  (3, 6.);
  (5, 120.)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int): float =
  if n < 0 then domain()
  else
    match n with
    | 0 -> 1.
    | _ -> float_of_int(n) *. fact(n - 1);;


(* TODO: Write your own tests for the binomial function.
         See the provided tests for fact, above, for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct the incorrect test cases for the function. *)
  ((0, 0), 1.);
  ((1, 0), 1.);
  ((2, 0), 1.);
  ((2, 1), 2.);
  ((10, 1), 10.);
  ((10, 10), 1.);
  ((10, 2), 45.)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let binomial (n: int) (k: int) =
  if n < 0
  then domain ()
  else if k < 0
  then domain ()
      
  else (if k > n
        then domain ()
        else fact (n) /. (fact k *. fact (n - k))
       )


(* TODO: Write a good set of tests for ackerman. *)
let ackerman_tests = [
  ((0,0), 1);
  ((0,1), 2);
  ((0,2), 3);
  ((1,0), 2);
  ((2,0), 3);
  ((1,1), 3)
  (* Your test cases go here *)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let ackerman (n, k)  =
  if n < 0 || k < 0
  then domain ()
  else (let rec ack n k =
          match (n, k) with
          | (0, _) -> k + 1
          | (_, 0) -> ack (n-1) 1
          | (_, _) -> ack (n - 1) (ack n (k - 1))
        in ack n k)


(* Question 2: is_prime *)

(* TODO: Write a good set of tests for is_prime. *)
let is_prime_tests = [
  (2, true);
  (3, true);
  (4, false);
  (63, false);
  (2719, true);
  (53, true);
  (52, false)
(* Your tests go here *)

]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let is_prime n =
  let rec p n x=
    if x * x <= n then 
      if n mod x = 0 then
        false
      else
        p n (x+1)
    else true      
  in
  if n <= 1 then domain() 
  else p n 2
  
      
          
       


(* Question 3: Newton-Raphson method for computing the square root
*)

let square_root_tests = [
  (4., 2.);
  (1., 1.);
  (0.25, 0.5);
  (2., 1.4142135623730950488016887)
]

let square_root a =
  let rec findroot x acc =
    let x' = ((a /. x) +. x) /. 2. in
    
    if abs_float(x' -. x)< acc then x'
    else findroot x' acc

  in
  if a > 0.
  then findroot 1. epsilon_float
  else domain ()


(* Question 4: Fibonacci*)

(* TODO: Write a good set of tests for fib_tl. *)
let fib_tl_tests = [
  (0, 1);
  (1, 1);
  (2, 2);
  (3, 3);
  (4, 5);
  (5, 8);
  (6, 13);
  (20, 10946)
]

(* TODO: Implement a tail-recursive helper fib_aux. *)
let rec fib_aux n a b = (* a acts as fib(n-2) and b acts as fib(n-1) when n=1 it means the element fib(n) has been reached. its build from the very start of index fib(0) *)
  if n = 1 then b
  else fib_aux (n-1) b (a+b)

(* TODO: Implement fib_tl using fib_aux. *)
let fib_tl n =
  if n<0 then domain()
  else
  if n =0 then 1 
  else fib_aux n 1 1
      
