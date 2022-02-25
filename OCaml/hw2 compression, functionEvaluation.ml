(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* Q1 *)
(* TODO: Write a good set of tests for compress *)
let compress_tests = [ 
  ([], []);
  ([A;A;A;A;G;G;A;T;T;T;C;T;C], [(4,A); (2, G); (1, A); (3, T); (1, C); (1, T); (1, C)]);
  ([A], [(1, A)]);
  ([T], [(1, T)]);
  ([C], [(1, C)]);
  ([G], [(1, G)])
]

(* TODO: Implement compress. *)

let rec compress_help (l : nucleobase list) (base : nucleobase) (n : int) (acc :(int * nucleobase) list) : (int * nucleobase) list =  
  match l with 
  | [] -> acc @ [(n,base)] 
  | x::t -> if(x=base) then compress_help t base (n+1) acc
      else compress_help t x 1 (acc @ [(n,base)])
        
let compress (l : nucleobase list) : (int * nucleobase) list = match l with 
  | [] -> []
  | x::t  -> compress_help t x 1 []       
               
  
(* TODO: Write a good set of tests for decompress *)
let decompress_tests = [ 
  ([], []);
  ([(4,A); (2, G); (1, A); (3, T); (1, C); (1, T); (1, C)], [A;A;A;A;G;G;A;T;T;T;C;T;C]);
  ([(1, A)], [A]);
  ([(1, T)], [T]);
  ([(1, C)], [C]);
  ([(1, G)], [G]);
  ([(0, G)], [])
]

(* TODO: Implement decompress. *)
let rec decompress (l : (int * nucleobase) list) : nucleobase list = match l with 
  | [] -> []
  | (x,y)::t  -> if(x <= 0) then decompress t 
      else y::(decompress (((x-1),y)::t))


(* Q2 *)
(* TODO: Write a good set of tests for eval *)
let eval_tests = [
  ((MULT (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0)), 27.5);
  (FLOAT 4.0, 4.0);
  (MULT (FLOAT 4.0, FLOAT 4.0), 16.0);
  (MULT (FLOAT 4.0, FLOAT 0.0), 0.0);
  (PLUS (FLOAT 4.0, FLOAT 4.0), 8.0);
  (MINUS (FLOAT 4.0, FLOAT 4.0), 0.0);
  (DIV (FLOAT 4.0, FLOAT 4.0), 1.0);
  (DIV (FLOAT 4.0, FLOAT 0.0), infinity);
  (DIV (FLOAT 0.0, FLOAT 0.0), nan);
  (SIN (FLOAT 0.0), 0.0);
  (COS (FLOAT 0.0), 1.0);
  (EXP (FLOAT 2.0), 7.38905609893);
  (EXP (FLOAT 0.0), 1.0)
]

(* TODO: Implement eval. *)
let rec eval e = match e with 
  | PLUS  (x,y) -> (eval x) +. (eval y) 
  | MINUS (x,y) -> (eval x) -. (eval y)
  | MULT   (x,y) -> (eval x) *. (eval y)
  | DIV   (x,y) -> (eval x) /. (eval y)
  | SIN   x   ->  sin(eval x) 
  | COS   x   -> cos(eval x)  
  | EXP   x  -> 2.7182818284590452353602874713527 ** (eval x)
  | FLOAT x -> x
  

(* TODO: Write a good set of tests for to_instr *)
let to_instr_tests = [
  ((MULT (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0)), [Float 2.2; Float 3.3; Plus; Float 5.; Mult]);
  (FLOAT 4.0, [Float 4.0;]);
  (MULT (FLOAT 4.0, FLOAT 1.0), [Float 4.0; Float 1.0; Mult;]); 
  (PLUS (FLOAT 4.0, FLOAT 1.0), [Float 4.0; Float 1.0; Plus;]);
  (MINUS (FLOAT 4.0, FLOAT 1.0), [Float 4.0; Float 1.0; Minus;]);
  (DIV (FLOAT 4.0, FLOAT 1.0), [Float 4.0; Float 1.0; Div;]); 
  (SIN (FLOAT 0.0), [Float 0.0; Sin;]);
  (COS (FLOAT 0.0), [Float 0.0; Cos;]);
  (EXP (FLOAT 2.0), [Float 2.0; Exp]); 
]

(* TODO: Implement to_instr. *)
let rec to_instr e = match e with 
  | PLUS  (x,y) -> (to_instr x) @ (to_instr y) @ [Plus]
  | MINUS (x,y) -> (to_instr x) @ (to_instr y) @ [Minus]
  | MULT   (x,y) -> (to_instr x) @ (to_instr y) @ [Mult]
  | DIV   (x,y) -> (to_instr x) @ (to_instr y) @ [Div]
  | SIN   x   ->  (to_instr x) @ [Sin] 
  | COS   x   -> (to_instr x) @ [Cos]   
  | EXP   x  -> (to_instr x) @ [Exp] 
  | FLOAT x -> [Float x]


(* TODO: Write a good set of tests for instr *)
let instr_tests = [
  ((Mult, [1.0]), None);
  ((Div, [1.0]), None);
  ((Plus, [1.0]), None);
  ((Minus, [1.0]), None);
  ((Exp, []), None);
  ((Sin, []), None);
  ((Cos, []), None);
  ((Mult, [2.0; 2.0;3.0]), Some [4.0;3.0]);
  ((Div, [2.0; 4.0;3.0]), Some [2.0;3.0]);
  ((Plus, [2.0; 5.0;3.0]), Some [7.0;3.0]);
  ((Minus, [2.0; 1.0; 3.0]), Some [-1.0 ;3.0]);
  ((Sin, [0.0; 2.0;3.0]), Some [0.0;2.0;3.0]);
  ((Cos, [0.0; 2.0;3.0]), Some [1.0;2.0;3.0]);
  ((Exp, [0.0; 2.0;3.0]), Some [1.0;2.0;3.0]);
  (((Float 4.2), [0.0; 2.0;3.0]), Some [4.2;0.0;2.0;3.0]);
]


(* TODO: Implement to_instr. *)               
let instr i s = 
  match i with 
  | Plus -> 
      (match s with 
       | [] -> None
       | [h] -> None 
       | x::y::t -> Some ((x +. y)::t) ;
      )
                                        
  | Minus -> (match s with 
      | [] -> None
      | [h] -> None 
      | x::y::t -> Some ((y -. x)::t) ;
    )
  | Mult -> (match s with 
      | [] -> None 
      | [h] -> None 
      | x::y::t -> Some ((y *. x)::t) ;
    )
  | Div -> (match s with 
      | [] -> None 
      | [h] -> None 
      | x::y::t -> Some ((y /. x)::t) ;
    )
  | Sin ->  (match s with 
      | [] -> None 
      | x::t -> Some ((sin(x))::t) ;
    )
  | Cos  -> (match s with 
      | [] -> None 
      | x::t -> Some ((cos(x))::t) ;
    )
  | Exp -> (match s with 
      | [] -> None 
      | x::t -> Some ((2.7182818284590452353602874713527 ** x)::t) ;
    )
  | Float x -> Some ((x)::s)


(* TODO: Write a good set of tests for prog *)
let prog_tests = [
  ([Float 2.2; Float 3.3; Plus; Float 5.; Mult], Some 27.5);
  ([Float 2.2; Float 3.3; Plus; Mult], None);
  ([], None);
  ([Float 2.2; Float 3.3; Plus;], Some 5.5);
  ([Float 2.2; Float 3.3; Mult;],  Some 7.26);
  ([Float 3.3; Float 2.2; Minus;], Some 1.1);
  ([Float 2.2; Float 2.; Div;], Some 1.1);
  ([Float 0.0; Sin;], Some 0.0);
  ([Float 0.0; Cos], Some 1.0);
  ([Float 0.0; Exp], Some 1.0);
  ([Float 1.0], Some 1.0)
]


      

(* TODO: Implement prog. *)
let prog instrs = (* if float then add to stack, if intruction call instr using the stack and the instruction. Move down the list. if ever instr returns None, then return none, else continue*)
  
  let rec prog_help i s = match i with 
    |[] -> 
        (
          match s with
          |[] -> None
          |[x] -> Some x
          |x::y::t -> None
        )
      
    |x::t -> 
        (
          let z = instr x s in 
          match z with
          |None ->  None  
          |Some f -> prog_help t f
        )
  in 
  prog_help instrs []
                  
    
