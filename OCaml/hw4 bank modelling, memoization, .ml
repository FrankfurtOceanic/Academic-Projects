(* ------------------------------------------------------------------------*)
(* Q 1 : Money in the bank (25 points)                                     *)
(* ------------------------------------------------------------------------*)

let new_account (p: passwd) : bank_account =
  
  
  let pw = ref p in
  let a = ref 0 in
  let c = ref 0 in
  {update_passwd  = 
     (fun oldp newp -> if(oldp = !pw) then (pw := newp; c := 0;)
       else (c := !c + 1; 
             raise wrong_pass)
     );

   retrieve = 
     (fun pwd amt -> if(!c > 2) then raise too_many_attempts
       else 
       if(pwd = !pw) then 
       
         if(amt <= !a) then 
           (a := !a - amt;
            c :=0;)
         else raise no_money
           
       else (c := !c + 1; 
             raise wrong_pass)
     );
                 
   deposit =
     (fun pwd amt -> if(!c > 2) then raise too_many_attempts
       else 
       if(pwd = !pw) then 
         (a := !a + amt;
          c :=0;)               
           
       else (c := !c + 1; 
             raise wrong_pass);
     );

   print_balance = 
     (fun pwd -> if(!c > 2) then raise too_many_attempts
       else 
       if(pwd = !pw) then 
         (c := 0;
          !a;)
       else (c := !c + 1; 
             raise wrong_pass)
     )
  }
;;


(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)

(* Q 2.1 : Counting how many function calls are made *)

let rec fib_I (n: int) : fib_result =
  let nr = ref 0 
  in 
  let rec fib x = (
    nr := !nr +1;
    if x = 0 then 0
    else (if x = 1 then 1 else fib (x-2) + fib (x-1));)
  in 
  let res = fib n;
  in
  {num_rec = !nr; result = res}
      
    
         
;;


(* Q 2.2 : Memoization with a global store *)

let fib_memo (n: int) : int = 
  let rec fib n =
    
    let x = (Hashtbl.find_opt store n) in
    match x with
    | None -> 
        if n > 1 then (let y = fib (n-1) + fib (n-2) in
                       Hashtbl.add store n y;
                       y;)
        else (Hashtbl.add store n n;
              n); 
            
    | Some a -> a; 
      
  in
  fib n
;;


(* Q 2.3 : General memoization function *)

let memo (f: (('a -> 'b) -> 'a -> 'b)) (stats: stats) : ('a -> 'b) =
  let storem  = ref (Hashtbl.create 1000) in 
  let rec g x = (
    let z = (Hashtbl.find_opt !storem x) in
    match z with
    |Some w -> (stats.lkp := !(stats.lkp) + 1;
                w);
        
    | None -> (stats.entries := !(stats.entries) + 1; 
               let y = f g x in
               Hashtbl.add !storem x y;
               y;);
  )in
  g;
;;


(* Q 2.4 : Using memo to efficiently compute the Fibonacci number *)
(* We also accept let fibM = raise NotImplemented ;; *)
let fibM =
  let z = {entries = (ref 0); lkp= (ref 0);} in
  let i = (memo (fun g x -> if x = 0 then 0
                  else (if x = 1 then 1 else g(x-2) + g(x-1))) z) in
  fun n -> (i n, z)
  
  
;;
