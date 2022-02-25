(* TODO: Write a good set of tests for unused_vars. *)
let unused_vars_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (Let ("x", I 1, I 5), ["x"]);
  (Fn ([("x", Int); ("y", Int)],
       Primop (Plus, [Var "y"; I 1])), ["x"]);
  (Fn ([("x", Int); ("y", Int)],
       Primop (Plus, [Var "y"; Var "x"])), []);
  (Fn ([("x", Int)],
       I 1), ["x"]);
  (Apply (Fn ([("x", Int)],
              I 1), [Let ("y", I 1, I 5); I 2]),["x"; "y"]);
  (Rec ("fact", Arrow ([Int], Int), I 1), ["fact"]);
  (Rec ("fact",
        Arrow ([Int], Int),
        Fn ([("n", Int)],
            If (Primop (Equals, [Var "n"; I 0]),
                I 1,
                Primop (Times, [Var "n"; Apply (Var "fact", [Primop (Minus, [Var "n"; I 1])])])))), [])
]

(* TODO: Implement the missing cases of unused_vars. *)
let rec unused_vars =
  function
  | Var _ | I _ | B _ -> []
  | If (e, e1, e2) -> unused_vars e @ unused_vars e1 @ unused_vars e2
  | Primop (_, args) ->
      List.fold_left (fun acc exp -> acc @ unused_vars exp) [] args
  | Let (x, e1, e2) ->
      let unused = unused_vars e1 @ unused_vars e2 in
      if List.mem x (free_variables e2) then
        unused
      else
        x :: unused

  | Rec (x, _, e) -> if List.mem x (free_variables e) then
        unused_vars e
      else
        x :: unused_vars e

  | Fn (xs, e) -> let unused = unused_vars e in
      let free = free_variables e in
      
      List.fold_left (fun acc (n,t) -> if List.mem n (free) then acc
                       else n :: acc) unused xs
      

  | Apply (e, es) -> List.fold_left (fun acc x -> unused_vars x @ acc) (unused_vars e) es 

(* TODO: Write a good set of tests for subst. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let subst_tests : (((exp * name) * exp) * exp) list = [
  (* An example test case. If you have trouble writing test cases of the
     proper form, you can try copying this one and modifying it.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (((I 1, "x"), (* [1/x] *)
    (* let y = 2 in y + x *)
    Let ("y", I 2, Primop (Plus, [Var "y"; Var "x"]))),
   (* let y = 2 in y + 1 *)
   Let ("y", I 2, Primop (Plus, [Var "y"; I 1])));
  
  (*
    (((I 1, "x"), Fn ([("x", Int); ("y", Int)],
                      Primop (Plus, [Var "y"; I 1]))), 
     Fn ([("x", Int); ("y", Int)],
         Primop (Plus, [Var "y"; I 1]))); *)
  (((Var "fact", "y"), Rec ("fact", Arrow ([Int], Int), Var "y")), 
   Rec ("a", Arrow ([Int], Int), 
        Var "fact"));
  
  (((I 2, "fact"), Rec ("fact", Arrow ([Int], Int), Fn ([("n", Int)], If (
       Primop (LessThan, [Var "n"; I 0]),
       I 1,
       Apply (Var "fact", [Primop (Minus, [Var "n"; I 1])]))))),
   Rec ("fact", Arrow ([Int], Int), Fn ([("n", Int)], If (
       Primop (LessThan, [Var "n"; I 0]),
       I 1,
       Apply (Var "fact", [Primop (Minus, [Var "n"; I 1])])))));
   
  
  
  
  
  (((Var "x", "y"), Fn ([("x", Int)],
                        Primop (Plus, [Var "y"; Var "x"]))), 
   Fn ([("a", Int)],
       Primop (Plus, [ Var "x"; Var "a"])));
  
  (((Var "x", "y"), Fn ([("y", Int)],
                        Primop (Plus, [Var "y"; Var "x"]))), 
   Fn ([("y", Int)],
       Primop (Plus, [ Var "y"; Var "x"])));
  
  (((Var "x","y"),(Apply (Fn ([("x", Int)],
                              Primop (Plus, [Var "y"; Var "x"])), [Var "y";Var "y"]))),
   (Apply (Fn ([("a", Int)],
               Primop (Plus, [Var "x"; Var "a"])), [Var "x";Var "x"])));
]

(* TODO: Implement the missing cases of subst. *)
let rec subst ((e', x) as s) exp =
  match exp with
  | Var y ->
      if x = y then e'
      else Var y
  | I n -> I n
  | B b -> B b
  | Primop (po, args) -> Primop (po, List.map (subst s) args)
  | If (e, e1, e2) ->
      If (subst s e, subst s e1, subst s e2)
  | Let (y, e1, e2) ->
      let e1' = subst s e1 in
      if y = x then
        Let (y, e1', e2)
      else
        let (y, e2) =
          if List.mem y (free_variables e') then
            rename y e2
          else
            (y, e2)
        in
        Let (y, e1', subst s e2)

  | Rec (y, t, e) -> 
      if y = x then Rec(y, t, e)
      else 
        let (y, e) = 
          if List.mem y (free_variables e') then 
            rename y e
          else
            (y, e)
        in 
        Rec (y, t, subst s e)

  | Fn (xs, e) -> let varlist = List.map (fun (name, _) -> name) xs in
      
      if List.mem x varlist then Fn (xs, e)
      else 
        let (xs, e) =
          let free = free_variables e' in 
          List.fold_left 
            (fun (acc, e) (i,t) ->
               let (y,e) = if List.mem i free then
                   rename i e
                 else (i,e)
               in 
               ((y,t)::(acc) , e))
              
            ([],e) xs 
        in
        Fn (List.rev(xs), subst (e', x) e)

  | Apply (e, es) -> Apply(subst (s) e, (List.map (subst(s)) es)) 

and rename x e =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and rename_all names exp =
  List.fold_right
    (fun name (names, exp) ->
       let (name', exp') = rename name exp in
       (name' :: names, exp'))
    names
    ([], exp)

(* Applying a list of substitutions to an expression, leftmost first *)
let subst_list subs exp =
  List.fold_left (fun exp sub -> subst sub exp) exp subs


(* TODO: Write a good set of tests for eval. *)
let eval_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec and Apply!
  *)
  (Let ("x", I 1, Primop (Plus, [Var "x"; I 5])), I 6);
  
  ((Apply (Fn ([("a", Int)],
               Primop (Plus, [I 3; Var "a"])), [I 2;])), I 5);
  ((Apply (Fn ([("a", Int); ("b", Int)],
               Primop (Minus, [Var "b"; Var "a"])), [I 2; I 3])), I (1));
  ((Apply (Fn ([],
               Primop (Minus, [I 3; I 1])), [])), I 2);
  
  ((Apply ((Rec ("fact", Arrow ([Int], Int), Fn ([("n", Int)], If (
       Primop (LessThan, [Var "n"; I 0]),
       I 1,
       Apply (Var "fact", [Primop (Minus, [Var "n"; I 1])]))))), [I 1])), I 1);
  
  
  
  (Rec ("fact", Arrow ([Int], Int), Fn ([("n", Int)], If (
       Primop (LessThan, [Var "n"; I 0]),
       I 1,
       Apply (Var "fact", [Primop (Minus, [Var "n"; I 1])])))), Fn
     ([("n", Int)],
      If (Primop (LessThan, [Var "n"; I 0]), I 1,
          Apply
            (Rec
               ("fact", Arrow ([Int], Int),
                Fn
                  ([("n", Int)],
                   If (Primop (LessThan, [Var "n"; I 0]), I 1,
                       Apply (Var "fact", [Primop (Minus, [Var "n"; I 1])])))),
             [Primop (Minus, [Var "n"; I 1])]))));
  
  
]

(* TODO: Implement the missing cases of eval. *)
let rec eval exp =
  match exp with
  (* Values evaluate to themselves *)
  | I _ -> exp
  | B _ -> exp
  | Fn _ -> exp

  (* This evaluator is _not_ environment-based. Variables should never
     appear during evaluation since they should be substituted away when
     eliminating binding constructs, e.g. function applications and lets.
     Therefore, if we encounter a variable, we raise an error.
*)
  | Var x -> raise (Stuck (Free_variable x))

  (* Primitive operations: +, -, *, <, = *)
  | Primop (po, args) ->
      let args = List.map eval args in
      begin
        match eval_op po args with
        | None -> raise (Stuck Bad_primop_args)
        | Some v -> v
      end

  | If (e, e1, e2) ->
      begin
        match eval e with
        | B true -> eval e1
        | B false -> eval e2
        | _ -> raise (Stuck If_non_true_false)
      end

  | Let (x, e1, e2) ->
      let e1 = eval e1 in
      eval (subst (e1, x) e2)

  | Rec (f, _, e) -> eval (subst (exp, f) e) 

  | Apply (e, es) -> match eval e with
    | Fn (xs, e) -> 
        if (List.length xs != List.length es) then
          raise (Stuck Arity_mismatch)
        else
          eval(subst_list (List.combine (List.map (eval) es) (List.map (fun (name, _) -> name)xs)) e)
    | _ -> raise (Stuck Apply_non_fn)

(* TODO: Write a good set of tests for infer. *)
let infer_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (([("x", Int)], Var "x"), Int);
  
  (([],(Rec ("fact", Arrow ([Int], Int), Fn ([("n", Int)], If (
       Primop (LessThan, [Var "n"; I 0]),
       I 1,
       Apply (Var "fact", [Primop (Minus, [Var "n"; I 1])])))))),Arrow([Int],Int));
  
  
  (([],Fn ([("x", Int)],
           Primop (Plus, [ Var "x"; I 3]))) , Arrow([Int],Int));
  (([],Fn ([("x", Int); ("y", Int)],
           Primop (Plus, [ Var "x"; Var "y"]))) , Arrow([Int; Int],Int));
  (([],Fn ([],
           Primop (Plus, [ I 1; I 3]))) , Arrow([],Int));
  
  (([], (Apply (Fn ([("x", Int)],
                    Primop (Plus, [ Var "x"; I 3])), [I 1]))), Int);
  (([], (Apply (Fn ([],
                    Primop (Plus, [ I 1; I 3])), []))), Int);
  (([], (Apply (Fn ([("x", Int); ("y", Int)],
                    Primop (Plus, [ Var "x"; Var "y"])), [I 1; I 2]))), Int);
]

(* TODO: Implement the missing cases of infer. *)
let rec infer ctx e =
  match e with
  | Var x ->
      begin
        try lookup x ctx
        with Not_found -> raise (TypeError (Free_variable x))
      end
  | I _ -> Int
  | B _ -> Bool

  | Primop (po, exps) ->
      let (domain, range) = primopType po in
      check ctx exps domain range

  | If (e, e1, e2) ->
      begin
        match infer ctx e with
        | Bool ->
            let t1 = infer ctx e1 in
            let t2 = infer ctx e2 in
            if t1 = t2 then t1
            else type_mismatch t1 t2
        | t -> type_mismatch Bool t
      end

  | Let (x, e1, e2) ->
      let t1 = infer ctx e1 in
      infer (extend ctx (x, t1)) e2

  | Rec (f, t, e) -> 
      let t' = (infer (extend ctx (f,t)) e) in
      if t = t' then t
      else type_mismatch t t'

  | Fn (xs, e) -> Arrow(List.map (fun (_,t) -> t) xs,infer (extend_list ctx xs) e)

  | Apply (e, es) -> 
      match infer ctx e with 
      |Arrow (xs, x) ->
          let t1 = List.map (infer ctx) es in
          if List.length(xs) != List.length(t1) then 
            raise (TypeError Arity_mismatch)
          else
            (List.map2 (fun x y -> 
                 if(x!=y) then (type_mismatch x y)
               ) 
                xs t1); x 
      |t' -> raise (TypeError (Apply_non_arrow t'))
      

and check ctx exps tps result =
  match exps, tps with
  | [], [] -> result
  | e :: es, t :: ts ->
      let t' = infer ctx e in
      if t = t' then check ctx es ts result
      else type_mismatch t t'
  | _ -> raise (TypeError Arity_mismatch)

(* TODO: Implement type unification. *)
let rec unify (t1 : utp) (t2 : utp) : unit =
  match t1, t2 with
  (* unifying identical concrete types does nothing *)
  | UInt, UInt
  | UBool, UBool -> ()
  (* For type constructors, recursively unify the parts *)
  | UArrow (t1, t1'), UArrow (t2, t2') ->
      (unify t1 t2);
      (unify t1'  t2')
  | UTVar a, _ -> unifyVar a t2
  | _, UTVar b -> unifyVar b t1
  (* All other cases are mismatched types. *)
  | _, _ -> unif_error @@ UnifMismatch (t1, t2)

(* Unify a variable with a type *)
and unifyVar a t =
  match !a with 
  | Some t'-> unify t' t 
  | None -> 
      match t with 
      |UTVar b -> 
          (match !b with 
           |Some i -> unifyVar a i 
           |None -> 
               if (a==b) then ()
               else a:= Some (UTVar b)
          )
      |_ -> if(occurs a t) then unif_error (UnifOccursCheckFails) 
          else a := Some t
      
      
      
      
      

      
