include Ast

module Parser = Parser
module Lexer = Lexer

let parse (s : string) : expression =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.expression_eof Lexer.token lexbuf in
     ast

let string_of_declaration = String_of.string_of_declaration

let empty = []
let singleton x y = [(x,y)] (* x is substituted in place of y *)


let rec mismatch_help elm lst = (* if there is a mismatch, return true *)
  match lst with
  | [] -> false
  | (a, b)::tl -> (match elm with 
      | (l,r) -> if b = r then (if a <> l then true else mismatch_help elm tl) else mismatch_help elm tl)

let rec mismatches x y = (* looking for cases where two enteries have the same varaible but differnt substitutions (second tuple is the same, but first tuple differs) *) (* if there is a mismatch, return true *)
  match x with 
  | [] -> false
  | hd :: tl -> if mismatch_help hd y then true else mismatches tl y

let rec find_dupe hd tl = 
  match tl with
  | [] -> false
  | (_, b) :: t -> (match hd with 
              | (_, r) -> if b = r then true else find_dupe hd t)

let rec remove_dupes lst =
  match lst with
  | [] -> lst
  | hd :: tl -> if find_dupe hd tl then remove_dupes tl else lst 

let merge s1 s2 = match (s1, s2) with
| ([], []) -> Some empty  
| ([], _) -> Some s2 (* this case is actually correct, but it shouldn't be needed once the other one is fixed *)
  | (_, []) -> Some s1 (* todo: fix *)
  | (x, y) -> if mismatches x y then None else Some (remove_dupes (x @ y))


let rec is_a_var vars x =
  match vars with 
  | [] -> false
  | hd::tl -> if x = hd then true else is_a_var tl x

let rec match_expression variables pattern expression = (* pattern is the equality, expression is expression *) (*just returns list of singletons which hold mappings *)
  match pattern with
    | Identifier x -> if (is_a_var variables x) then (* *)
        (* if x is a variable: *)
        Some (singleton x expression)
        else
        (* if x is a constant: *)
        (if pattern = expression then Some empty else None)
    | Application (p1, p2) -> (* x has to be an application too, otherwise return None *)
        (match expression with
            | Application (e1, e2) ->
                (* We recursively match the sub-expressions.
                    This part is much easier to write if e2 is an expression (and it is for this particular ast),
                    because it's so symmetrical *)
                (match match_expression variables p1 e1, match_expression variables p2 e2 with
                | Some s1, Some s2 -> merge s2 s1
                | _ -> None) (* p1 didn't match e1 and/or p2 didn't match e2 *)
            | _ -> None) (* tries to match an application to an identifier, which obviously can't be equal *)
    | _ -> None (* not sure how you'd even hit this *)


let rec find x s = match s with
  | [] -> failwith "Not found (find was passed an empty list)"
  | (k,v) :: tl -> if x = k then v else find x tl 


let rec substitute variables s e = match e with (* variables holds the original varaibles *)
    | Identifier x -> if (is_a_var variables x) then (find x s) else (Identifier x)
    | Application (e1, e2) -> Application (substitute variables s e1, substitute variables s e2)
    | _ -> e

    
let rec attempt_rewrite variables lhs rhs expression = (* lhs and rhs of equality *)
  let rec attempt_help vars = match vars with
  | [] -> []
  | TypedVariable(nm, _) :: tl -> nm :: (attempt_help tl)  in 
  
  match match_expression (attempt_help variables) lhs expression with
    | Some s -> Some (substitute (attempt_help variables) s rhs) (* if find lhs in expression, substitute with rhs *)
    | None -> (match expression with
            | Application (e1, e2) -> (match attempt_rewrite variables lhs rhs e2 with (* if don't find lhs in expression, recurse through expression *)
              | None -> (match attempt_rewrite variables lhs rhs e1 with (* todo: try the other side too! i.e. side e1? *)
                        | None -> None
                        | Some e1' -> Some (Application (e1', e2)))

              | Some e2' -> Some (Application (e1, e2')))
            | _ -> None (* not succesful *)
        )

   (*
let rec perform_step rules expression = match rules with
  | (variables, nm, lhs, rhs) :: rest -> (match attempt_rewrite variables lhs rhs expression with
      | Some e -> Some (nm, e)
      | None -> perform_step rest expression)
  | _ -> None

let rec perform_steps rules expression = match perform_step rules expression with
  | Some (nm, e) -> (nm, e) :: perform_steps [] e
  | None -> [] *)
  



  let perform_step rule expression = match rule with
  | (variables, nm, lhs, rhs) -> (match attempt_rewrite variables lhs rhs expression with
      | Some e -> Some (nm, e)
      | None -> None )

let rec perform_steps rules full_rules expression = match rules with 
  | (variables, nm, lhs, rhs) :: rest -> (match perform_step (variables, nm, lhs, rhs) expression with 
                  | None -> perform_steps rest full_rules expression 
                  | Some (nm, e) -> (nm, e) :: perform_steps full_rules full_rules e (* expression was changed, so allow re-application of rules *)
            )
  | [] -> [] (* out of rules to apply *)

  
let rec match_right rules lhs rhs = 
  String_of.string_of_expression rhs :: 
  (match perform_steps rules rules rhs with
  | (nm, e) :: _ -> (" = { " ^ nm ^ " }") :: match_right rules lhs e
  | [] -> if lhs = rhs then [] else " = { ??? }" :: [String_of.string_of_expression rhs])



  let rec prove rules lhs rhs
  = String_of.string_of_expression lhs :: 
    (match perform_steps rules rules lhs with
     | (nm, e) :: _ -> (" = { " ^ nm ^ " }") :: prove rules e rhs
     | [] -> if lhs = rhs then [] else (match (List.rev (match_right rules lhs rhs)) with 
            | hd :: tl -> if hd = " = { ??? }" then hd :: tl else tl
            | x -> x
     ))
     
     let rec prover rules declarations = (* rules is a list of equalities with: variables in the equality, name of equality, lhs of equality, rhs of equality*)
      match declarations with
         | ProofDeclaration (nm, vars, Equality (lhs,rhs), None) :: rest
            -> (* no hint, so let's prove this *)
               (("Proof of " ^ nm ^ ":") :: prove rules lhs rhs) :: prover ((vars,nm,lhs,rhs)::rules) rest
         | ProofDeclaration (nm, vars, Equality (lhs,rhs), _) :: rest
            -> (* we got a hint so we simply assume the statement *)
               prover ((vars,nm,lhs,rhs)::rules) rest
         | _ :: rest -> prover rules rest
         | [] -> []
   let prover_main decls =
      prover [] decls |>
      List.map (String.concat "\n") |>
      String.concat "\n\n" |>
      print_endline