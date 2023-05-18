exception Missing_case

let noop _ = raise (Match_failure ("", 0, 0))

module type Input = sig 
  type input
  type output
end

module Make(F : Input) = struct
  let f_cell : (F.input -> F.output) ref = ref (fun _ -> raise Missing_case)

  let register (f : F.input -> F.output) : unit =
    let prev = !f_cell in
    f_cell := fun t -> try f t with Match_failure _ -> prev t

  let call (x : F.input) : F.output = !f_cell x
end