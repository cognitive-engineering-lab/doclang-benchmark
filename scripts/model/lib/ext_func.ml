exception Undefined_behavior

module type Input = sig 
  type input
  type output
end

module type Output = functor (F: Input) -> sig 
  val register : (F.input -> F.output option) -> unit
  val call : F.input -> F.output
end

module Make(F : Input) = struct
  let f_cell : (F.input -> F.output option) ref = ref (fun _ -> None)

  let register (f : F.input -> F.output option) : unit =
    let prev = !f_cell in
    f_cell := fun t -> match f t with Some v -> Some v | None -> prev t

  let call (x : F.input) : F.output = match !f_cell x with
    | Some v -> v
    | None -> raise Undefined_behavior
end