type var = string

exception Undefined_behavior
exception Not_desugared

module Expr = struct
  type t = ..

  module Eval = Open_func.Make(struct
      type input = t
      type output = t
    end)

  module Eval_list = Open_func.Make(struct
      type input = t * t list
      type output = t
    end)

  module Subst = Open_func.Make(struct
      type input = var * t * t
      type output = t
    end)

  module Desugar = Open_func.Make(struct
      type input = t
      type output = t
    end)

  module Show = Open_func.Make(struct
      type input = t
      type output = string
    end)

  let eval = Eval.call
  let eval_list = Eval_list.call
  let subst = Subst.call
  let desugar = Desugar.call
  let show = Show.call
  let desugar_eval e = eval (desugar e)
end

module type ExprFragment = sig
  val eval : Expr.t -> Expr.t
  val eval_list : Expr.t * Expr.t list -> Expr.t
  val subst : var * Expr.t * Expr.t -> Expr.t
  val desugar : Expr.t -> Expr.t
  val show : Expr.t -> string
end

module MakeExprFragment(F: ExprFragment) = struct
  let register () = 
    Expr.Eval.register F.eval;
    Expr.Eval_list.register F.eval_list;
    Expr.Subst.register F.subst;
    Expr.Desugar.register F.desugar;
    Expr.Show.register F.show;
end

module Template = struct
  type t = ..
  type ttext = t list

  module Desugar = Open_func.Make(struct
      type input = t
      type output = Expr.t
    end)

  module Desugar_in_context = Open_func.Make(struct
      type input = t * ttext
      type output = Expr.t
    end)

  module Show = Open_func.Make(struct
      type input = t
      type output = string
    end)

  let desugar = Desugar.call
  let desugar_in_context = Desugar_in_context.call
  let show = Show.call
end

module type TemplateFragment = sig
  val desugar_template : Template.t -> Expr.t
  val desugar_template_in_context : Template.t * Template.ttext -> Expr.t
  val show_template : Template.t -> string
end

module MakeTemplateFragment(F: TemplateFragment) = struct
  let register () = 
    Template.Desugar.register F.desugar_template;
    Template.Desugar_in_context.register F.desugar_template_in_context;
    Template.Show.register F.show_template;
end
