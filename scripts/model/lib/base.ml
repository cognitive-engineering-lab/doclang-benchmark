type var = string

exception Undefined_behavior
exception Not_desugared
exception Type_error of string

module Type = struct
  type t = ..
  type ctx_elem = ..
  type ctx = ctx_elem list

  module Subst = Open_func.Make(struct
      type input = var * t * t
      type output = t
    end)

  module Show = Open_func.Make(struct
      type input = t
      type output = string
    end)

  let subst = Subst.call
  let show = Show.call
end


module type TypeFragment = sig
  val type_subst : var * Type.t * Type.t -> Type.t
  val type_show : Type.t -> string
end

module MakeTypeFragment(F: TypeFragment) = struct
  let register () = 
    Type.Subst.register F.type_subst;
    Type.Show.register F.type_show;
end


module Expr = struct
  type t = ..

  module Eval = Open_func.Make(struct
      type input = t
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

  module Is_article = Open_func.Make(struct
      type input = t
      type output = bool
    end)

  module Typecheck = Open_func.Make(struct
      type input = Type.ctx * t
      type output = Type.t
    end)

  let eval = Eval.call
  let subst = Subst.call
  let desugar = Desugar.call
  let show = Show.call
  let is_article = Is_article.call
  let typecheck = Typecheck.call
  let desugar_eval e = eval (desugar e)  
end

module type ExprFragment = sig
  val eval : Expr.t -> Expr.t
  val subst : var * Expr.t * Expr.t -> Expr.t
  val desugar : Expr.t -> Expr.t
  val show : Expr.t -> string
  val is_article : Expr.t -> bool
  val typecheck : Type.ctx * Expr.t -> Type.t 
end

module MakeExprFragment(F: ExprFragment) = struct
  let register () = 
    Expr.Eval.register F.eval;
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