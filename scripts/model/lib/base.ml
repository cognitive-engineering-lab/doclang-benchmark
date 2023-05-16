type var = string

exception Undefined_behavior
exception Not_desugared

module Expr = struct
  type t = ..

  module Eval = Ext_func.Make(struct
      type input = t
      type output = t
    end)

  module Subst = Ext_func.Make(struct
      type input = var * t * t
      type output = t
    end)

  module Desugar = Ext_func.Make(struct
      type input = t
      type output = t
    end)

  module Show = Ext_func.Make(struct
      type input = t
      type output = string
    end)

  let eval = Eval.call
  let subst = Subst.call
  let desugar = Desugar.call
  let show = Show.call
  let desugar_eval e = eval (desugar e)
end

module type ExprFragment = sig
  val eval : Expr.t -> Expr.t option
  val subst : var * Expr.t * Expr.t -> Expr.t option
  val desugar : Expr.t -> Expr.t option
  val show : Expr.t -> string option
end

module MakeExprFragment(F: ExprFragment) = struct
  let register () = 
    Expr.Eval.register F.eval;
    Expr.Subst.register F.subst;
    Expr.Desugar.register F.desugar;
    Expr.Show.register F.show;
end

module Tinline = struct
  type t = ..
  type ttext = t list

  module Desugar = Ext_func.Make(struct
      type input = t
      type output = Expr.t
    end)

  module Desugar_ttext_elem = Ext_func.Make(struct
      type input = t * ttext
      type output = Expr.t
    end)

  module Show = Ext_func.Make(struct
      type input = t
      type output = string
    end)

  let desugar = Desugar.call
  let desugar_ttext_elem = Desugar_ttext_elem.call
  let show = Show.call
end


module type TinlineFragment = sig
  val desugar_tinline : Tinline.t -> Expr.t option
  val desugar_ttext_elem : Tinline.t * Tinline.ttext -> Expr.t option
  val show_tinline : Tinline.t -> string option
end

module MakeTinlineFragment(F: TinlineFragment) = struct
  let register () = 
    Tinline.Desugar.register F.desugar_tinline;
    Tinline.Desugar_ttext_elem.register F.desugar_ttext_elem;
    Tinline.Show.register F.show_tinline;
end

module Tblock = struct
  type t = ..
  type tarticle = t list

  module Desugar = Ext_func.Make(struct
      type input = t
      type output = Expr.t
    end)

  module Desugar_tarticle_elem = Ext_func.Make(struct
      type input = t * tarticle
      type output = Expr.t
    end)

  module Show = Ext_func.Make(struct
      type input = t
      type output = string
    end)

  let desugar = Desugar.call
  let desugar_tarticle_elem = Desugar_tarticle_elem.call
  let show = Show.call
end

module type TBlockFragment = sig
  val desugar_tblock : Tblock.t -> Expr.t option
  val desugar_tarticle_elem : Tblock.t * Tblock.tarticle -> Expr.t option
  val show_tblock : Tblock.t -> string option
end

module MakeTBlockFragment(F: TBlockFragment) = struct
  let register () = 
    Tblock.Desugar.register F.desugar_tblock;
    Tblock.Desugar_tarticle_elem.register F.desugar_tarticle_elem;
    Tblock.Show.register F.show_tblock;
end
