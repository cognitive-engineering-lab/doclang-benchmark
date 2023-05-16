open Base
open Strings

module DArtProg = struct
  type Expr.t += 
    | Bold of Expr.t
    | Para of Expr.t
    | Section of Expr.t * Expr.t

  (* Boring code *)

  let eval e = match e with
    | Bold e -> Some(Bold (Expr.eval e))
    | Para e -> Some(Para (Expr.eval e))
    | Section (e1, e2) -> Some(Section (Expr.eval e1, Expr.eval e2))
    | _ -> None

  let subst (x, e1, e2) = match e2 with
    | Bold e' -> Some(Bold (Expr.subst (x, e1, e')))
    | Para e' -> Some(Para (Expr.subst (x, e1, e')))
    | Section (e1', e2') -> Some(Section (Expr.subst (x, e1, e1'), Expr.subst (x, e1, e2')))
    | _ -> None

  let desugar e = match e with
    | Bold e -> Some(Bold (Expr.desugar e))
    | Para e -> Some(Para (Expr.desugar e))
    | Section (e1, e2) -> Some(Section (Expr.desugar e1, Expr.desugar e2))
    | _ -> None

  let show e = match e with
    | Bold e -> Some(Printf.sprintf "<bold>%s</bold>" (Expr.show e))
    | Para e -> Some(Printf.sprintf "<p>%s</p>" (Expr.show e))
    | Section (e1, e2) -> Some(Printf.sprintf "<section><header>%s</header>%s</section>" (Expr.show e1) (Expr.show e2))
    | _ -> None  
end
module DArtProgFrag = MakeExprFragment(DArtProg)



module DArtTLit = struct
  open DStrTLit
  open DArtProg
  open Tinline

  type Tblock.t += 
    | TPara of ttext 
    | TSection of ttext * Tblock.tarticle 
    | TBlockExpr of Expr.t
  type Tinline.t += TBold of ttext 
  type Expr.t += ArtTmpl of Tblock.tarticle

  let rec desugar e = match e with
    | ArtTmpl kt -> Some(desugar_tarticle kt)
    | _ -> None
  and desugar_tarticle kt = Flatten(desugar_tarticle_rec kt)
  and desugar_tarticle_rec at = match at with 
    | [] -> List []
    | bt :: bts -> Tblock.desugar_tarticle_elem (bt, bts)

  let[@warning "-partial-match"] desugar_tarticle_elem (bt, bts) = 
    let List l = desugar_tarticle_rec bts in 
    Some(List ((Tblock.desugar bt) :: l))

  let desugar_tblock kt = match kt with
    | TPara kt -> Some(Para (desugar_ttext kt))
    | TSection (kt1, kt2) -> Some(Section (desugar_ttext kt1, desugar_tarticle kt2)    )
    | TBlockExpr e -> Some(Expr.desugar e)
    | _ -> None

  let desugar_tinline lt = match lt with
    | TBold kt -> Some(Bold (desugar_ttext kt))
    | _ -> None

  (* Boring code *)

  let eval e = match e with
    | ArtTmpl _ -> raise Not_desugared
    | _ -> None

  let desugar_ttext_elem _ = None

  let subst (_, _, e2) = match e2 with
    | ArtTmpl _ -> raise Not_desugared
    | _ -> None

  let rec show e = match e with
    | ArtTmpl kt -> Some(show_tarticle kt)
    | _ -> None
  and show_tarticle kt = String.concat "" (List.map Tblock.show kt)

  let show_tblock kt = match kt with
    | TPara kt -> Some(Printf.sprintf "<p>%s</p>" (show_ttext kt))
    | TSection (kt1, kt2) -> Some(Printf.sprintf "<section><header>%s</header>%s</section>" (show_ttext kt1) (show_tarticle kt2))
    | _ -> None

  let show_tinline lt = match lt with
    | TBold kt -> Some(Printf.sprintf "<bold>%s</bold>" (show_ttext kt))
    | _ -> None  
end
module DArtTLitFrag = MakeExprFragment(DArtTLit)
module DArtTLitInlineFrag = MakeTinlineFragment(DArtTLit)
module DArtTLitBlockFrag = MakeTBlockFragment(DArtTLit)

module DArtTProg = struct 
  open DStrProg
  open DStrTProg
  open DArtTLit

  type Tblock.t += 
    | TBlockForeach of Expr.t * var * Tblock.tarticle 
    | TBlockSet of var * Expr.t  

  let desugar_tarticle_elem (bt, bts) = match bt with
    | TBlockSet (x, e) -> Some(Let (x, e, desugar_tarticle_rec bts))
    | _ -> None  

  let desugar_tblock bt = match bt with
    | TBlockForeach (e1, x, kt) -> Some(Foreach (Expr.desugar e1, x, desugar_tarticle kt))
    | _ -> None

  (* Boring code *)
  let show_tblock bt = match bt with
    | TBlockForeach (e1, x, kt) -> Some(Printf.sprintf "{{ foreach %s in %s }} %s {{ endforeach }}" x (Expr.show e1) (show_tarticle kt))
    | TBlockSet (x, e) -> Some(Printf.sprintf "{{ %s = %s }}" x (Expr.show e))
    | _ -> None
end
module DArtTProgBlockFrag = MakeTBlockFragment(DArtTProg)