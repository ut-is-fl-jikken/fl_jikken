module Make :
  (Stdlib : sig
              type 'a ref
              val ref : 'a -> 'a ref
              val ( := ) : 'a ref -> 'a -> unit
              val ( + ) : int -> int -> int
              val ( ! ) : 'a ref -> 'a
              val string_of_int : int -> string
              val print_string : string -> unit
              val ( ^ ) : string -> string -> string
              val ( = ) : 'a -> 'a -> bool
              val raise : exn -> 'a
            end)
    ->
    sig
      module TySyntax :
        sig
          type tyvar
          type ty = TyInt | TyBool | TyFun of ty * ty | TyVar of tyvar
          (*
           * Generate a fresh type variable
           *   (i.e. a type variable that has not been appeared)
           *)
          val new_tyvar : unit -> tyvar
          val print_type : ty -> unit
        end
      module ConstraintSolver :
        sig
          exception TyError
          (*
           * the type of substitution
           *)
          type subst
          (*
           * the type of constraints
           *   a list of equations t1 = t2 for types t1 and t2
           *)
          type constraints = (TySyntax.ty * TySyntax.ty) list
          (*
           * apply the substitution to the type
           *)
          val ty_subst :
            subst -> TySyntax.ty -> TySyntax.ty
          (*
           * return the most general unifier of the constraints
           * raise TyError if unification fails
           *)
          val unify :
            constraints -> subst
        end
    end
