module Record

%access public export

namespace Record
  infix 5 :=

  data Field : lbl -> Type -> Type where
        (:=) : (field_label : lbl) ->
               (value : b) -> Field field_label b

  using (field : lbl, xs : List (lbl, Type))
    data Record : List (lbl, Type) -> Type where
      Nil  : Record { lbl } []
      (::) : Field field a -> Record xs -> Record ((field, a) :: xs)

    data FieldType : lbl -> List (lbl, Type) -> Type -> Type where
      First : FieldType field ((field, ty) :: xs) ty
      Later : FieldType field xs t -> FieldType field (a :: xs) t

    getField' : (field : lbl) -> Record xs -> FieldType field xs t -> t
    getField' field ((_ := val) :: xs) First = val
    getField' field (_ :: xs) (Later y) = getField' field xs y

    getField : (field : lbl) -> Record xs  ->
             {default tactics { search } prf : FieldType field xs t} -> t
    getField f rec {prf} = getField' f rec prf
