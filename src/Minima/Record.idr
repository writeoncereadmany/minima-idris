module Record

%access public export

namespace Record
  infix 8 :=

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
             {auto prf : FieldType field xs t} -> t
    getField f rec {prf} = getField' f rec prf

    updateField' : (field : lbl) -> (newVal : t) -> Record xs -> FieldType field xs t -> Record xs
    updateField' field newVal (_ :: xs) First = (field := newVal) :: xs
    updateField' field newVal (x :: xs) (Later y) = x :: updateField' field newVal xs y

    updateField : (field : lbl) -> (newVal : t) -> Record xs ->
                  { auto prf : FieldType field xs t } -> Record xs
    updateField field newVal rec { prf } = updateField' field newVal rec prf
