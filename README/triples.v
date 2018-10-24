Definition fst3 {A B C} (triple : A * B * C) : A :=
  match triple with
  | (first, second, third) => second
  end.
