datatype nat = ZERO | SUCC of nat;

fun is_positive n =
  n <> ZERO;
