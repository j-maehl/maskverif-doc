require import Distr.
require (*++*) Ring.
require (*++*) Int.

type byte.
op id (x:byte) = x.
op expr: byte -> int -> byte.
op (>>): byte -> int -> byte.
op (&): byte -> byte -> byte.

clone export Ring.ComRing with
  type t           <- byte,
  op [-]           = fun (x:byte), x.

axiom addrK (x:byte): x + x = zeror.

axiom nosmt expr0 x: expr x 0 = oner.
axiom nosmt exprS x n: Int.(<=) 0 n => expr x (Int.(+) n 1) = x * (expr x n).

lemma nosmt Cn_eq0: ofint 2 = zeror.
proof.
  rewrite (_: 2 = Int.(+) (Int.(+) 0 1) 1) // !ofintS 3:ofint0 //.
  by rewrite addrA addrK addr0.
qed.

instance ring 2 _ with byte
  op rzero = zeror
  op rone  = oner
  op add   = ( + )
  op opp   = id
  op mul   = ( * )
  op sub   = ( + )
  op expr  = expr
  op ofint = ofint

  proof oner_neq0 by apply oner_neq0
  proof addr0     by apply addr0
  proof addrA     by apply addrA
  proof addrC     by apply addrC
  proof addrN     by apply addrK
  proof mulr1     by apply mulr1
  proof mulrA     by apply mulrA
  proof mulrC     by apply mulrC
  proof mulrDl    by (move=> x y z;rewrite mulrDl;done)
  proof subrE     by done
  proof expr0     by apply expr0
  proof exprS     by apply exprS
  proof ofint0    by apply ofint0
  proof ofint1    by apply ofint1
  proof ofintS    by apply ofintS
  proof ofintN    by apply ofintN
  proof Cn_eq0    by apply Cn_eq0.

op distr: byte distr.
axiom distr_ll: weight distr = 1%r.

lemma natmul2K n x: ofint (Int.( * ) 2 n) * x = zeror.
proof.
  cut posn: forall n, Int.(<=) 0 n => ofint (Int.( * ) 2 n) * x = zeror.
    elim/Int.intind; first by algebra.
    move=> k le0n IH.
    rewrite (_: Int.( * ) 2 (Int.(+) k 1) = Int.(+) (Int.(+) (Int.( * ) 2 k) 1) 1) 1:smt.
    by rewrite ofintS 1:smt ofintS 1:smt addrA mulrDl IH addrK; algebra.
  case (Int.(<=) 0 n); first by apply posn.
  move=> ltn0; rewrite -ofintN (_: Int.([-]) (Int.( * ) 2 n) = Int.( * ) 2 (Int.([-]) n)) 1:smt.
  by apply posn; smt.
qed.

op pow2 x = expr x 2.
op pow4 x = expr x 4.
op pow16 x = expr x 16.

lemma pow2_additive x y: pow2 (x + y) = pow2 x + pow2 y.
proof.
  rewrite /pow2 (_: 2 = Int.(+) (Int.(+) 0 1) 1) // !exprS // !expr0 !mulr1.
  rewrite !mulrDl !mulrDr (mulrC y x) -addrA (addrA (x * y)) addrK (addrC zeror) addr0.
  done.
qed.

lemma pow4_additive x y: pow4 (x + y) = pow4 x + pow4 y.
proof.
  rewrite (_: pow4 = fun x, pow2 (pow2 x)) //=.
    by apply fun_ext=> z /=; rewrite /pow4 !/pow2; algebra.
  by rewrite !pow2_additive.
qed.

lemma pow16_additive x y: pow16 (x + y) = pow16 x + pow16 y.
proof.
  rewrite (_: pow16 = fun x, pow4 (pow4 x)) //=.
    by apply fun_ext=> z /=; rewrite /pow16 !/pow4; algebra.
  by rewrite !pow4_additive.
qed.
