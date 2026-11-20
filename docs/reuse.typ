#import "@preview/curryst:0.6.0": rule, prooftree, rule-set

#let VDash = symbol("\u{22A9}")

#let pt(..args) = prooftree(rule(..args))

$
#let rvar = pt(
  name: [[RVAR]],
  [$S VDash x | R ~> x | R$]
)

#let rcon_ru = pt(
  name: [[RCON-REUSE]],
  [$overline(x)=x_1 "..." x_n$],
  [$S VDash C overline(x) | R, r:n ~> C overline(x) | R$]
)

#let rcon = pt(
  name: [[RCON]],
  [$overline(x)=x_1 "..." x_n$],
  [$r:n in.not R$],
  [$S VDash C overline(x) | R ~> C overline(x) | R$]
)

#let rapp = pt(
  name: [[RAPP]],
  [$S VDash e | R ~> e' | R'$],
  [$S VDash e x | R ~> e' x | R'$]
)

#let rlam = pt(
  name: [[RLAM]],
  [$emptyset VDash e | emptyset ~> e' | emptyset$],
  [$S VDash lambda_Gamma x.e | R ~> lambda_Gamma x.e' | R$]
)

#let rdup = pt(
  name: [[RDUP]],
  [$S VDash e | R ~> e' | R'$],
  [$S VDash "dup" x; e | R ~> "dup" x; e' | R'$]
)


#let rdropr = pt(
  name: [[RDROPR]],
  [$S VDash e | R, r : n ~> e' | R', r : n$],
  [$S VDash e | R, r : n ~> "drop" r;e'$]
)

#let rdrop_reuse = pt(
  name: [[RDROP-REUSE]],
  [$x : n in S$],
  [$S VDash e | R, r : n ~> e' | R'$],
  [$r in.not R'$],
  [$"fresh" r$],
  [$S VDash "drop" x ; e | R ~> r <- "dropru" x; e'| R'$]
)

#let rlet = pt(
  name: [[RLET]],
  [$S VDash e_1 | R ~> e'_1 | R'$],
  [$S VDash e_2 | R' ~> e'_2 | R''$],
  [$S VDash "let" x = e_1 "in" e_2 | R ~> "let" x = e'_1 "in" e'_2 | R''$]
)

#let rdrop = pt(
  name: [[RDROP]],
  [$S VDash e | R ~> e' | R'$],
  [$S VDash "drop" x ; e | R ~> "drop" x ; e' | R'$]
)

#let rdrop_ru = pt(
  name: [[RDROPRU]],
  [$S VDash e | R, r ~> e' | R'$],
  [$r in.not R'$],
  [$S VDash r <- "dropru" x; e | R ~> r <- "dropru" x; e' | R'$]
)

#let rmatch = pt(
  name: [[RMATCH]],
  [$S, x: n VDash e_i | R ~> e'_i | R'_i$],
  [$p_i = C x_1 "..." x_n$],
  [$R' = R'_1 union "..." union R'_i$],
  [$S VDash "match" x {overline(p_i |-> e_i)} | R ~> "match" x {overline(p_i |-> e'_i)} | R'$]
)

#align(center, rule-set(rvar, rcon, rcon_ru, rapp, rlam, rdup, rdropr, rdrop_reuse, rlet, rdrop_ru, rdrop, rmatch))
$
