
(*
 *      Copyright (C) 2024 by Laboratoire Méthodes Formelles (LMF),
 *      UMR 9021  Université Paris-Saclay, CNRS et ENS Paris-Saclay.
 *      Modified by Mihaela Sighireanu.
 *
 *      Copyright (C) 2005, 2006 by Laboratoire Spécification et Vérification (LSV),
 *      UMR 8643 CNRS & ENS Cachan.
 *      Written by Jean Goubault-Larrecq.  Derived from the csur project.
 *
 *      Permission is granted to anyone to use this software for any
 *      purpose on any computer system, and to redistribute it freely,
 *      subject to the following restrictions:
 *
 *      1. Neither the author nor its employer is responsible for the consequences of use of
 *              this software, no matter how awful, even if they arise
 *              from defects in it.
 *
 *      2. The origin of this software must not be misrepresented, either
 *              by explicit claim or by omission.
 *
 *      3. Altered versions must be plainly marked as such, and must not
 *              be misrepresented as being the original software.
 *
 *      4. This software is restricted to non-commercial use only.  Commercial
 *              use is subject to a specific license, obtainable from LMF.
 *
*)

type typ = Tscalar of typ_basic | Tfun of typ_fun

(* type integer or pointer *)
and  typ_basic = Tint | Tptr of typ

(* type of functions *)
and  typ_fun = { result: typ_basic;  args: typ_basic list }
