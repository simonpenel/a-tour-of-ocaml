 (* Simple functions
    ----------------  *)


let carre x = 
x *. x 
;;

let valeur_absolue x = 
if x > 0. then  x
else  -.x 
;;

let moyenne x y =
(x +. y) /. 2.
;;

let gmoyenne x y =
sqrt (x *. y)
;;

let evalue fonction x = fonction x ;; (* 'a est le type de fonction 'b est le type de x *)

let compose f g =  function x -> f (g x);; 

(* Functions on 2d vectors 
   ----------------------- *)

type vecteur  = {x : float ; y : float};;

let origine = { x=0. ; y=0.} ;;
let ex = {x=1. ; y=0. };;
let ey = {x=0. ; y=1. };;


let vdist v1 v2 =
sqrt ( (v2.x -. v1.x) ** 2. +.  (v2.y -. v1.y) ** 2.) 
;;

let norm2 v =
sqrt ( v.x  ** 2. +.  v.y  ** 2.) 
;;

let  (+|) v1 v2 =
{x=v1.x +. v2.x ; y=v1.y +. v2.y}
;; 

let  ( *| ) lambda v1  =
{x=v1.x *. lambda ; y=v1.y *. lambda}
;; 

norm2 (ex +| 2. *| ey );;	(* Calcul de la norme*)



(* First steps with recursion 
   ----------------------- *)


let rec sigma n = 
if n = 0 then 0
else sigma ( n- 1 ) + n
;;


(* Question : - difference d'utilisation  if / then et | 
              - sigma -1 vs sigma (-1)

*)

(* nouvelle version *)

let rec sigma n = 
if n <= 0 then 0
else sigma ( n- 1 ) + n
;;

(* nouvelle nouvelle version *)


let rec sigma n = 
if n < 0 then failwith "negativer Wert, Dummkopf!"
else
if n = 0 then 0
else sigma ( n- 1 ) + n
;;


let rec factorielle n = 
if n <= 0 then 0
else
if n = 1 then 1
else factorielle ( n- 1 ) * n
;;


let identite  = function x -> x ;;

let rec puissance f  n =
if n <= 0 then identite
else 
if n = 1 then f
else   compose (puissance f (n-1))   f
;;

(* exemple *)

let ajoute_un x = x +. 1. ;;

puissance ajoute_un 30 0.;;

(* Application: derivatives of a function 
   -------------------------------------- *)
   
 let derivee f epsilone = function  x -> 
 if epsilone = 0. then failwith " epsilone nul, ducon"
 else (f ( x +. epsilone)  -. f x    ) /. epsilone
 ;;

(* Exemple *)
let derivee_carre  = derivee carre 0.0000000001 ;;

derivee_carre 0. ;;
derivee_carre 3. ;;


(* Application: computing a root square by dichotomy
   ------------------------------------------------- *)
   
let rec racine_carre_dichotomique a  b  x epsilone =
if (b -. a) < epsilone then [a;b]  
else
if (carre (( a +. b) /. 2. )) > x then  racine_carre_dichotomique a (( a +. b) /. 2.) x epsilone  
else  racine_carre_dichotomique (( a +. b) /. 2.)  b  x epsilone  
;;
   
let rec racine_carre x epsilone = racine_carre_dichotomique 0.  x  x epsilone ;; (* J'ai pas reussi a faire ca en une seule fonction *)
