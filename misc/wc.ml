#require "core"
open Core.Std

(* Counting lines *)

let nombre_lignes fichier =
let list = In_channel.read_lines fichier in
List.length list
;;


(* Counting characters *)


let rec somme_entiers list =
	match list with
	| [] -> 0
	| head::tail -> head + somme_entiers tail		(* elle n'est pas tail recursive*)
	;;

let rec somme_entiers_tailrec felement list =			(* elle est  tail recursive*)
	match list with
	| [] -> felement
	| head::tail ->somme_entiers_tailrec (head + felement) tail
	;;
let 	somme_entiers_2 list = 	somme_entiers_tailrec	0 list ;;					(* pour l'utiliser*)


let 	somme_entiers_lfold list = 
	List.fold ~init:0 ~f:(+) list;;				(* avec List.fold *)


let     get_lignes fichier = 
	In_channel.read_lines fichier 					(* recupere la liste des lignes*)
;;

let     get_ligne_lengths fichier = 				(* donne la liste des longueurs d'une liste de ligne *)
	List.map  ~f:String.length  (get_lignes fichier)
;;

let     get_nbchar fichier = 					(* donne le nombre de char dans un fichier *)
	somme_entiers_lfold (get_ligne_lengths fichier)		(* attention les lignes vides ne sont pas comptabilisees *)
;;
