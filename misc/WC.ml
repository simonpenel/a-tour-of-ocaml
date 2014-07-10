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

let 	get_nbchar_slist slist = 				(* donne le nombre de char dans une liste de string *)
	somme_entiers_lfold (List.map  ~f:String.length slist )	
	;;

	
(* Counting words *)

let split_chaine chaine =
	List.filter ~f:(fun x -> (String.length x )>  0) (String.split_on_chars chaine [' ';'\t']	)	(* Je supprime les mots de longueur 0*)	
	(* Une doc indique val split_on_chars : string -> on:char list -> escape_char:char -> string list Probleme de version?*)
	
	;;

let nb_mots_chaine chaine =			(* nombre de mots dans une chaine*)
	List.length (split_chaine chaine )
	;;

let nb_mots_slist slist =			(* nombre de mots dans une liste de chaine*)
	somme_entiers_lfold (List.map  ~f:nb_mots_chaine slist )	
	;;
	
	
(* Command-line interface *)


  

let wc () =							(* Renvoie bv de lignes, mots, characteres (nb de char est faux car pas de prise en compte des "retour chariot" *)
  if Array.length Sys.argv > 1 then
  let premierarg = Sys.argv.(1) in 
  if Sys.file_exists_exn premierarg then
  if Sys.is_file_exn  premierarg then
  let slignes =  get_lignes  premierarg in
  let nlines, nmots, nchar = (List.length slignes, nb_mots_slist slignes, get_nbchar_slist slignes) in
  printf "%d %d %d\n" nlines nmots nchar
  else
  printf "impossible de traiter un repertoire\n"
  else
  printf "fichier d'entree inexistant\n"
  else
  printf "pas de fichier d'entree\n"
  ;;
wc () ;;
