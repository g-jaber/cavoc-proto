(** Point d'entrée principal pour l'interface web de l'explorateur (CAVOC).
    Ce module gère les interactions DOM, l'affichage via Ace Editor et la boucle d'évaluation. *)

open Js_of_ocaml

(** {1 État global} *)

val editor_content : string ref
(** Contient le code source OCaml courant récupéré de l'éditeur Ace. *)

val signature_content : string ref
(** Contient la signature OCaml courante récupérée de l'éditeur de signature. *)

val previous_moves : string list ref
(** Liste mutable stockant l'historique des coups joués. *)

(** {1 Manipulation du DOM et Affichage} *)

val fetch_editor_content : unit -> unit
(** Récupère le contenu des éditeurs Ace (code et signature) et met à jour les références globales. *)

val print_to_output : string -> unit
(** Redirige une chaîne de caractères vers la console HTML (div #console). *)

val display_previous_moves : unit -> unit
(** Met à jour l'affichage de l'historique des coups dans le DOM. *)

val add_move : string -> unit
(** Ajoute un coup à l'historique et rafraîchit l'affichage. *)

val flush_moves : unit -> unit
(** Vide l'historique des coups et l'affichage correspondant. *)

val update_container : string -> string -> unit
(** Remplace le contenu HTML interne de l'élément ayant l'ID donné. *)

val highlight_subject : string -> unit
(** Met en évidence l'élément correspondant dans l'environnement interactif (ienv)
    basé sur le JSON du coup sélectionné. *)

(** {1 Génération de HTML} *)

val html_escape : string -> string
(** Échappe les caractères spéciaux HTML (<, >, &). *)

val generate_store_html : string -> string
(** Génère le HTML pour l'ancien format de store (chaîne brute). *)

val generate_store_html_from_json : Yojson.Safe.t -> Yojson.Safe.t option -> string
(** Génère le HTML pour le store à partir du format JSON.
    Filtre les variables déjà présentes dans [ienv] pour éviter les doublons. *)

val generate_ienv_html : Yojson.Safe.t -> string
(** Génère le HTML pour l'environnement interactif (ienv), créant des blocs
    préparés pour l'initialisation des éditeurs Ace en lecture seule. *)

val generate_clickables : (int * string) list -> unit
(** Génère les boutons radio cliquables pour la liste des coups possibles. *)

val clear_list : unit -> unit
(** Vide la liste des coups affichée dans l'interface. *)

(** {1 Configuration et Logique} *)

val display_conf : Yojson.Safe.t -> unit
(** Affiche la configuration courante (store, ienv, etc.) dans les onglets appropriés.
    Initialise également les instances Ace pour l'ienv. *)

val generate_kind_lts : unit -> Lts_kind.kind_lts
(** Génère la configuration du système de transition (LTS).
    Lit l'état de la case à cocher "Direct Style" dans le DOM pour déterminer
    le mode de contrôle (CPS ou DirectStyle). *)

val get_chosen_move : int -> int Lwt.t
(** Attend que l'utilisateur clique sur un bouton (Select, Load, Stop).
    Renvoie l'index du coup choisi, ou un code d'erreur/arrêt. *)

(** {1 Aide et Markdown} *)

val parse_markdown : string -> string
(** Convertit une chaîne Markdown en HTML en utilisant la librairie JS `marked`. *)

val show_help : unit -> unit Lwt.t
(** Affiche la modale d'aide en chargeant le contenu depuis `help.md`. *)

val close_help : unit -> bool Js.t
(** Ferme la modale d'aide. *)

val init_help_events : unit -> unit
(** Initialise les écouteurs d'événements pour le bouton d'aide et la fermeture de la modale. *)

(** {1 Boucle Principale} *)

val evaluate_code : unit -> int Lwt.t
(** Lance l'évaluation du code : parsing, construction du LTS et boucle interactive. *)

val init_page : unit -> unit
(** Fonction d'initialisation principale. Configure les boutons et lance la boucle d'événements. *)