(*
  The Cluster names module defines functions for grouping/clustering
  names for entities that vary in which words are included.
*)
open! Core_kernel

let get_words s =
  String.lowercase s |> String.split_on_chars ~on:[ ' '; '-'; '/'; '('; ')'; ','; '.'; '&' ]

(**
  Accepts a list of names and returns the frequencies of the words
  that appear in the names.
*)
let get_word_freq ~ignore (names : string list) : float String.Table.t =
  let n, ws =
    List.fold names
      ~init:(0, String.Table.create ())
      ~f:(fun acc name ->
        get_words name
        |> List.fold ~init:acc ~f:(fun ((n, ws) as acc) w ->
               match String.Set.mem ignore w with
               | true -> acc
               | false ->
                 String.Table.incr ws w;
                 n + 1, ws))
  in
  String.Table.map ws ~f:(fun w -> w // n)

(**
  Accepts a string and returns the set of words that comprise it
  excluding those listed in the ignore set.
*)
let get_word_set ~ignore s =
  get_words s |> List.filter ~f:(fun w -> not @@ String.Set.mem ignore w) |> String.Set.of_list

(** An auxiliary function to compute x^y. *)
let expt x y = Float.exp (y *. Float.log x)

(**
    A metric that looks at two word sets and returns a score that is
    larger when the two sets share relatively rare words and smaller
    when the sets do not share rare words.

    Note: k determines how heavily to weight the rareness of words. k
    ranges from 0 to inf. Smaller values assign less weight to
    rareness.

          ---                  ---
          \        1           \        1
          /     -------     -  /    ----------
          ---   w_f ^ k_s      ---  w_f ^ k_ns
      w in w0 inter w1     w in w0 xor w1
  *)
let metric ~k_shared ~k_not_shared freq ws0 ws1 =
  let open Float in
  let f k =
    String.Set.fold ~init:0.0 ~f:(fun sum w ->
        sum + (String.Table.find freq w |> Option.value_map ~default:0.0 ~f:(fun wf -> 1.0 / expt wf k)))
  in

  f k_shared (String.Set.inter ws0 ws1)
  - f k_not_shared (String.Set.union (String.Set.diff ws0 ws1) (String.Set.diff ws1 ws0))

module type Make_arg = sig
  (** Represents named entries *)
  type t [@@deriving sexp, compare]

  val get_name : t -> string
end

module Make (M : Make_arg) = struct
  (** Represents clusters of entries that have similar names.*)
  type cluster = {
    label: string;
    word_set: String.Set.t;
    entries: M.t list;
  }
  [@@deriving sexp, compare]

  type t = cluster [@@deriving sexp]

  module Set = struct
    include Set.Make (struct
      type t = cluster [@@deriving sexp, compare]
    end)
  end

  type create_init_clusters_arg = {
    label: string;
    name: string;
  }

  (**
    Accepts a list of names to seed clusters around and generates
    empty clusters centered on these names.

    WARNING: once ignored words are removed, any remaining equivalent
    clusters will be merged.
  *)
  let create_init_clusters ~ignore =
    List.fold ~init:Set.empty ~f:(fun acc ({ label; name } : create_init_clusters_arg) ->
        Set.add acc { label; word_set = get_word_set ~ignore name; entries = [] })

  (**
    Accepts a list of entries and groups them into clusters based on
    their names.

    This function splits the names into sets of words then groups the
    entries into clusters that share relatively rare words.

    See: metric for more details about how similarity/distance is
    computed.

    The k_shared and k_not_shared parameters specify how much this
    function will weight words that are shared vs words that are not
    shared. The ignore parameter specifies words that will be ignored
    during clustering.

    Note: all entries that consist of either the empty string, blank
    spaces, and or words in the ignore list will be grouped together
    in a single cluster that has an empty word set.
  *)
  let group ~k_shared ~k_not_shared ~ignore (entries : M.t list) : t list =
    let freq = List.map entries ~f:M.get_name |> get_word_freq ~ignore in
    let ignore = String.Set.add ignore "" in
    (* seed with initial clusters if given *)
    List.fold entries ~init:(Bag.create ()) ~f:(fun acc entry ->
        let name = M.get_name entry in
        let ws = get_word_set ~ignore name in
        (* find the closest cluster *)
        Bag.fold acc ~init:None ~f:(fun acc (x : t ref) ->
            let { word_set; _ } = !x in
            let score = metric ~k_shared ~k_not_shared freq word_set ws in
            match acc with
            | None -> Option.some_if Float.(0.0 <= score) (score, x)
            | Some ((curr_score, _) as curr) ->
              Some (if Float.(curr_score <= score) then score, x else curr))
        |> function
        | Some (_, x) ->
          (* add entry to cluster *)
          let { label; word_set; entries } = !x in
          x := { label; word_set = String.Set.inter word_set ws; entries = List.cons entry entries };
          acc
        | None ->
          (* add new cluster *)
          Bag.add_unit acc (ref { label = name; word_set = ws; entries = [ entry ] });
          acc)
    |> Bag.to_list
    |> List.map ~f:(fun x -> !x)

  type cluster_res = {
    clusters: t list;
    rejected: M.t list String.Table.t;
  }

  type cluster_acc = {
    clusters: t ref list;
    rejected: M.t list String.Table.t;
  }
  [@@deriving stable_record ~version:cluster_res ~modify:[ clusters ]]

  (**
    Accepts a list of entries and groups them into clusters based on
    their names. This function accepts a bag of clusters and groups
    the given entries around them.
  *)
  let cluster ~k_shared ~k_not_shared ~ignore ~clusters (entries : M.t list) : cluster_res =
    let freq = List.map entries ~f:M.get_name |> get_word_freq ~ignore in
    let ignore = String.Set.add ignore "" in
    let init_clusters = Set.to_list clusters |> List.map ~f:(fun cluster -> ref cluster) in
    let res =
      List.fold entries
        ~init:{ clusters = init_clusters; rejected = String.Table.create () }
        ~f:(fun ({ clusters; rejected } as acc) entry ->
          let name = M.get_name entry in
          let ws = get_word_set ~ignore name in
          (* find the closest cluster *)
          List.fold clusters ~init:None ~f:(fun acc (cluster : t ref) ->
              let { word_set; _ } = !cluster in
              let score = metric ~k_shared ~k_not_shared freq word_set ws in
              match acc with
              | None -> Option.some_if Float.(0.0 <= score) (score, cluster)
              | Some ((curr_score, _) as curr) ->
                Some (if Float.(curr_score <= score) then score, cluster else curr))
          |> function
          | Some (_, cluster) ->
            (* add entry to cluster *)
            let { label; word_set; entries } = !cluster in
            cluster := { label; word_set; entries = List.cons entry entries };
            acc
          | None ->
            (* add new cluster *)
            String.Table.update rejected name ~f:(function
              | None -> [ entry ]
              | Some entries -> entry :: entries);
            acc)
    in
    cluster_acc_to_cluster_res res ~modify_clusters:(List.map ~f:(fun cluster -> !cluster))
end
