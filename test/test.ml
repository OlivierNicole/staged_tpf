module Iter = Tpf_staged.Iter

let () =
  Format.printf "@[<v>";
  Format.printf "------@,";
  Iter.show ();
  Format.printf "@,";
  Format.printf "------@,";
  Iter.iter_list (Format.printf "%d@,") [42; (-1); (-898)];
  (*M.iter_option_list (Format.printf "%d@,") [Some 42; Some (-1); None; Some (-898)];*)
  Format.printf "------@,";

[@@@warning "-ignored-partial-application"]
[@@@warning "-unused-value-declaration"]
[@@@warning "-unused-var-strict"]
[@@@warning "-unused-rec-flag"]

module Dyn'1 = struct
  module Iterate_proxy = struct
    external (!) : 'a -> 'a = "%identity"
    external (!:) : 'a -> 'a = "%identity"
  end
end

let _ =
  fun f ->
    fun l ->
        (
           (fun v ->
              let rec fix x =
                match x with
                | [] -> ()
                | x::xs ->
                    ((();
                        (
                           (fun v ->
                                (
                                   (fun v ->
                                      let rec fix x =
                                        match x with
                                        | None -> ()
                                        | Some a ->
                                            (();
                                               (
                                                  (fun v ->
                                                     (Format.printf "%d\n") v))
                                               a) in
                                      fix v)) v)) x);
                     fix xs) in
              fix v)) l
