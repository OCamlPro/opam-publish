(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamStd.Op

let reset_terminal : (unit -> unit) option ref = ref None
let cleanup () =
  match !reset_terminal with None -> () | Some f -> f ()

let descr_template =
  OpamFile.Descr.create "Short description\n\nLong\ndescription\n"

let has_dotopam =
  OpamFormatConfig.init ();
  let root = OpamStateConfig.opamroot () in
  let has_dotopam = OpamStateConfig.load_defaults root in
  OpamStd.Config.init ();
  OpamRepositoryConfig.init ();
  OpamStateConfig.init ();
  has_dotopam

let opam_root = OpamStateConfig.(!r.root_dir)
let allow_checks_bypass =
  OpamStd.Option.Op.(OpamStd.Config.env_bool "PUBLISHBYPASSCHECKS" +! false)

(* -- Metadata checkup functions -- *)

type lint = int * [`Error|`Warning] * string
type check_result = Pass | Fail of lint list | Warnings of lint list

let mkwarn () =
  let warnings = ref ([]: (int * [`Error|`Warning] * string) list) in
  (fun n level s -> warnings := (n,level,s)::!warnings),
  (fun file -> match !warnings with
     | [] -> Pass
     | w ->
       OpamConsole.error "In %s:\n%s\n" (OpamFile.to_string file)
         (OpamFileTools.warns_to_string w);
       if List.exists (function _,`Error,_ -> true | _ -> false) w
       then Fail w else Warnings w)

let check_opam file =
  let module OF = OpamFile.OPAM in
  let warn, warnings = mkwarn () in
  try
    let errs, opam_opt = OpamFileTools.lint_file file in
    List.iter (fun (n,l,s) -> warn n l s) errs;
    if OpamStd.Option.Op.(opam_opt >>| fun o ->
                          OF.name_opt o <> None || OF.version_opt o <> None)
       = Some true
    then
      warn 99 `Warning "should not contain 'name' or 'version' fields";
    if OpamStd.Option.Op.(opam_opt >>| fun o ->
                          OF.url o <> None || OF.descr o <> None)
       = Some true
    then
      warn 100 `Error "don't include url{} or descr{} sections just yet";
    warnings file
  with
  | e ->
    OpamStd.Exn.fatal e;
    OpamConsole.error "Couldn't read %s (%s)" (OpamFile.to_string file)
      (Printexc.to_string e);
    Fail []

let check_descr file =
  let module OF = OpamFile.Descr in
  try
    let descr = OF.read file in
    let warn, warnings = mkwarn () in
    if OF.synopsis descr = OF.synopsis descr_template ||
       OpamStd.String.strip (OF.synopsis descr) = "" then
      warn 98 `Error "short description unspecified";
    if OF.body descr = OF.body descr_template ||
       OpamStd.String.strip (OF.body descr) = "" then
      warn 97 `Warning "long description unspecified";
    warnings file
  with e ->
    OpamStd.Exn.fatal e;
    OpamConsole.error "Couldn't read %s" (OpamFile.to_string file);
    Fail []

let check_url file =
  let module OF = OpamFile.URL in
  try
    let url = OF.read file in
    let warn, warnings = mkwarn () in
    let checksum = OF.checksum url in
    if checksum = [] then warn 96 `Warning "no checksum supplied";
    let check_url url =
      if url.OpamUrl.backend <> `http then
        warn 95 `Error @@
        Printf.sprintf "%s is not a regular http or ftp address"
          (OpamUrl.to_string url)
      else
        OpamFilename.with_tmp_dir @@ fun tmpdir ->
        let name = OpamUrl.basename url in
        let file = OpamFilename.Op.(tmpdir // name) in
        let archive =
          OpamProcess.Job.run
            (OpamRepository.pull_file name file [] [url])
        in
        match archive with
        | Not_available s ->
          warn 94 `Error @@
          Printf.sprintf "%s couldn't be fetched (%s)"
            (OpamUrl.to_string url)
            s
        | Result () ->
          if checksum <> [] &&
             not @@ List.for_all
               (OpamHash.check_file (OpamFilename.to_string file)) checksum
          then
            warn 93 `Error @@
            Printf.sprintf "bad checksum for %s"
              (OpamUrl.to_string url)
        | _ -> assert false
    in
    List.iter check_url (OF.url url :: OF.mirrors url);
    warnings file
  with e ->
    OpamStd.Exn.fatal e;
    OpamConsole.error "Couldn't read %s" (OpamFile.to_string file);
    Fail []


(* -- Submit command -- *)

let (/) a b = String.concat "/" [a;b]

let git cmds = OpamSystem.command ("git" :: cmds)

let github_root = "git@github.com:"

type github_repo = { label: string; owner: string; name: string; }

let default_label = "default"

let default_repo =
  { label = default_label; owner = "ocaml"; name = "opam-repository"; }

let opam_publish_root =
  OpamFilename.Op.( opam_root / "plugins" / "opam-publish" )

let create_opam_publish_root () =
  try OpamFilename.mkdir opam_publish_root with
  Unix.Unix_error (c, _, _) ->
      OpamConsole.warning "Error %s while creating opam publishing root"
        (Unix.error_message c)

let repo_dir label =
  OpamFilename.Op.(opam_publish_root / "repos" / label)

let user_branch package =
  "opam-publish" /
  String.map (function
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.' | '_' as c -> c
      | _ -> '-'
    ) (OpamPackage.to_string package)

let repo_of_dir dir =
  let label = OpamFilename.Base.to_string (OpamFilename.basename_dir dir) in
  let remote =
    OpamFilename.in_dir dir (fun () ->
        OpamSystem.read_command_output ~verbose:false
          ["git"; "config"; "--get"; "remote.origin.url"]
        |> List.hd)
  in
  Scanf.sscanf remote "git@github.com:%s@/%s@."
    (fun owner name -> { label; owner; name })

let user_of_dir dir =
  let remote =
    OpamFilename.in_dir dir (fun () ->
        OpamSystem.read_command_output ~verbose:false
          ["git"; "config"; "--get"; "remote.user.url"]
        |> List.hd)
  in
  Scanf.sscanf remote "git@github.com:%s@/%s"
    (fun owner _ -> owner)

let get_user repo user_opt =
  let dir = repo_dir repo.label in
  match user_opt with
  | Some u ->
    if OpamFilename.exists_dir dir && user_of_dir dir <> u then
      OpamConsole.error_and_exit
        "Repo %s already registered with GitHub user %s"
        repo.label u
    else u
  | None ->
    if OpamFilename.exists_dir dir then user_of_dir dir else
    let rec get_u () =
      OpamConsole.msg
        "A GitHub account is currently required to directly contribute to \
         the official opam repository.\n\
         If you don't have a GitHub account, you can create one at \
         https://github.com/join\n\n";
      match OpamConsole.read "Please enter your GitHub name:" with
      | None -> get_u ()
      | Some u -> u
    in
    get_u ()

module GH = struct
  open Lwt
  open Github

  let token_note hostname = "opam-publish access token ("^hostname^")"

  let no_stdin_echo f =
    let open Unix in
    let attr = tcgetattr stdin in
    let reset () = tcsetattr stdin TCSAFLUSH attr in
    reset_terminal := Some reset;
    tcsetattr stdin TCSAFLUSH
      { attr with
        c_echo = false; c_echoe = false; c_echok = false; c_echonl = true; };
    let v = f () in
    reset ();
    reset_terminal := None;
    v

  let recent_otp = ref None
  let complete_2fa user c =
    let rec try_again f = Monad.(f () >>~ function
    | Result auths -> return auths
    | Two_factor _ when !recent_otp <> None ->
      recent_otp := None;
      try_again f
    | Two_factor mode ->
      let otp = OpamConsole.read "%s 2FA code from '%s':" user mode in
      recent_otp := otp;
      try_again (c ?otp)
    ) in
    let otp = !recent_otp in
    try_again (c ?otp)

  let is_valid token = Lwt_main.run @@ Monad.(
    Lwt.catch (fun () -> run (
      User.current_info ~token ()
      >>~ fun _ -> return true
    )) (function
      | Message (`Unauthorized, _) -> Lwt.return false
      | exn -> Lwt.fail exn
    )
  )

  let rec get_token user =
    let tok_file = OpamFilename.Op.(opam_publish_root // (user ^ ".token")) in
    if OpamFilename.exists tok_file
    then
      let token = Token.of_string (OpamFilename.read tok_file) in
      if is_valid token
      then token
      else begin
        OpamConsole.msg "Existing token is no longer valid.\n\n";
        OpamFilename.remove tok_file;
        get_token user
      end
    else
    let hostname = Unix.gethostname () in
    let token_note = token_note hostname in
    let pass =
      OpamConsole.msg
        "Please enter your GitHub password.\n\
         It will be used to generate an auth token that will be stored \
         for subsequent \n\
         runs in %s.\n\
         Your active tokens can be seen and revoked at \
         https://github.com/settings/tokens\n\n"
        (OpamFilename.prettify tok_file);
      let rec get_pass () =
        match OpamConsole.read "%s password:" user with
        | Some p -> p
        | None -> get_pass ()
      in
      no_stdin_echo get_pass
    in
    let open Github.Monad in
    let create_token () =
      complete_2fa user
        (fun ?otp () ->
           Token.create ~scopes:[`Repo] ~user ~pass ~note:token_note ?otp ()
        )
    in
    let token =
      Lwt_main.run @@ Monad.run @@
      (complete_2fa user (Token.get_all ~user ~pass)
       >>= fun auths ->
       (try
          let auth = List.find (fun a ->
            a.Github_t.auth_note = Some token_note)
            auths
          in
          OpamConsole.msg "Remote token for %s already exists. Resetting.\n\n"
            hostname;
          complete_2fa user (Token.delete ~user ~pass ~id:auth.Github_t.auth_id)
          >>= fun () ->
          create_token ()
        with Not_found -> create_token ()
       )
       >>= fun auth ->
       Token.of_auth auth |> Monad.return)
    in
    let tok_file = OpamFilename.to_string tok_file in
    create_opam_publish_root ();
    let tok_fd = Unix.(openfile tok_file [O_CREAT; O_TRUNC; O_WRONLY] 0o600) in
    let tok_oc = Unix.out_channel_of_descr tok_fd in
    output_string tok_oc (Token.to_string token);
    close_out tok_oc;
    let { Unix.st_perm } = Unix.stat tok_file in
    let safe_perm = 0o7770 land st_perm in
    begin if safe_perm <> st_perm
      then Unix.chmod tok_file safe_perm
    end;
    token

  let fork token repo =
    let check uri =
      let not_found = API.code_handler ~expected_code:`Not_found (fun _ ->
        OpamConsole.log "PUBLISH" "Check for fork failed: not found";
        return_false
      ) in
      API.get ~fail_handlers:[not_found] ~expected_code:`OK ~token ~uri
        (fun _ -> return_true)
    in
    let rec until ?(n=0) f x () = Monad.(
      f x >>~ function
      | true ->
        if n > 0 then OpamConsole.msg "\n";
        return ()
      | false ->
        if n=0 then
          OpamConsole.msg "Waiting for GitHub to register the fork..."
        else if n<20 then
          OpamConsole.msg "."
        else
          failwith "GitHub fork timeout";
        embed (Lwt_unix.sleep 1.5) >>= until ~n:(n+1) f x
    ) in
    Lwt_main.run Monad.(run (
      Repo.fork ~token ~user:repo.owner ~repo:repo.name ()
      >>~ fun { Github_t.repository_url = uri } ->
      until check (Uri.of_string uri) ()
    ))

  let pull_request title user token repo ?text package =
    (* let repo = gh_repo.owner/gh_repo.name in *)
    let title = match title with
      | None | Some "" -> OpamPackage.to_string package ^ " — via opam-publish"
      | Some t -> t in
    let pull = {
      Github_t.
      new_pull_title = title;
      new_pull_base = "master";
      new_pull_head = user^":"^user_branch package;
      new_pull_body = text;
    } in
    let update_pull = {
      Github_t.
      update_pull_title = Some pull.Github_t.new_pull_title;
      update_pull_body = pull.Github_t.new_pull_body;
      update_pull_state = None;
      update_pull_base = None;
    } in
    let open Github.Monad in
    let existing () =
      let pulls = Pull.for_repo ~token ~user:repo.owner ~repo:repo.name () in
      Stream.find Github_t.(fun p ->
        (match p.pull_head.branch_user with
         | None -> false | Some u -> u.user_login = user) &&
        p.pull_head.branch_ref = user_branch package &&
        p.pull_state = `Open
      ) pulls
    in
    let pr =
      Response.value @@ Lwt_main.run @@ Monad.run @@
      (existing () >>= function
        | None ->
          Pull.create ~token ~user:repo.owner ~repo:repo.name ~pull ()
        | Some (p,_) ->
          let num = p.Github_t.pull_number in
          OpamConsole.msg "Updating existing pull-request #%d\n" num;
          Pull.update
            ~token ~user:repo.owner ~repo:repo.name ~update_pull ~num
            ())
    in
    pr.Github_t.pull_html_url

end



let init_mirror repo user token =
  let dir = repo_dir repo.label in
  OpamFilename.mkdir dir;
  git ["clone"; github_root^repo.owner/repo.name^".git";
       OpamFilename.Dir.to_string dir];
  GH.fork token repo;
  OpamFilename.in_dir dir (fun () ->
      git ["remote"; "add"; "user"; github_root^user/repo.name]
    )

let update_mirror repo =
  OpamFilename.in_dir (repo_dir repo.label) (fun () ->
      git ["fetch"; "--multiple"; "origin"; "user"];
      git ["reset"; "origin/master"; "--hard"];
    )

let repo_package_dir package =
  OpamFilename.Op.(
    OpamFilename.Dir.of_string "packages" /
    OpamPackage.Name.to_string (OpamPackage.name package) /
    OpamPackage.to_string package
  )

let add_metadata ?msg repo user token package title lint user_meta_dir =
  let mirror = repo_dir repo.label in
  let opam,descr =
    OpamFilename.in_dir mirror @@ fun () ->
    let meta_dir = repo_package_dir package in
    if OpamFilename.exists_dir meta_dir then
      git ["rm"; "-r"; OpamFilename.Dir.to_string meta_dir];
    OpamFilename.mkdir (OpamFilename.dirname_dir meta_dir);
    OpamFilename.copy_dir
      ~src:user_meta_dir
      ~dst:meta_dir;
    let setmode f mode =
      let file = OpamFilename.Op.(meta_dir // f) in
      if OpamFilename.exists file then OpamFilename.chmod file mode;
    in
    setmode "opam" 0o644;
    setmode "descr" 0o644;
    let () =
      let dir = OpamFilename.Op.(meta_dir / "files") in
      if OpamFilename.exists_dir dir then
        Unix.chmod (OpamFilename.Dir.to_string dir) 0o755
    in
    git ["add"; OpamFilename.Dir.to_string meta_dir];
    git ["commit"; "-m";
         Printf.sprintf "%s - via opam-publish"
           (OpamPackage.to_string package)];
    git ["push"; "user"; "+HEAD:"^user_branch package];
    OpamFile.OPAM.read (OpamFile.make OpamFilename.Op.(meta_dir // "opam")),
    OpamFile.Descr.read (OpamFile.make OpamFilename.Op.(meta_dir // "descr"))
  in
  let lint_text = match lint with
    | Pass -> ""
    | Warnings w | Fail w ->
      Printf.sprintf "### opam-lint failures\n%s\n---\n"
        (String.concat "" (List.map (function
             | n, `Warning, s -> Printf.sprintf "- **WARNING** %2d %s\n" n s
             | n, `Error, s -> Printf.sprintf "- **ERROR** %2d %s\n" n s)
           w))
  in
  let msg = match msg with
    | None   -> ""
    | Some m ->
      let str = OpamSystem.read m in
      Printf.sprintf "\n---\n%s\n" str
  in
  let text =
    Printf.sprintf
      "%s\n\
       \n---\n\
       * Homepage: %s\n\
       * Source repo: %s\n\
       * Bug tracker: %s\n\
       \n---\n\
       %s\n\
       %s\
       Pull-request generated by opam-publish v%s"
    (OpamFile.Descr.full descr)
    (String.concat " " (OpamFile.OPAM.homepage opam))
    OpamStd.Option.Op.((OpamFile.OPAM.dev_repo opam >>|
                        OpamUrl.to_string) +! "")
    (String.concat " " (OpamFile.OPAM.bug_reports opam))
    lint_text msg
    Version.version
  in
  let url =
    GH.pull_request title user token repo ~text package
  in
  OpamConsole.msg "Pull-requested: %s\n" url;
  try
    let auto_open =
      if OpamStd.Sys.(os () = Darwin) then "open" else "xdg-open"
    in
    OpamSystem.command [auto_open; url]
  with OpamSystem.Command_not_found _ -> ()

let reset_to_existing_pr package repo =
  let mirror = repo_dir repo.label in
  OpamFilename.in_dir mirror @@ fun () ->
  try git ["reset"; "--hard"; "remotes"/"user"/user_branch package; "--"]; true
  with OpamSystem.Process_error _ -> false

let get_git_user_dir package repo =
  let mirror = repo_dir repo.label in
  OpamFilename.in_dir mirror @@ fun () ->
  let meta_dir = repo_package_dir package in
  if OpamFilename.exists_dir meta_dir then Some meta_dir
  else None

let get_git_max_v_dir package repo =
  let mirror = repo_dir repo.label in
  OpamFilename.in_dir mirror @@ fun () ->
  let meta_dir = repo_package_dir package in
  let parent = OpamFilename.dirname_dir meta_dir in
  if OpamFilename.exists_dir parent then
    let packages =
      OpamStd.List.filter_map
        (OpamPackage.of_string_opt @*
         OpamFilename.Base.to_string @* OpamFilename.basename_dir)
        (OpamFilename.dirs parent)
    in
    try
      let max =
        OpamPackage.max_version (OpamPackage.Set.of_list packages)
          (OpamPackage.name package)
      in
      Some (repo_package_dir max)
    with Not_found -> None
  else None

let sanity_checks meta_dir =
  let files = OpamFilename.files meta_dir in
  let dirs = OpamFilename.dirs meta_dir in
  let warns =
    files |> List.fold_left (fun warns f ->
        match OpamFilename.Base.to_string (OpamFilename.basename f) with
        | "opam" | "descr" | "url" -> warns
        | f -> (92, `Warning, Printf.sprintf "extra file %S" f) :: warns
      ) []
  in
  let warns =
    dirs |> List.fold_left (fun warns d ->
        match OpamFilename.Base.to_string (OpamFilename.basename_dir d) with
        | "files" -> warns
        | d -> (91, `Warning, Printf.sprintf "extra dir %S" d) :: warns
      ) warns
  in
  if warns <> [] then
    OpamConsole.error "Bad contents in %s:\n%s\n"
      (OpamFilename.Dir.to_string meta_dir)
      (OpamFileTools.warns_to_string warns);
  let ( * ) a b =
    let warns = function Fail w | Warnings w -> w | Pass -> [] in
    match (a,b) with
    | Fail _, _ | _, Fail _ -> Fail (warns a @ warns b)
    | Warnings _, _ | _, Warnings _ -> Warnings (warns a @ warns b)
    | Pass, Pass -> Pass
  in
  (if warns = [] then Pass
   else if List.exists (function _,`Error,_ -> true | _ -> false) warns
   then Fail warns else Warnings warns)
  * check_opam (OpamFile.make OpamFilename.Op.(meta_dir // "opam"))
  * check_url (OpamFile.make OpamFilename.Op.(meta_dir // "url"))
  * check_descr (OpamFile.make OpamFilename.Op.(meta_dir // "descr"))

let submit ?msg repo_label user_opt package title meta_dir =
  let check = sanity_checks meta_dir in
  let pass = match check with
    | Pass -> true
    | Warnings _ ->
      OpamConsole.confirm "Go on submitting, ignoring the warnings ?"
    | Fail _ when allow_checks_bypass ->
      OpamConsole.confirm "Submit, bypassing checks ?"
    | Fail _ ->
      OpamConsole.msg "Please correct the above errors and retry\n";
      false
  in
  if not pass then OpamConsole.msg "Aborting\n"
  else
  (* Prepare the repo *)
  let mirror_dir = repo_dir repo_label in
  let user, repo, token =
    if not (OpamFilename.exists_dir mirror_dir) then
      if repo_label = default_label then
        let user = get_user default_repo user_opt in
        let token = GH.get_token user in
        init_mirror default_repo user token;
        user, default_repo, token
      else
        OpamConsole.error_and_exit
          "Repository %S unknown, see `opam-publish repo'"
          repo_label
    else
    let repo = repo_of_dir mirror_dir in
    let user = get_user repo user_opt in
    let token = GH.get_token user in
    user, repo, token
  in
  (* pull-request processing *)
  update_mirror repo;
  add_metadata ?msg repo user token package title check meta_dir


(* -- Prepare command -- *)

let prepare ?name ?version ?(repo_label=default_label) http_url =
  let open OpamFilename.Op in
  let open OpamStd.Option.Op in (* Option monad *)
  OpamFilename.with_tmp_dir @@ fun tmpdir ->
  (* Fetch the archive *)
  let url = OpamUrl.parse ~handle_suffix:true http_url in
  let urlname = OpamUrl.basename url in
  let file = OpamFilename.Op.(tmpdir // urlname) in
  let f =
    OpamProcess.Job.run
      (OpamRepository.pull_file urlname file [] [url])
  in
  let archive = match f with
    | Not_available s ->
      OpamConsole.error_and_exit "Could not download the archive at %s" http_url
    | Result () -> file
    | _ -> assert false
  in
  let checksum = OpamHash.compute (OpamFilename.to_string archive) in
  let srcdir = tmpdir / "src" in
  OpamFilename.extract archive srcdir;
  (* Utility functions *)
  let f_opt f = if OpamFilename.exists f then Some f else None in
  let dir_opt d = if OpamFilename.exists_dir d then Some d else None in
  let get_file name reader dir =
    dir >>= dir_opt >>= fun d ->
    f_opt (d // name) >>= fun f ->
    try Some (f, reader (OpamFile.make f))
    with OpamPp.Bad_format _ -> None
  in
  let get_opam = get_file "opam" OpamFile.OPAM.read in
  let get_descr dir =
    get_file "descr" OpamFile.Descr.read dir >>= fun (_,d as descr) ->
    if OpamFile.Descr.synopsis d = OpamFile.Descr.synopsis descr_template
    then None else Some descr
  in
  let get_files_dir dir = dir >>= dir_opt >>= fun d -> dir_opt (d / "files") in
  (* Get opam from the archive *)
  let src_meta_dir = dir_opt (srcdir / "opam") ++ dir_opt srcdir in
  let src_opam = get_opam src_meta_dir in
  (* Guess package name and version *)
  let name = match name, src_opam >>| snd >>= OpamFile.OPAM.name_opt with
    | None, None ->
      OpamConsole.error_and_exit "Package name unspecified"
    | Some n1, Some n2 when n1 <> n2 ->
      OpamConsole.warning
        "Publishing as package %s, while it refers to itself as %s"
        (OpamPackage.Name.to_string n1) (OpamPackage.Name.to_string n2);
      n1
    | Some n, _ | None, Some n -> n
  in
  let version =
    match version ++ (src_opam >>| snd >>= OpamFile.OPAM.version_opt) with
    | None ->
      OpamConsole.error_and_exit "Package version unspecified"
    | Some v -> v
  in
  let package = OpamPackage.create name version in
  (* Metadata sources: from OPAM overlay, prepare dir, git mirror, archive.
     Could add: from highest existing version on the repo ? Better
     advise pinning at the moment to encourage some testing. *)
  let prepare_dir_name = OpamFilename.cwd () / OpamPackage.to_string package in
  let prepare_dir = dir_opt prepare_dir_name in
  let overlay_dir =
    if has_dotopam <> None then
      OpamStateConfig.(!r.current_switch) >>| fun switch ->
      OpamPath.Switch.Overlay.package opam_root switch name
    else
      None
  in
  let repo = dir_opt (repo_dir repo_label) >>| repo_of_dir in
  (repo >>| update_mirror) +! ();
  let has_pr = (repo >>| reset_to_existing_pr package) +! false in
  let pub_dir = repo >>= get_git_user_dir package in
  let other_versions_pub_dir =
    if has_pr then None else repo >>= get_git_max_v_dir package
  in
  (* Choose metadata from the sources *)
  let prep_url =
    (* Todo: advise mirrors if existing in other versions ? *)
    OpamFile.URL.with_checksum [checksum] (OpamFile.URL.create url)
  in
  let chosen_opam_and_files =
    let get_opam_and_files dir =
      get_opam dir >>| fun o -> o, get_files_dir dir
    in
    get_opam_and_files overlay_dir  ++
    get_opam_and_files prepare_dir ++
    get_opam_and_files pub_dir ++
    get_opam_and_files src_meta_dir
  in
  let chosen_descr =
    get_descr overlay_dir ++
    get_descr prepare_dir ++
    get_descr pub_dir ++
    get_descr src_meta_dir ++
    get_descr other_versions_pub_dir
  in
  (* Choose and copy or write *)
  OpamFilename.mkdir prepare_dir_name;
  let prepare_dir = prepare_dir_name in
  match chosen_opam_and_files with
  | None ->
    OpamConsole.error_and_exit
      "No metadata found. \
       Try pinning the package locally (`opam pin add %s %S`) beforehand."
      (OpamPackage.Name.to_string name) http_url
  | Some ((opam_file, opam), files_opt) ->
    let open OpamFile in
    let opam =
      opam |>
      OPAM.with_name_opt None |>
      OPAM.with_version_opt None |>
      OPAM.with_url_opt None |>
      OPAM.with_descr_opt None
    in
    let opam_str =
      OPAM.to_string_with_preserved_format (OpamFile.make opam_file) opam
    in
    OpamFilename.write (prepare_dir // "opam") opam_str;
    (files_opt >>| fun src ->
     OpamFilename.copy_dir ~src ~dst:(prepare_dir / "files"))
    +! ();
    (match
       chosen_descr >>| fun (src, _descr) ->
       OpamFilename.copy ~src ~dst:(prepare_dir // "descr")
     with Some () -> ()
        | None ->
          OpamFile.Descr.write (OpamFile.make (prepare_dir // "descr"))
            descr_template);
    OpamFile.URL.write (OpamFile.make (prepare_dir // "url")) prep_url;
    (* Todo: add an option to get all the versions in prepare_dir and let
       the user merge *)

    OpamConsole.msg
      "Template metadata generated in %s/.\n\
      \  * Check the 'opam' file\n\
      \  * Fill in or check the description of your package in 'descr'\n\
      \  * Check that there are no unneeded files under 'files/'\n\
      \  * Run 'opam publish submit ./%s' to submit your package\n"
      (OpamPackage.to_string package)
      (OpamPackage.to_string package)


(* -- Command-line handling -- *)

open Cmdliner

(* name * version option *)
let package =
  let parse str =
    let name, version_opt =
      match OpamStd.String.cut_at str '.' with
      | None -> str, None
      | Some (n,v) -> n, Some v
    in
    try
      `Ok
        (OpamPackage.Name.of_string name,
         OpamStd.Option.map OpamPackage.Version.of_string version_opt)
    with Failure _ -> `Error (Printf.sprintf "bad package name %s" name)
  in
  let print ppf (name, version_opt) =
    match version_opt with
    | None -> Format.pp_print_string ppf (OpamPackage.Name.to_string name)
    | Some v -> Format.fprintf ppf "%s.%s"
                  (OpamPackage.Name.to_string name)
                  (OpamPackage.Version.to_string v)
  in
  parse, print

let github_user =
  Arg.(value & opt (some string) None & info ["n";"name"]
         ~docv:"NAME"
         ~doc:"GitHub user name. This can only be set during initialisation \
               of a repo")

let repo_name =
  Arg.(value & opt string default_label & info ["r";"repo"]
         ~docv:"NAME"
         ~doc:"Local name of the repository to use (see the $(b,repo) \
               subcommand)")

let latest_tag () =
  OpamSystem.read_command_output ~verbose:false
    ["git"; "describe"; "--tags"; "--abbrev=0"]
  |> List.hd

let guess_github_archive_url () =
  try
    let repo = repo_of_dir (OpamFilename.cwd ()) in
    let tag = latest_tag () in
    Some
      (Printf.sprintf "https://github.com/%s/%s/archive/%s.tar.gz"
         repo.owner repo.name tag)
  with _ ->  None

let prepare_cmd =
  let doc = "Provided a remote archive URL, gathers metadata for an OPAM \
             package suitable for editing and submitting to an OPAM repo. \
             A directory $(b,PACKAGE).$(b,VERSION) is generated, or updated \
             if it exists." in
  let url = Arg.(value & pos ~rev:true 0 (some string) None & info
                   ~doc:"Public URL hosting the package source archive \
                         (if unspecified, it may be guessed for latest tag \
                         of a Github-hosted repository in the current \
                         directory)"
                   ~docv:"URL" [])
  in
  let pkg_opt = Arg.(value & pos ~rev:true 1 (some package) None & info
                       ~docv:"PACKAGE"
                       ~doc:"Package to release, with optional version" [])
  in
  let prepare url pkg_opt repo_label =
    let url = match url with
      | None -> guess_github_archive_url ()
      | some -> some
    in
    match url with
    | None -> `Error (false, "Please specify an archive url")
    | Some url ->
      `Ok OpamStd.Option.Op.(
          prepare ?name:(pkg_opt >>| fst) ?version:(pkg_opt >>= snd) ~repo_label
            url)
  in
  Term.(ret (pure prepare $ url $ pkg_opt $ repo_name)),
  Term.info "prepare" ~doc

let repo_cmd =
  let doc = "Sets up aliases for repositories you want to submit to." in
  let command =
    Arg.(value &
         pos 0 (enum ["add", `Add; "remove", `Remove; "list", `List]) `List &
         info [] ~docv:"SUBCOMMAND"
           ~doc:"One of $(b,add), $(b,remove) or $(b,list). Defaults to \
                 $(b,list).")
  in
  let label =
    Arg.(value & pos 1 string default_label & info []
           ~docv:"NAME"
           ~doc:"Local name of the repository to use") in
  let gh_address =
    Arg.(value &
         pos 2 (some (pair ~sep:'/' string string)) None &
         info []
           ~docv:"USER/REPO_NAME"
           ~doc:"Address of the GitHub repo (github.com/USER/REPO_NAME)")
  in
  let repo command label gh_address user_opt =
    match command,gh_address with
    | `Add, Some (owner,name) ->
      if OpamFilename.exists_dir (repo_dir label) then
        `Error (false, "Repo "^label^" is already registered")
      else
      let repo = {label; owner; name} in
      let user = get_user repo user_opt in
      let token = GH.get_token user in
      `Ok (init_mirror repo user token)
    | `Add, _ -> `Error (true, "GitHub address or user unspecified")
    | `Remove, _ -> `Ok (OpamFilename.rmdir (repo_dir label))
    | `List, _ ->
      `Ok (
        OpamFilename.dirs OpamFilename.Op.(opam_publish_root/"repos")
        |> List.iter @@ fun dir ->
        let repo = repo_of_dir dir in
        Printf.printf "%-20s  %s/%s (%s)\n" (OpamConsole.colorise `bold repo.label)
          repo.owner repo.name (get_user repo None)
      );
  in
  Term.(ret (pure repo $ command $ label $ gh_address $ github_user)),
  Term.info "repo" ~doc

let guess_prepared_dir () =
  try
    let repo = repo_of_dir (OpamFilename.cwd ()) in
    let tag = latest_tag () in
    let name =
      try
        OpamPackage.Name.to_string @@
        OpamFile.OPAM.name @@ OpamFile.OPAM.read @@
        OpamFile.make @@
        List.find OpamFilename.exists @@
        List.map OpamFilename.of_string ["opam";"opam/opam"]
      with Not_found -> repo.name
    in
    let d = Printf.sprintf "%s.%s" name tag in
    if Sys.file_exists d then Some d else None
  with _ -> None

let submit_cmd =
  let doc = "submits or updates a pull-request to an OPAM repo." in
  let dir =
    Arg.(value & pos ~rev:true 0 (some string) None & info []
           ~docv:"DIR"
           ~doc:"Path to the metadata from opam-publish prepare")
  in
  let title =
    Arg.(value & opt (some string) None & info ["t"; "title"]
           ~docv:"TXT"
           ~doc:"Title of the pull request")
  in
  let msg =
    Arg.(value & opt (some string) None & info ["msg"]
           ~docv:"FILE"
           ~doc:"Message to be appended to the pull request's body, \
                 such as release notes.")
  in
  let submit user dir msg repo_name title =
    let dir = match dir with
      | None -> guess_prepared_dir ()
      | some -> some
    in
    match dir with
    | None -> `Error (false, "Please specify the output dir of \
                              'opam-publish prepare'")
    | Some dir ->
      `Ok (
        submit ?msg repo_name user
          (OpamPackage.of_string (Filename.basename dir))
          title
          (OpamFilename.Dir.of_string dir)
      )
  in
  Term.(ret (pure submit $ github_user $ dir $ msg $ repo_name $ title)),
  Term.info "submit" ~doc

let cmds = [prepare_cmd; submit_cmd; repo_cmd]

let help_cmd =
  let usage () =
    OpamConsole.msg "\
Opam-publish v.%s

Sub-commands:\n\
\      prepare URL   Prepares a local package definition directory from a\n\
\                    public URL pointing to a source archive.\n\
\      submit DIR    Submits or updates the request for integration of\n\
\                    the package defined by metadata at DIR.\n\
\      repo          Manage the repos you contribute to.\n\
\n\
See '%s COMMAND --help' for details on each command.\n\
"
      Version.version
      Sys.argv.(0)
  in
  Term.(pure usage $ pure ()),
  Term.info "opam-publish" ~version:(Version.version)

let () =
  at_exit cleanup;
  Sys.catch_break true;
  let _ = Sys.signal Sys.sigpipe (Sys.Signal_handle (fun _ -> ())) in
  try match Term.eval_choice ~catch:false help_cmd cmds with
    | `Error _ -> exit 1
    | _ -> exit 0
  with
  | OpamStd.Sys.Exit i as e ->
    if OpamConsole.debug () && i <> 0 then
      Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e);
    exit i
  | OpamSystem.Internal_error _
  | OpamSystem.Process_error _ as e ->
    Printf.eprintf "%s\n" (Printexc.to_string e);
    Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e);
  | Sys.Break ->
    exit 130
  | Failure msg as e ->
    Printf.eprintf "Fatal error: %s\n" msg;
    Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e);
    exit 1
  | Github.Message (code, m) ->
    Printf.eprintf "GitHub API error %s: %s\n"
      (Cohttp.Code.string_of_status code)
      (Github.API.string_of_message m);
    exit 1
  | e ->
    Printf.eprintf "Fatal error:\n%s\n" (Printexc.to_string e);
    Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e);
    exit 1
