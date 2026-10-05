open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let%expect_test "frontmatter: the scalar forms skill files use" =
  let show text =
    let fields, body = Frontmatter.split text in
    print_s [%sexp (fields : Frontmatter.t), (body : string)]
  in
  show
    {|---
name: plain
description: Guidance for distinctive, intentional visual design: typography.
license: Complete terms in LICENSE.txt
---

# Body
|};
  [%expect
    {|
    (((name plain)
      (description
       "Guidance for distinctive, intentional visual design: typography.")
      (license "Complete terms in LICENSE.txt"))
      "\
     \n# Body\
     \n")
    |}];
  show
    "---\r\n\
     name: \"quoted \\\"double\\\"\"\r\n\
     description: 'single, it''s quoted'\r\n\
     ---\r\n\
     body";
  [%expect
    {|
    (((name "quoted \"double\"") (description "single, it's quoted")) body)
    |}];
  show
    {|---
# a comment
description: a plain scalar
  continued on the next line
folded: >
  one
  paragraph

  two
literal: |-
  line one
    indented
metadata:
  author: someone
  version: 1
disable-model-invocation: true
...
after|};
  [%expect
    {|
    (((description "a plain scalar continued on the next line")
      (folded  "one paragraph\
              \ntwo")
      (literal  "line one\
               \n  indented")
      (metadata  "author: someone\
                \nversion: 1")
      (disable-model-invocation true))
     after)
    |}];
  (* No frontmatter, or an unterminated one: all body. *)
  show "# just markdown\n";
  show "---\nname: x\nno end";
  [%expect
    {|
    (() "# just markdown\n")
    (()  "---\
        \nname: x\
        \nno end")
    |}]
;;

let skill_md ?name ?(extra = "") description =
  sprintf
    "---\n%sdescription: %s\n%s---\nDo the %s thing.\n"
    (Option.value_map name ~default:"" ~f:(sprintf "name: %s\n"))
    description
    extra
    description
;;

let%expect_test "discovery: roots, nesting, precedence, invalid files" =
  with_sandbox
  @@ fun t ->
  let home = Filename.concat t.dir "home" in
  let cwd = Filename.concat t.dir "proj/sub" in
  Core_unix.mkdir_p cwd;
  Core_unix.mkdir_p home;
  let roots = Skill.roots ~cwd ~home in
  print_s [%sexp (List.map (List.take roots 6) ~f:(mask t) : string list)];
  print_s
    [%sexp
      (List.map (List.drop roots (List.length roots - 3)) ~f:(mask t)
       : string list)];
  [%expect
    {|
    ($DIR/proj/sub/.prigh/skills $DIR/proj/sub/.claude/skills
     $DIR/proj/sub/.agents/skills $DIR/proj/.prigh/skills
     $DIR/proj/.claude/skills $DIR/proj/.agents/skills)
    ($DIR/home/.prigh/skills $DIR/home/.claude/skills $DIR/home/.agents/skills)
    |}];
  write t "home/.prigh/skills/review/SKILL.md" (skill_md "user review");
  write
    t
    "home/.claude/skills/synced/abc/pdf/SKILL.md"
    (skill_md "pdf handling");
  write t "proj/.claude/skills/review/SKILL.md" (skill_md "project review");
  write
    t
    "proj/sub/.agents/skills/dir-name/SKILL.md"
    (skill_md ~name:"named in file" "renamed");
  write
    t
    "proj/.prigh/skills/manual/SKILL.md"
    (skill_md ~extra:"disable-model-invocation: true\n" "only by hand");
  write
    t
    "proj/.prigh/skills/broken/SKILL.md"
    "---\nname: broken\n---\nno description\n";
  write t "proj/.prigh/skills/.hidden/SKILL.md" (skill_md "hidden");
  write t "proj/.prigh/skills/too/deep/to/find/SKILL.md" (skill_md "deep");
  let skills = Skill.discover ~cwd ~home in
  List.iter skills ~f:(fun s ->
    print_endline (mask_sexp t [%sexp (s : Skill.t)]));
  [%expect
    {|
    ((name manual) (description "only by hand")
     (path $DIR/proj/.prigh/skills/manual/SKILL.md) (model_invocable false))
    ((name named-in-file) (description renamed)
     (path $DIR/proj/sub/.agents/skills/dir-name/SKILL.md)
     (model_invocable true))
    ((name pdf) (description "pdf handling")
     (path $DIR/home/.claude/skills/synced/abc/pdf/SKILL.md)
     (model_invocable true))
    ((name review) (description "project review")
     (path $DIR/proj/.claude/skills/review/SKILL.md) (model_invocable true))
    |}];
  print_endline (Option.value_exn (Skill.prompt_section skills) |> mask t);
  [%expect
    {|
    Skills hold instructions for particular tasks. When a task matches a skill's description, read its file with read before starting, and follow it; paths in it are relative to its directory.
    - named-in-file ($DIR/proj/sub/.agents/skills/dir-name/SKILL.md): renamed
    - pdf ($DIR/home/.claude/skills/synced/abc/pdf/SKILL.md): pdf handling
    - review ($DIR/proj/.claude/skills/review/SKILL.md): project review
    |}];
  print_s [%sexp (Skill.prompt_section [] : string option)];
  [%expect {| () |}]
;;

let%expect_test "invocation and expansion" =
  let show text =
    print_s [%sexp (Skill.invocation text : (string * string) option)]
  in
  show "/skill:review the diff please";
  show "/skill:review";
  show "/skill:review\nmulti\nline";
  show "  /skill:x  ";
  show "/skill:";
  show "/skills";
  show "please /skill:review";
  [%expect
    {|
    ((review "the diff please"))
    ((review ""))
    ((review  "multi\
             \nline"))
    ((x ""))
    ()
    ()
    ()
    |}];
  let skill =
    { Skill.name = "review"
    ; description = "d"
    ; path = "/p/.prigh/skills/review/SKILL.md"
    ; model_invocable = true
    }
  in
  print_endline (Skill.expand skill ~body:"\nDo the thing.\n" ~args:"src/a.ml");
  [%expect
    {|
    <skill name="review" location="/p/.prigh/skills/review/SKILL.md">
    References are relative to /p/.prigh/skills/review.

    Do the thing.
    </skill>

    src/a.ml
    |}];
  print_endline (Skill.expand skill ~body:"Do the thing." ~args:"");
  [%expect
    {|
    <skill name="review" location="/p/.prigh/skills/review/SKILL.md">
    References are relative to /p/.prigh/skills/review.

    Do the thing.
    </skill>
    |}];
  List.iter
    [ Skill.expand skill ~body:"Do the thing." ~args:"src/a.ml\nand more"
    ; Skill.expand skill ~body:"Do\n</skill>\n" ~args:""
    ; "plain text"
    ]
    ~f:(fun text -> print_endline (Skill.as_typed text));
  [%expect
    {|
    /skill:review src/a.ml
    and more
    /skill:review
    plain text
    |}];
  print_endline (Error.to_string_hum (Skill.unknown [ skill ] "reveiw"));
  print_endline (Error.to_string_hum (Skill.unknown [] "reveiw"));
  [%expect
    {|
    unknown skill "reveiw"; did you mean: review (/skills lists them all)
    unknown skill "reveiw": there are none here; add one as .prigh/skills/<name>/SKILL.md (or .claude/skills/) in the project or ~/.prigh
    |}]
;;

let%expect_test "the system prompt lists skills only with the read tool" =
  let skill =
    { Skill.name = "review"
    ; description = "reviews"
    ; path = "/s/SKILL.md"
    ; model_invocable = true
    }
  in
  let build tools =
    System_prompt.build
      ~date:"2026-01-02"
      ~instructions:[]
      ~skills:[ skill ]
      ~cwd:"/p"
      ~home:"/h"
      ~tools:(Tools.specs tools)
      ()
  in
  print_endline (build [ Tool_read.tool ]);
  [%expect
    {|
    You are prigh, a coding agent working in the user's project from the command line.

    Guidelines:
    - Use the tools to inspect and change the project; do not guess file contents.
    - Prefer edit over write for existing files. Keep changes minimal and focused.
    - After making changes, verify them (build, tests) when a way to do so exists.
    - Be concise. Explain non-trivial decisions briefly. No filler.
    - Ask before destructive or irreversible actions.

    Available tools:
    - read: Read a text or image file. Returns the content; large text files are truncated a

    Skills hold instructions for particular tasks. When a task matches a skill's description, read its file with read before starting, and follow it; paths in it are relative to its directory.
    - review (/s/SKILL.md): reviews

    Environment:
    - Working directory: /p
    - Date: 2026-01-02
    - OS: Linux
    |}];
  print_s
    [%sexp
      (String.is_substring (build [ Tool_ls.tool ]) ~substring:"review" : bool)];
  [%expect {| false |}]
;;

let with_agent replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let requests = Queue.create () in
  let provider =
    Faux_provider.create ~on_request:(Queue.enqueue requests) replies
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:(Filename.concat t.dir "home")
      ~cwd:t.dir
      ()
  in
  f t agent requests
;;

let last_user_text (request : Provider.Request.t) =
  List.find_map (List.rev request.messages) ~f:(function
    | Message.User u -> Some u.text
    | Assistant _ | Tool_result _ -> None)
  |> Option.value ~default:"(none)"
;;

let%expect_test "agent: /skill:NAME expands on the host; unknown names fail" =
  with_agent [ Reply.text "ok"; Reply.text "ok"; Reply.text "ok" ]
  @@ fun t agent requests ->
  write t ".prigh/skills/review/SKILL.md" (skill_md "careful review");
  write t "home/.prigh/skills/notes/SKILL.md" (skill_md "note taking");
  print_endline
    (mask_sexp t [%sexp (Agent.skills agent : Skill.t list Or_error.t)]);
  [%expect
    {|
    (Ok
     (((name notes) (description "note taking")
       (path $DIR/home/.prigh/skills/notes/SKILL.md) (model_invocable true))
      ((name review) (description "careful review")
       (path $DIR/.prigh/skills/review/SKILL.md) (model_invocable true))))
    |}];
  print_s
    [%sexp (Agent.prompt agent "/skill:reveiw the diff" : unit Or_error.t)];
  [%expect
    {|
    (Error
     "unknown skill \"reveiw\"; did you mean: review, notes (/skills lists them all)")
    |}];
  Or_error.ok_exn (Agent.prompt agent "/skill:review the diff");
  Agent.wait_idle agent;
  let request = Queue.dequeue_exn requests in
  print_endline (mask t (last_user_text request));
  [%expect
    {|
    <skill name="review" location="$DIR/.prigh/skills/review/SKILL.md">
    References are relative to $DIR/.prigh/skills/review.

    Do the careful review thing.
    </skill>

    the diff
    |}];
  (* The model is told about them, and the session keeps the expansion. *)
  let system = Option.value_exn request.system in
  print_endline
    (mask
       t
       (String.concat
          ~sep:"\n"
          (List.filter (String.split_lines system) ~f:(fun line ->
             String.is_substring line ~substring:"SKILL.md"))));
  [%expect
    {|
    - notes ($DIR/home/.prigh/skills/notes/SKILL.md): note taking
    - review ($DIR/.prigh/skills/review/SKILL.md): careful review
    |}];
  (* Steering keeps the typed text in the queue and sends the expansion. *)
  print_s [%sexp (Agent.follow_up agent "/skill:nope" : unit Or_error.t)];
  [%expect
    {|
    (Error
     "unknown skill \"nope\"; did you mean: notes, review (/skills lists them all)")
    |}];
  Or_error.ok_exn (Agent.follow_up agent "/skill:notes");
  Agent.wait_idle agent;
  print_endline (mask t (last_user_text (Queue.dequeue_exn requests)));
  [%expect
    {|
    <skill name="notes" location="$DIR/home/.prigh/skills/notes/SKILL.md">
    References are relative to $DIR/home/.prigh/skills/notes.

    Do the note taking thing.
    </skill>
    |}];
  (* Other texts are untouched. *)
  Or_error.ok_exn (Agent.prompt agent "/skills is not an invocation");
  Agent.wait_idle agent;
  print_endline (last_user_text (Queue.dequeue_exn requests));
  [%expect {| /skills is not an invocation |}]
;;
