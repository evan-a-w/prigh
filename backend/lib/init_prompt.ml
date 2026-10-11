open! Core

let prompt ~cwd =
  sprintf
    {|[prigh init] Write an AGENTS.md for the project in %s, or improve the one there.

First explore the repository: the README and other docs, the build files (package manifests, Makefile, dune, flake.nix, CI workflows), the source layout, the tests, and any AGENTS.md, CLAUDE.md, .cursorrules, .github/copilot-instructions.md or similar instruction files.

Then write AGENTS.md at the root of the repository (in %s if it is not in one), for coding agents working on this project. Keep it concise, at most about 150 lines, and specific to this project:
- how to build, run the tests (all of them and a single one), lint and format, with the exact commands;
- the layout: what lives where, in a few lines;
- conventions that are not obvious from the code: style, naming, error handling, how tests are written, commit messages;
- anything surprising: required tools or environment, generated files not to edit, slow or flaky steps.

Only write what you have verified in the repository; leave out generic advice. If AGENTS.md exists, keep what is still accurate, correct what is wrong and add what is missing rather than starting over; if only CLAUDE.md exists, build on it. When done, summarise what you wrote or changed.|}
    cwd
    cwd
;;
