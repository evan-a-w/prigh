# The tools users install with `nix profile install` (DOCKER.md, "Installing
# tools"), on PATH in every shell: /etc/profile resets PATH for login shells
# (the terminal panel's, `bash -l`), and `docker exec` starts with the image's.
# Installed as /etc/profile.d/nix-path.sh and sourced by /etc/bash.bashrc.
case ":$PATH:" in
*":$HOME/.local/state/nix/profiles/profile/bin:"*) ;;
*) export PATH="$HOME/.local/state/nix/profiles/profile/bin:$PATH" ;;
esac
