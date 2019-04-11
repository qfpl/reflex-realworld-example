#! /usr/bin/env nix-shell
#! nix-shell -p nix-prefetch-git -i bash
HERE=$(dirname $0)
nix-prefetch-git https://github.com/imalsogreg/servant-reflex.git $1 > $HERE/git.json
