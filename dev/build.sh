#!/bin/bash
set -euo pipefail

(cd avh4-lib && hpack)
(cd elm-format-markdown && hpack)
(cd elm-format-lib && hpack)
(cd elm-format-test-lib && hpack)
hpack
exec cabal build elm-format --minimize-conflict-set --count-conflicts --allow-newer
