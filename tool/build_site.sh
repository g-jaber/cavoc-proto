#!/bin/sh
# Assembles site/, the static bundle of the web front end (doc/web.md,
# stage 3): the page, its vendored assets, the engine and the examples,
# publishable on any static host. The bundle mirrors the repo layout —
# including the engine's _build path — so the deployed page is the very
# page the dev server serves, and everything verified locally holds for
# the bundle. Deploy with e.g.
#   rsync -a site/ user@host:public_html/cavoc/
# and point attendees at .../cavoc/front/index.html
set -eu
cd "$(dirname "$0")/.."
# Release profile: dune's dev profile pretty-prints the js_of_ocaml
# output (~25 MB); release minifies it (~2.5 MB).
dune build --profile release bin/explore_web.bc.js \
  bin/ocamlformat_web/ocamlformat_web.bc.js
rm -rf site
mkdir -p site/_build/default/bin/ocamlformat_web
cp -R front site/front
cp -R test site/test
# Repo-only material: unit tests, orphaned comparison files, cram tests
# and dune stanzas have no business on the web host.
rm -rf site/test/unit site/test/compare
rm -f site/test/dune site/test/*.t site/test/*/dune
cp _build/default/bin/explore_web.bc.js site/_build/default/bin/
cp _build/default/bin/ocamlformat_web/ocamlformat_web.bc.js \
  site/_build/default/bin/ocamlformat_web/
# The root page forwards to the app, keeping any #<dir> deep link, so
# the deployed URL can be the bare site root.
cat > site/index.html <<'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8" />
<title>CAVOC</title>
<script>location.replace('front/index.html' + location.hash);</script>
</head>
<body><a href="front/index.html">CAVOC</a></body>
</html>
EOF
echo "site/ ready — try: python3 -m http.server -d site 8000"
echo "then open http://localhost:8000/front/index.html"
