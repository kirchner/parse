start:
	nix-shell --run "npm start"

dashboard:
	nix-shell --run "npm run dashboard"

frontend:
	mkdir -p _site
	elm-make --yes --warn --output=_site/index.html src/Main.elm
