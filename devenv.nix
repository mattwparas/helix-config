{ pkgs, lib, config, inputs, ... }:

{
  packages = with pkgs; [
    git
    xplr
    pkg-config
    openssl
  ] ++ lib.optionals pkgs.stdenv.isDarwin [
    pkgs.libiconv
  ];

  languages.rust = {
    enable = true;
    channel = "stable";
  };

  scripts = {
    install-steel-forge.exec = ''
      echo "Installing forge CLI from steel repo..."
      cargo install --git https://github.com/mattwparas/steel.git steel-forge
    '';

    install-helix-steel.exec = ''
      if [ -z "$HELIX_SRC" ]; then
        echo "Set HELIX_SRC to path of mattwparas/helix clone, or clone it:"
        echo "  git clone https://github.com/mattwparas/helix \$HOME/helix-steel"
        echo "  export HELIX_SRC=\$HOME/helix-steel"
        exit 1
      fi
      echo "Building helix with steel from $HELIX_SRC..."
      (cd "$HELIX_SRC" && cargo xtask steel)
    '';

    deploy.exec = ''
      DEST="$HOME/.config/helix/vim"
      SRC="$(pwd)/vim"
      if [ ! -d "$SRC" ]; then
        echo "error: vim/ not found in $(pwd) — run from the repo root"
        exit 1
      fi
      mkdir -p "$DEST"
      cp "$SRC"/*.scm "$DEST/"
      echo "Copied $(ls "$SRC"/*.scm | wc -l | tr -d ' ') files → $DEST"
    '';

    dev-setup.exec = ''
      echo "=== helix-vim-plugin dev setup ==="
      echo ""
      echo "Step 1: install forge CLI"
      echo "  run: install-steel-forge"
      echo ""
      echo "Step 2: clone & build helix fork"
      echo "  git clone https://github.com/mattwparas/helix ~/helix-steel"
      echo "  export HELIX_SRC=~/helix-steel"
      echo "  run: install-helix-steel"
      echo ""
      echo "Step 3: install this package"
      echo "  forge install"
      echo ""
    '';
  };

  enterShell = ''
    dev-setup
  '';
}
