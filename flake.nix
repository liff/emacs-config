{
  description = "My Emacs setup as a Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, emacs-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        inherit (builtins) concatStringsSep filter getAttr map isString readFile;
        pkgs = import nixpkgs { inherit system; };
        inherit (pkgs) fetchpatch runCommand writeText;

        emacs = emacs-overlay.packages.${system}.emacsPgtk.overrideAttrs
          (prev: {
            patches = (prev.patches or [ ]) ++ [
              (fetchpatch {
                name = "xdg-plus.patch";
                url = "https://github.com/liff/emacs/compare/master...liff:emacs:xdg-plus.patch";
                hash = "sha256-h4bTYBujqQmSE9zcNWMWJ5rZGH9TRLupcrPCYnZJj+8=";
              })
              (fetchpatch {
                name = "eglot-expand-region.patch";
                url = "https://github.com/liff/emacs/compare/master...liff:emacs:eglot-expand-region.patch";
                hash = "sha256-rxRiuIUKgMmzuzYgF2IjHFR5q/euYgUQHWRX9EhXBQg=";
              })
            ];
          });

        bundledRequires = [
          "simple"
          "minibuffer"
          "files"
          "custom"
          "help"
          "jit-lock"
          "delsel"
          "paren"
          "replace"
          "uniquify"
          "tool-bar"
          "menu-bar"
          "minibuffer"
          "recentf"
          "auth-source"
          "xref"
          "xwidget"
          "dbus"
          "ansi-color"
          "savehist"
          "saveplace"
          "hl-line"
          "display-fill-column-indicator"
          "autorevert"
          "ispell"
          "flyspell"
          "flyspell-correct"
          "apropos"
          "multisession"
          "emoji"
          "url-cache"
          "eldoc"
          "project"
          "treesit"
          "tramp"
          "eshell"
          "outline"
          "text-mode"
          "shortdoc"
          "doc-view"
          "vc"
          "compile"
          "prog-mode"
          "lisp-mode"
          "edebug"
          "elisp-mode"
          "cc-mode"
          "c-ts-mode"
          "cmake-ts-mode"
          "dockerfile-ts-mode"
          "java-ts-mode"
          "rust-ts-mode"
          "html-ts-mode"
          "toml-ts-mode"
          "yaml-ts-mode"
          "conf-mode"
          "js"
          "flymake"
          "flyspell"
          "eglot"
        ];
        usedPackages = [
          # Early and/or essential
          "dash"
          "s"
          "f"
          "diminish"

          # Tools
          "gsettings"
          "transient"
          "ws-butler"
          "editorconfig"
          "avy"
          "envrc"
          "crux"
          "move-dup"
          "better-jumper"
          "helpful"
          "corfu"
          "consult"
          "consult-project-extra"
          "vertico"
          "orderless"
          "marginalia"
          "embark"
          "embark-consult"
          "dired-sidebar"
          "with-editor"

          # Version control
          "magit"

          # Prose
          "flyspell-correct"
          "plantuml-mode"
          "mermaid-mode"

          # Programming Assistance
          {
            name = "smartparens";
            require = [ "smartparens-config" ];
          }
          "rainbow-delimiters"
          "expand-region"
          #"combobulate"
          "consult-eglot"

          # Programming Languages and File Formats
          "nix-mode"
          "hcl-mode"
          "terraform-mode"
          #"protobuf-ts-mode"
          "haskell-mode"
          "kotlin-mode" # TODO: replace with kotlin-ts-mode

          # Themes
          "twilight-anti-bright-theme"
          "modus-themes"
        ];

        requireSexp = pkg: "(require '${pkg})";

        toRequire = pkg:
          if isString pkg then
            requireSexp pkg
          else
            concatStringsSep "\n" (map requireSexp pkg.require);

        toEpkg = pkg: epkgs:
          let attr = if isString pkg then pkg else pkg.name;
          in getAttr attr epkgs;

        ollijh = epkgs:
          epkgs.trivialBuild {
            pname = "ollijh";
            version = "1";
            src = ./lisp;
            buildInputs = with epkgs; [ crux better-jumper ];
          };

        nixDependencies = with pkgs; ''
          (defconst nixpkgs/aspell "${aspell}")
          (defconst nixpkgs/editorconfig-core-c "${editorconfig-core-c}")
          (defconst nixpkgs/ghostscript "${ghostscript}")
          (defconst nixpkgs/openjdk "${openjdk}")
          (defconst nixpkgs/plantuml "${plantuml}")
          (defconst nixpkgs/mermaid-cli "${nodePackages.mermaid-cli}")
        '';

        defaultEl = let
          bundled = concatStringsSep "\n" (map requireSexp bundledRequires);
          installed = concatStringsSep "\n" (map toRequire usedPackages);
        in writeText "default.el" ''
          ;; Local Variables:
          ;; lexical-binding t
          ;;; Code:
          (custom-set-variables `(gc-cons-threshold ,(* 1024 1024 1024)))
          ${nixDependencies}

          (setq edebug-inhibit-emacs-lisp-mode-bindings t)
          ${bundled}

          (setq magit-define-global-key-bindings nil)
          ${installed}
          (require 'ollijh)

          ${readFile ./setup.el}

          (garbage-collect)
          (custom-set-variables `(gc-cons-threshold ,(* 128 1024 1024)))
        '';

        defaultElAsPackage = runCommand "default.el" { } ''
          mkdir -p $out/share/emacs/site-lisp
          cp ${defaultEl} $out/share/emacs/site-lisp/default.el
        '';

        noEglot = inputs: filter (p: !(p ? pname) || p.pname != "eglot") inputs;

        epkgOverrides = final: prev: {
          consult-eglot = prev.consult-eglot.overrideAttrs (old: {
            nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ pkgs.gnused ];
            postPatch = (old.postPatch or "") + ''
              sed -ri 's/Package-Requires: (.*) \(eglot "[^"]+"\)/Package-Requires: \1/' consult-eglot.el
            '';
            buildInputs = noEglot (old.buildInputs or []);
            propagatedBuildInputs = noEglot (old.propagatedBuildInputs or []);
            propagatedUserEnvPkgs = noEglot (old.propagatedUserEnvPkgs or []);
          });
        };

        emacsWithPackages = ((pkgs.emacsPackagesFor emacs).overrideScope' epkgOverrides).withPackages (epkgs:
          [ defaultElAsPackage ] ++ [ (ollijh epkgs) ]
          ++ map (use: toEpkg use epkgs) usedPackages);

        app = {
          type = "app";
          program = "${emacsWithPackages}/bin/emacs";
        };

      in {
        packages = {
          default = emacsWithPackages;
        };
        apps = {
          default = app;
        };
      });
}
