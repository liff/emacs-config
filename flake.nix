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
        inherit (builtins)
          attrValues concatStringsSep filter getAttr map isString readFile;
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlays.default ];
        };
        inherit (pkgs.lib) isDerivation fakeHash;
        inherit (pkgs) fetchpatch runCommand writeText;

        patchedEmacs = (pkgs.emacs-pgtk.overrideAttrs (prev: {

          passthru = prev.passthru // {
            treeSitter = true;
          };

          patches = (prev.patches or [ ]) ++ [
            (fetchpatch {
              name = "xdg-plus.patch";
              url =
                "https://github.com/liff/emacs/compare/master...liff:emacs:xdg-plus.patch";
              hash = "sha256-qu92/oN6NNI3XyLVbjaPHznz2hk/bdwVOqHYhd+PC38=";
            })
            (fetchpatch {
              name = "eglot-expand-region.patch";
              url =
                "https://github.com/liff/emacs/compare/master...liff:emacs:eglot-expand-region.patch";
              hash = "sha256-PJ41AxYNcWiTINKWVYGJVh3+JHx02FdeHYvOpxVwiPw=";
            })
          ];
        }));

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
          "recentf"
          "secrets"
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
          "executable"
          "eshell"
          "outline"
          "imenu"
          "text-mode"
          "shortdoc"
          "doc-view"
          "vc"
          "compile"
          "prog-mode"
          "lisp-mode"
          "edebug"
          "elisp-mode"
          "sql"
          "cc-mode"
          "c-ts-mode"
          "cmake-ts-mode"
          "dockerfile-ts-mode"
          "java-ts-mode"
          "python"
          "rust-ts-mode"
          "html-ts-mode"
          "toml-ts-mode"
          "yaml-ts-mode"
          "conf-mode"
          "sh-script"
          "js"
          "diff-mode"
          "flymake"
          "flyspell"
          "winner"
          "eglot"
          "smerge-mode"
          "windmove"
        ];
        usedPackages = [
          # Early and/or essential
          "dash"
          "s"
          "f"
          "emacsql"
          "emacsql-sqlite-builtin"
          "diminish"

          # Tools
          "gsettings"
          "transient"
          "undo-tree"
          "which-key"
          "windsize"
          {
            name = "popper";
            require = [ "popper" "popper-echo" ];
          }
          "ws-butler"
          "editorconfig"
          "avy"
          "envrc"
          "crux"
          "move-dup"
          "better-jumper"
          "goto-chg" # TODO
          "helpful"
          {
            name = "corfu";
            require = [ "corfu" "corfu-history" "corfu-popupinfo" ];
          }
          "consult"
          "consult-project-extra"
          "vertico"
          "orderless"
          "marginalia"
          "embark"
          "embark-consult"
          "with-editor"
          "restclient"
          "wgrep"

          # Version control
          "magit"
          "forge"

          # File and project management
          "treemacs"
          "treemacs-all-the-icons"
          "treemacs-icons-dired"
          "treemacs-magit"

          # Prose
          "flyspell-correct"
          "plantuml-mode"
          "mermaid-mode"
          "markdown-mode"
          "grip-mode"
          "gh-md"
          "adoc-mode"

          # Programming Assistance
          {
            name = "smartparens";
            require = [ "smartparens-config" ];
          }
          "rainbow-delimiters"
          "prism"
          "expand-region"
          #"combobulate"
          "consult-eglot"
          "aggressive-indent"
          "scopeline"

          # Programming Languages and File Formats
          "elisp-autofmt"
          "nix-mode"
          "nickel-mode"
          "hcl-mode"
          "terraform-mode"
          "jq-mode"
          #"protobuf-ts-mode"
          "haskell-mode"
          "kotlin-mode" # TODO: replace with kotlin-ts-mode

          # Tools
          "himalaya"

          # Themes
          "kind-icon"
          "twilight-anti-bright-theme"
          "modus-themes"
          "nano-theme"
          "ef-themes"
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
          (defconst nixpkgs/aspell "${aspellWithDicts (ds: with ds; [ en fi ])}")
          (defconst nixpkgs/editorconfig-core-c "${editorconfig-core-c}")
          (defconst nixpkgs/ghostscript "${ghostscript}")
          (defconst nixpkgs/openjdk "${openjdk}")
          (defconst nixpkgs/plantuml "${plantuml}")
          (defconst nixpkgs/mermaid-cli "${nodePackages.mermaid-cli}")
          (defconst nixpkgs/python3 "${python3}")
          (defconst nixpkgs/sqlite "${sqlite}")
          (defconst nixpkgs/grip "${python3Packages.grip}")
          (defconst nixpkgs/marksman "${marksman}")
        '';

        defaultEl = let
          bundled = concatStringsSep "\n" (map requireSexp bundledRequires);
          installed = concatStringsSep "\n" (map toRequire usedPackages);
        in writeText "default.el" ''
          ;; Local Variables:
          ;; lexical-binding t
          ;;; Code:
          (custom-set-variables
            `(gc-cons-threshold ,(* 1024 1024 1024))
            '(byte-compile-warnings '(not docstrings))
            '(warning-suppress-log-types '((comp) (bytecomp))))
          ${nixDependencies}

          (setq edebug-inhibit-emacs-lisp-mode-bindings t)
          ${bundled}

          (require 'f)
          (setq magit-define-global-key-bindings nil)
          (setq forge-database-connector 'sqlite-builtin)
          (setq treemacs-python-executable (f-join nixpkgs/python3 "bin/python3"))
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

        removePkg = name: inputs:
          filter (p: !(p ? pname) || p.pname != name) inputs;

        noEglot = removePkg "eglot";

        epkgOverrides = final: prev: {
          smartparens = prev.smartparens.overrideAttrs (old: {
            patches = (old.patches or []) ++ [
              ./patches/smartparens/0001-Add-support-for-tree-sitter-modes.patch
            ];
          });
          consult-eglot = prev.consult-eglot.overrideAttrs (old: {
            nativeBuildInputs = (old.nativeBuildInputs or [ ])
              ++ [ pkgs.gnused ];
            postPatch = (old.postPatch or "") + ''
              sed -ri 's/Package-Requires: (.*) \(eglot "[^"]+"\)/Package-Requires: \1/' consult-eglot.el
            '';
            buildInputs = noEglot (old.buildInputs or [ ]);
            propagatedBuildInputs = noEglot (old.propagatedBuildInputs or [ ]);
            propagatedUserEnvPkgs = noEglot (old.propagatedUserEnvPkgs or [ ]);
          });
        };

        emacsPackages =
          (pkgs.emacsPackagesFor patchedEmacs).overrideScope' epkgOverrides;
        emacsWithPackages = emacsPackages.emacsWithPackages;
        finalEmacs = emacsWithPackages (epkgs:
          [ defaultElAsPackage ] ++ [ (ollijh epkgs) ]
          ++ [ epkgs.treesit-grammars.with-all-grammars ]
          ++ map (use: toEpkg use epkgs) usedPackages);

        app = {
          type = "app";
          program = "${finalEmacs}/bin/emacs";
        };

      in {
        packages = { default = finalEmacs; };
        apps = { default = app; };
      });
}
