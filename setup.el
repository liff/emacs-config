;;; startup/C source
(custom-set-variables
 '(auto-save-list-file-prefix (f-join user-state-directory "auto-save-list/saves-"))
 '(create-lockfiles nil)
 '(delete-by-moving-to-trash t)
 '(history-delete-duplicates t)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message "")
 ;;'(pixel-scroll-precision-mode t)
 '(redisplay-skip-fontification-on-input t)
 '(scroll-step 1)
 '(show-trailing-whitespace t)
 '(visible-bell t)
 '(window-combination-resize t)
 '(font-use-system-font t)
 '(meta-prefix-char nil))
;;; minibuffer
(ollijh/keymap-rewrite minibuffer-mode-map
                       :unset '("C-g" "C-j" "M-<" "M-p" "M-r" "M-s" "C-x" "ESC")
                       :set '(("<escape>" . minibuffer-keyboard-quit)
                              ("C-<tab>" . next-line)
                              ("C-S-<tab>" . previous-line)))
;;; window
(custom-set-variables '(scroll-error-top-bottom t))
;;; files
(custom-set-variables '(make-backup-files nil))
;;; custom
(custom-set-variables '(custom-safe-themes t))
;;; simple
(custom-set-variables
 '(backward-delete-char-untabify-method 'hungry)
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(kill-do-not-save-duplicates t)
 '(next-error-message-highlight t))
(column-number-mode 1)
;;; help
(custom-set-variables '(help-window-select t))
(ollijh/keymap-rewrite help-mode-map
                       :unset '("<" ">" "g" "l" "n" "p" "r" "s" "DEL" "C-c")
                       :set '(("<f5>" . revert-buffer)
                              ("M-<left>" . help-go-back)
                              ("M-<right>" . help-go-forward)
                              ("<escape>" . quit-window)
                              ("M-g M-g" . help-view-source)
                              ("C-<down>" . help-goto-next-page)
                              ("C-<up>" . help-goto-previous-page)))
;;; jit-lock
(custom-set-variables '(jit-lock-defer-time 0))
;;; delsel
(delete-selection-mode 1)
;;; paragraphs
(custom-set-variables '(sentence-end-double-space nil))
;;; indent
(custom-set-variables  '(tab-always-indent 'complete))
;;; paren
(custom-set-variables '(show-paren-context-when-offscreen 'overlay))
;;; replace
(ollijh/keymap-rewrite occur-edit-mode-map
                       :unset '("C-o" "C-c")
                       :set '(("C-<return>" . occur-cease-edit)
                              ("M-g M-o" . occur-mode-display-occurrence)
                              ("C-'" . next-error-follow-minor-mode)))
;;; uniquify
(custom-set-variables  '(uniquify-buffer-name-style 'forward))
;;; tool-bar
(tool-bar-mode -1)
;;; menu-bar
(menu-bar-mode -1)
;;; recentf
(custom-set-variables
 '(recentf-save-file (f-join user-state-directory "recentf"))
 '(recentf-exclude (list (lambda (filename) (string-prefix-p "/nix/store")))))
(recentf-mode 1)
;;; savehist
(custom-set-variables
 '(savehist-file (f-join user-state-directory "history")))
(savehist-mode 1)
;;; saveplace
(custom-set-variables
 '(save-place-file (f-join user-state-directory "places")))
(save-place-mode 1)
;;; hl-line
(global-hl-line-mode 1)
;;; display-fill-column-indicator
(global-display-fill-column-indicator-mode 1)
;;; line-numbers
(custom-set-variables '(display-line-numbers-type 'relative))
;;; ispell
(custom-set-variables
 '(ispell-program-name (f-join nixpkgs/aspell "bin/aspell")))
;;; project
(custom-set-variables '(project-list-file (f-join user-state-directory "projects")))
;;; multisession
(custom-set-variables '(multisession-directory (f-join user-state-directory "multisession")))
;;; tramp
(custom-set-variables '(tramp-default-method 'ssh))
;;; eshell
(custom-set-variables '(eshell-directory-name (f-join user-state-directory "eshell/")))
;;; text-mode
(ollijh/keymap-rewrite text-mode-map
                       :unset '("ESC"))
;;; autorevert
(global-auto-revert-mode 1)
;;; treesit
(custom-set-variables
 '(major-mode-remap-alist '((c-mode          . c-ts-mode)
                            (c++-mode        . c++-ts-mode)
                            (cmake-mode      . cmake-ts-mode)
                            (conf-toml-mode  . toml-ts-mode)
                            (css-mode        . css-ts-mode)
                            (js-mode         . js-ts-mode)
                            (js-json-mode    . json-ts-mode)
                            (python-mode     . python-ts-mode)
                            (sh-mode         . bash-ts-mode)
                            (typescript-mode . typescript-ts-mode))))
;;; tramp
(custom-set-variables '(tramp-default-method 'ssh))
;;; executable
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
;;; outline
(ollijh/keymap-rewrite outline-mode-map
                       :purge t
                       :set '(("<backtab>" . outline-cycle-buffer)))
;;; shortdoc
(ollijh/keymap-rewrite shortdoc-mode-map
                       :unset '("<" ">" "g" "n" "p" "DEL" "S-SPC" "C-c")
                       :set '(("<f5>" . revert-buffer)
                              ("<escape>" . quit-window)
                              ("C-<down>" . shortdoc-next)
                              ("C-<up>" . shortdoc-previous)))
;;; doc-view
(custom-set-variables
 '(doc-view-dvipdf-program (f-join nixpkgs/ghostscript "bin/dvipdf"))
 '(doc-view-ghostscript-program (f-join nixpkgs/ghostscript "bin/gs"))
 '(doc-view-ps2pdf-program (f-join nixpkgs/ghostscript "bin/ps2pdf")))
;;; compile
(ollijh/keymap-rewrite compilation-mode-map
                       :unset '("C-c" "ESC" "TAB" "<backtab>")
                       :set '(("<escape>" . quit-window)
                              ("C-<down>" . compilation-next-error)
                              ("C-<up>" . compilation-previous-error)))
(add-hook 'compilation-filter-hook (lambda () (let ((inhibit-read-only t)) (ansi-color-apply-on-region compilation-filter-start (point)))))
(add-hook 'compilation-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))
;;; prog-mode
(ollijh/keymap-rewrite prog-mode-map
                       :unset '("C-M-q" "ESC")
                       :set '(("C-M-b" . project-compile)))
;;; lisp-mode
(ollijh/keymap-rewrite lisp-mode-shared-map
                       :unset '("DEL" "C-M-q" "ESC")
                       :set '(("C-<return>" . eval-defun)))
(ollijh/keymap-rewrite lisp-interaction-mode-map
                       :unset '("C-j" "C-M-i" "C-M-q" "C-M-x" "C-c" "ESC"))
;;; elisp-mode
(ollijh/keymap-rewrite emacs-lisp-mode-map
                       :unset '("C-j" "C-M-i" "C-M-q" "C-M-x" "C-c" "ESC"))
;;; sql
(ollijh/keymap-rewrite sql-mode-map
		       :unset '("C-c")
		       :set '(("C-<return>" . sql-send-paragraph)))
(add-to-list 'eglot-server-programs '(sql-mode . ("sqls")))
(add-hook 'sql-mode-hook #'eglot-ensure)
;;; c-ts-mode
(ollijh/keymap-rewrite c-ts-base-mode-map
                       :unset '("C-c"))
(custom-set-variables
 '(c-ts-mode-indent-style 'linux)
 '(c-ts-mode-indent-offset 8))
(add-hook 'c-ts-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs `(c-ts-mode . ,(eglot-alternatives '(("ccls") ("clangd")))))
;;; rust-ts-mode
(add-hook 'rust-ts-mode-hook #'eglot-ensure)
(add-hook 'rust-ts-mode-hook (lambda () (setq-local compile-command "cargo build")))
;;; conf-mode
(ollijh/keymap-rewrite conf-mode-map
                       :unset '("C-c"))
;;; winner-mode
(ollijh/keymap-rewrite winner-mode-map
                       :unset '("C-c")
                       :set '(("M-. M-z" . winner-undo)
                              ("M-. M-y" . winner-redo)))
(winner-mode 1)
;;; eglot
(add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)

;;; gsettings
(when (gsettings-available?)
  (blink-cursor-mode (if (gsettings-get "org.gnome.desktop.interface" "cursor-blink") 1 -1)))

;;; transient
(ollijh/keymap-rewrite transient-map
                       :unset '("ESC")
                       :set '(("<escape>" . transient-quit-one)))
(custom-set-variables
 '(transient-history-file (f-join user-state-directory "transient/history.el")))

;;; undo-tree
(ollijh/keymap-rewrite undo-tree-map
		       :unset '("C-_" "C-/" "C-?" "M-_" "C-x" "ESC")
		       ;;:set '(([remap undo-redo] 'undo-tree-redo))
		       )
(define-key undo-tree-map [remap undo-redo] 'undo-tree-redo)
(custom-set-variables
 '(undo-tree-history-directory-alist `(("." . ,(f-join user-cache-directory "undo-tree/")))))
(global-undo-tree-mode 1)

;;; which-key
(which-key-mode 1)

;;; popper
(custom-set-variables
 '(popper-group-function #'popper-group-by-project)
 ;;'(popper-display-function #'display-buffer-in-child-frame)
 '(popper-reference-buffers '("\\*Messages\\*$"
                              "\\*Warnings\\*$"
                              help-mode
                              helpful-mode
                              compilation-mode)))
(popper-mode 1)
(popper-echo-mode 1)

;;; ws-butler
(add-hook 'prog-mode-hook #'ws-butler-mode)

;;; editorconfig
(custom-set-variables
 '(editorconfig-exec-path (f-join nixpkgs/editorconfig-core-c "bin/editorconfig")))
(add-to-list 'editorconfig-indentation-alist '(c-ts-mode c-basic-offset c-ts-mode-indent-offset))
(editorconfig-mode 1)

;;; avy

;;; better-jumper
;; +++
;; *** New command 'xref-go-forward'.
;; It is bound to 'C-M-,' and jumps to the location where 'xref-go-back'
;; ('M-,', also known as 'xref-pop-marker-stack') was invoked previously.
(custom-set-variables
 '(better-jumper-context 'window)
 '(better-jumper-use-evil-jump-advice nil)
 '(better-jumper-add-jump-behavior 'replace))
(better-jumper-mode +1)
(add-hook 'after-init-hook
          (lambda ()
            (advice-add 'switch-to-buffer :around #'ollijh/set-jump-maybe)
            (advice-add 'beginning-of-buffer :around #'ollijh/set-jump-maybe)
            (advice-add 'end-of-buffer :around #'ollijh/set-jump-maybe)
            (advice-add 'xref-find-definitions :around #'ollijh/set-jump-maybe)
            (advice-add 'search-forward :around #'ollijh/set-jump-maybe)
            (advice-add 'search-backward :around #'ollijh/set-jump-maybe)
            (advice-add 'consult-line :around #'ollijh/set-jump-if-moved))
          100)

;;; helpful
(ollijh/keymap-rewrite helpful-mode-map
                       :unset '("<" ">" "g" "l" "n" "p" "r" "s" "DEL" "C-c")
                       :set '(("<f5>" . helpful-update)
                              ("M-<left>" . help-go-back)
                              ("M-<right>" . help-go-forward)
                              ("<escape>" . quit-window)
                              ("M-g M-g" . help-view-source)
                              ("C-<down>" . help-goto-next-page)
                              ("C-<up>" . help-goto-previous-page)))

;;; corfu
(custom-set-variables
 '(corfu-auto t)
 '(corfu-popupinfo-mode 1)
 '(corfu-popupinfo-delay t))
(eldoc-add-command #'corfu-insert)
(global-corfu-mode 1)

;;; consult
(custom-set-variables
 '(consult-line-start-from-top t))

;;; vertico
(vertico-mode 1)
(ollijh/keymap-set-all vertico-map
                       '(("<next>" . vertico-scroll-up)
                         ("<prior>" . vertico-scroll-down)))

;;; orderless
(custom-set-variables
 '(completion-styles '(substring orderless basic)))

;;; marginalia
(marginalia-mode 1)

;;; embark

;;; embark-consult
(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode) ; TODO: is this useful?
(add-hook 'embark-collect-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))

;;; with-editor
(ollijh/keymap-rewrite with-editor-mode-map
                       :unset '("C-c")
                       :set '(("C-<return>" . with-editor-finish)
                              ("ESC" . with-editor-cancel)))

;;; restclient
(ollijh/keymap-rewrite restclient-mode-map
                       :unset '("C-c")
                       :set '(("C-<return>" . restclient-send-current)
                              ("C-<up>" . restclient-jump-prev)
                              ("C-<down>" . restclient-jump-next)))

;;; magit
(ollijh/keymap-rewrite magit-section-mode-map
                       :unset '("TAB" "C-<tab>" "M-<tab>" "<backtab>" "p" "n" "M-p" "M-n" "DEL" "S-SPC" "ESC")
                       :set '(("C-<down>" . magit-section-forward) ("C-<up>" . magit-section-backward)))
(ollijh/keymap-rewrite magit-mode-map
                       :unset '("TAB" "C-w" "C-c" "C-M-i" "<" ">" "DEL" "S-SPC" "M-w" "C-<return>" "C-<tab>" "M-<tab>" "<backtab>" "ESC")
                       :set '(("<right>" . magit-section-show)
                              ("<left>" . magit-section-hide)
                              ("M-g M-w" . magit-browse-thing)
                              ("<f5>" . magit-refresh)
                              ("M-<return>" . magit-stage-file)
                              ("M-<backspace>" . magit-unstage-file)))
(custom-set-variables
 '(magit-wip-mode t))

;;; forge
(custom-set-variables
 '(forge-database-file (f-join user-state-directory "forge/database.sqlite")))

;;; treemacs
(ollijh/keymap-rewrite treemacs-mode-map
                       :unset '("C-j" "C-k" "DEL" "?" "S-SPC" "C-?" "M-<down>" "M-<up>" "ESC" "C-c")
                       :set '(("C-<down>" . treemacs-next-project)
                              ("C-<up>" . treemacs-previous-project)
                              ("C-e" . treemacs-select-window)
			      ("<right>" . ollijh/treemacs-expand-node)
			      ("<left>" . ollijh/treemacs-collapse-node)
			      ("C-M-S-<right>" . treemacs-increase-width)
			      ("C-M-S-<left>" . treemacs-decrease-width)))
(custom-set-variables
 '(treemacs-python-executable (f-join nixpkgs/python3 "bin/python3"))
 '(treemacs-indent-guide-mode t)
 '(treemacs-git-commit-diff-mode t)
 '(treemacs-project-follow-mode t)
 '(treemacs-follow-mode t)
 '(treemacs-tag-follow-mode t)
 '(treemacs-git-mode 'deferred)
 '(treemacs-select-when-already-in-treemacs 'close)
 '(treemacs-persist-file (f-join user-state-directory "treemacs-persist"))
 '(treemacs-last-error-persist-file (f-join user-state-directory "treemacs-persist-at-last-error"))
 '(treemacs-is-never-other-window t))

;;; treemacs-icons-dired
(treemacs-icons-dired-mode 1)

;;; flyspell-correct
                                        ;(define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-wrapper)

;;; plantuml-mode
(custom-set-variables
 '(plantuml-java-command (f-join nixpkgs/openjdk "bin/java"))
 '(plantuml-jar-path (f-join nixpkgs/plantuml "lib/plantuml.jar")))

;;; smartparens
(add-hook 'prog-mode-hook #'smartparens-mode)
(ollijh/keymap-rewrite smartparens-mode-map
                       :set '(("(" . ollijh/wrap-round-on-round)
                              ("{" . ollijh/wrap-curly-on-curly)
                              ("[" . ollijh/wrap-square-on-square)))

;;; mermaid-mode
(custom-set-variables
 '(mermaid-mmdc-location (f-join nixpkgs/mermaid-cli "bin/mmdc")))

;;; markdown-mode
(add-hook 'markdown-mode-hook #'eglot-ensure)
(ollijh/keymap-rewrite markdown-mode-map
                       :unset '("ESC" "C-c" "C-x")
                       :set '(("C-k" . markdown-insert-link)
			      ("M-|" . markdown-fill-paragraph)
			      ("C-<down>" . markdown-forward-block)
			      ("C-<up>" . markdown-backward-block)))

;;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; expand-region
(add-to-list 'er/try-expand-list 'eglot-expand-region)

;;; consult-eglot

;;; aggressive-indent
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(ollijh/keymap-rewrite aggressive-indent-mode-map :unset '("C-c"))

;;; elisp-autofmt
(keymap-set emacs-lisp-mode-map "M-o M-o" 'elisp-autofmt-buffer)

;;; nix-mode
(add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
(add-hook 'nix-mode-hook #'eglot-ensure)
(add-hook 'nix-mode-hook
          (lambda ()
            (let ((default-directory (project-root (project-current t))))
              (setq-local compile-command
                          (cond ((f-exists? "flake.nix") "nix -L build")
                                (buffer-file-name (concat "nix -L build -f " buffer-file-name))
                                (t "make -k "))))))

;;; hcl-mode
(ollijh/keymap-rewrite hcl-mode-map
                       :unset '("ESC"))

;;; terraform-mode
(add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))
(add-hook 'terraform-mode-hook #'eglot-ensure)
(add-hook 'terraform-mode-hook (lambda () (setq-local compile-command "terraform plan")))
(ollijh/keymap-rewrite terraform-mode-map
                       :set '(("M-o M-o" . terraform-format-buffer)))

;;; kotlin-ts-mode

;;; haskell-mode
(add-hook 'haskell-mode-hook #'eglot-ensure)
(ollijh/keymap-rewrite haskell-mode-map
                       :unset '("C-c"))

;;; kotlin-mode
(add-hook 'kotlin-mode-hook #'eglot-ensure)
(add-hook 'kotlin-mode-hook (lambda () (setq-local compile-command "gradle build")))
(ollijh/keymap-rewrite kotlin-mode-map
                       :unset '("C-c"))

;;; adwaita
(load-theme 'adwaita t t)

;;; twilight-anti-bright
(load-theme 'twilight-anti-bright t t)

;;; modus-themes
(load-theme 'modus-operandi t t)
(load-theme 'modus-vivendi t t)

;;; nano-theme
(load-theme 'nano t t)

;;; ef-themes
(load-theme 'ef-cyprus t t)
(load-theme 'ef-day t t)
(load-theme 'ef-deuteranopia-light t t)
(load-theme 'ef-duo-light t t)
(load-theme 'ef-frost t t)
(load-theme 'ef-light t t)
(load-theme 'ef-spring t t)
(load-theme 'ef-summer t t)
(load-theme 'ef-trio-light t t)
(load-theme 'ef-tritanopia-light t t)

;;; Global keymap
(setq ollijh/global-map (make-keymap))

(let ((copy '(self-insert-command
              mwheel-scroll
              mouse-set-point
              mouse-drag-region
              mouse-set-region
              pgtk-drag-n-drop
              handle-select-window
              handle-select-frame
              handle-switch-frame
              ignore
              mouse-drag-bottom-left-corner
              mouse-drag-bottom-right-corner
              mouse-drag-right-edge
              mouse-drag-top-right-corner
              mouse-drag-top-edge
              mouse-drag-top-left-corner
              mouse-drag-left-edge
              mouse-drag-mode-line
              mouse-drag-vertical-line
              mouse-select-window
              scroll-bar-toolkit-horizontal-scroll
              scroll-bar-toolkit-scroll
              mouse-drag-mode-line
              mouse-select-window
              mouse-drag-tab-line)))
  (dolist (def copy)
    (substitute-key-definition def def ollijh/global-map global-map)))

(ollijh/keymap-set-all ollijh/global-map
                       '(("RET" . newline-and-indent)
                         ("TAB" . indent-for-tab-command)
                         ("C-o" . find-file)
                         ("C-M-o" . project-switch-project)
                         ("C-s" . save-buffer)
                         ("C-q" . save-buffers-kill-terminal)

                         ("M-/" . execute-extended-command)
                         ("M-?" . execute-extended-command-for-buffer)

                         ("<escape>" . keyboard-quit)

                         ;; Searching
                         ("C-f" . consult-line)
                         ("C-h" . query-replace)
                         ("<f3>" . ollijh/repeat-search-forward)
                         ("S-<f3>" . ollijh/repeat-search-backward)
                         ("C-S-f" . consult-ripgrep)

                         ;; Movement
                         ("<up>" . previous-line)
                         ("<down>" . next-line)
                         ("<left>" . left-char)
                         ("<right>" . right-char)

                         ("C-<right>" . right-word)
                         ("C-<left>" . left-word)

                         ("<prior>" . scroll-down-command)
                         ("<next>" . scroll-up-command)

                         ("<end>" . move-end-of-line)
                         ("<home>" . crux-move-beginning-of-line)
                         ("C-<home>" . beginning-of-buffer)
                         ("C-<end>" . end-of-buffer)

                         ;; Selection
                         ("C-a" . mark-whole-buffer)
                         ("C-c" . ollijh/save-whole-line-or-region)
                         ("C-x" . ollijh/kill-whole-line-or-region)
                         ("C-v" . yank)
                         ("C-z" . undo)
                         ("C-y" . undo-redo)
                         ("C-M-<right>" . er/expand-region)
                         ("C-M-<left>" . er/contract-region)

                         ;; Manipulation
                         ("<delete>" . delete-forward-char)
                         ("<backspace>" . backward-delete-char-untabify)
                         ("C-<backspace>" . ollijh/backward-delete-word)
                         ("C-<delete>" . ollijh/forward-delete-word)
                         ("M-," . move-dup-duplicate-down) ; TODO: duplicate-dwim and move down?
                         ("M-k" . ollijh/delete-whole-line)
                         ("M-j" . crux-top-join-line)

                         ;; Navigation
                         ("C-<tab>" . ollijh/switch-to-other-buffer)
                         ("M-d" . consult-project-extra-find)
                         ("M-D" . consult-buffer)
                         ("C-<f4>" . kill-current-buffer)
                         ("C-<prior>" . previous-buffer)
                         ("C-<next>" . next-buffer)
                         ("M-<left>" . better-jumper-jump-backward)
                         ("M-<right>" . better-jumper-jump-forward)
                         ("M-l" . avy-goto-char)
                         ("M-g M-b" . browse-url-at-point)
                         ("C-<mouse-1>" . browse-url-at-mouse)

                         ;; Code manipulation
                         ("M-;" . comment-dwim)

                         ;; Code navigation
                         ("M-g M-g" . xref-find-definitions)
                         ("M-g M-. M-g" . xref-find-definitions-other-window)
                         ("M-g M-u" . xref-find-references)
                         ("M-g M-w" . browse-at-remote)
                         ("M-g M-s" . consult-eglot-symbols)

                         ;; Windows/frames
                         ("<f11>" . toggle-frame-fullscreen)
                         ("C-M-<prior>" . ollijh/other-window-backwards)
                         ("C-M-<next>" . other-window)
                         ("C-M-S-<end>" . delete-other-windows)
                         ("C-M-S-<up>" . windsize-up)
                         ("C-M-S-<down>" . windsize-down)
                         ("C-M-S-<left>" . windsize-left)
                         ("C-M-S-<right>" . windsize-right)
                         ("M-. M-<down>" . split-window-below)
                         ("M-. M-<right>" . split-window-right)
                         ("M-. M-<delete>" . delete-window)
			 ("M-. M-s" . window-swap-states)
                         ("C-`" . popper-toggle-latest)

                         ;; Tools
                         ("M-<home>" . treemacs-select-window)
                         ("C-." . emoji-insert)
                         ("M-'" . embark-act)

                         ;; Documentation
                         ("<f1>" . helpful-at-point)
                         ("C-/" . describe-bindings)

                         ;; Copying
                         ("C-M-c C-M-p" . dired-copy-filename-as-kill)
                         ("C-M-c C-M-w" . browse-at-remote-kill)

                         ;; Other
                         ("<f10>" . menu-bar-open)
                         ("S-<f10>" . context-menu-open)
                         ("C--" . text-scale-adjust)
                         ("C-=" . text-scale-adjust)
                         ("C-0" . text-scale-adjust)
                         ("<f4>" . magit-status)
                         )
                       )

(use-global-map ollijh/global-map)

;;; Theme
(ollijh/choose-theme-from-appearance (ollijh/currently-preferred-appearance))
(ollijh/register-appearance-change-handler #'ollijh/choose-theme-from-appearance)

;;; envrc
(envrc-global-mode) ;; Must be as late as possible
