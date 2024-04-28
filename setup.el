;;; startup/C source
(setq read-process-output-max (* 4 1024 1024)) ;; 1mb
(custom-set-variables
 '(auto-save-list-file-prefix (f-join user-state-directory "auto-save-list/saves-"))
 '(create-lockfiles nil)
 '(delete-by-moving-to-trash t)
 '(history-delete-duplicates t)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message "")
 '(indent-tabs-mode nil)
 ;;'(pixel-scroll-precision-mode t)
 '(redisplay-skip-fontification-on-input t)
 '(scroll-step 1)
 '(show-trailing-whitespace t)
 '(visible-bell t)
 '(window-combination-resize t)
 '(font-use-system-font t)
 '(meta-prefix-char nil)
 '(x-underline-at-descent-line t))
;;; minibuffer
(ollijh/keymap-rewrite minibuffer-mode-map
                       :unset '("C-g" "C-j" "M-<" "M-p" "M-r" "M-s" "C-x" "ESC")
                       :set '(("<escape>" . minibuffer-keyboard-quit)
                              ("C-<tab>" . next-line)
                              ("C-<iso-lefttab>" . previous-line)
                              ("C-S-<tab>" . previous-line)))
(ollijh/keymap-rewrite minibuffer-local-map
		       :set '(("M-<up>" . previous-history-element)
			      ("M-<down>" . next-history-element)))
(custom-set-variables '(completions-detailed t))
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
;;; auth-source
(custom-set-variables
 '(auth-sources `(default
		  "secrets:Login"
		  ,(f-join user-state-directory "authinfo"))))
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
 '(ispell-program-name (f-join nixpkgs/aspell "bin/aspell"))
 '(ispell-alternate-dictionary (getenv "WORDLIST")))
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
;;; kill-ring-deindent
(kill-ring-deindent-mode 1)
;;; treesit
(custom-set-variables
 '(major-mode-remap-alist '(
                            ;;(c-mode          . c-ts-mode)
                            ;;(c++-mode        . c++-ts-mode)
                            ;;(cmake-mode      . cmake-ts-mode)
                            ;;(conf-toml-mode  . toml-ts-mode)
                            ;;(css-mode        . css-ts-mode)
                            ;;(js-mode         . js-ts-mode)
                            ;;(js-json-mode    . json-ts-mode)
                            ;;(python-mode     . python-ts-mode)
                            ;;(sh-mode         . bash-ts-mode)
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
;;; c-mode
(ollijh/keymap-rewrite c-mode-base-map
		       :unset '("C-c" "ESC" "C-d"))
(ollijh/keymap-rewrite c-mode-map
                       :unset '("C-c"))
(define-key c-mode-base-map [remap delete-forward-char] 'c-electric-delete-forward)
(custom-set-variables '(c-default-style '((java-mode . "java")
					 (awk-mode . "awk")
					 (other . "linux"))))
(add-hook 'c-mode-hook #'eglot-ensure)
;;; c-ts-mode
(ollijh/keymap-rewrite c-ts-base-mode-map
                       :unset '("C-c"))
(custom-set-variables
 '(c-ts-mode-indent-style 'linux)
 '(c-ts-mode-indent-offset 8))
(add-hook 'c-ts-mode-hook #'eglot-ensure)
;;; python
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook #'eglot-ensure)
(ollijh/keymap-rewrite python-ts-mode-map
		       :unset '("ESC" "C-c"))
(ollijh/keymap-rewrite python-mode-map
		       :unset '("ESC" "C-c"))
(setq-default eglot-workspace-configuration
	      '((:pylsp . (:plugins
			   (:black (:enabled t :line_length 140))))))

;;; yaml-ts-mode
(add-hook 'yaml-ts-mode-hook #'eglot-ensure)

;;; rust-ts-mode
(add-hook 'rust-ts-mode-hook #'eglot-ensure)
(add-hook 'rust-ts-mode-hook (lambda () (setq-local compile-command "cargo build")))
;;; typescript-ts-mode
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'tsx-ts-mode #'eglot-ensure)
;;; conf-mode
(ollijh/keymap-rewrite conf-mode-map
                       :unset '("C-c"))
;;; sh-script
(ollijh/keymap-rewrite sh-mode-map
                       :unset '("C-c" "C-M-x"))

;;; js
;;; diff-mode
(ollijh/keymap-rewrite diff-mode-map
		       :unset '("C-x" "C-c" "ESC")
		       :set '(("C-<down>" . diff-hunk-next)
			      ("C-<up>" . diff-hunk-prev)
			      ("M-g M-o" . diff-goto-source)
			      ("C-<return>" . diff-apply-hunk)
			      ("C-M-c C-M-c" . diff-hunk-kill)))
;;; winner-mode
(ollijh/keymap-rewrite winner-mode-map
                       :unset '("C-c")
                       :set '(("M-. M-z" . winner-undo)
                              ("M-. M-y" . winner-redo)))
(winner-mode 1)
;;; eglot
(custom-set-variables '(eglot-extend-to-xref t))
(add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)
(keymap-set eglot-mode-map "<f2>" #'eglot-rename)
(keymap-set eglot-mode-map "M-o M-o" #'eglot-format-buffer)
(setq-default eglot-workspace-configuration
	      '((:pylsp . (:plugins
			   (:black (:enabled t :line_length 140))))
		(:yaml . (:format (:enable t)))))

;;; smerge-mode
(ollijh/keymap-rewrite smerge-mode-map
		       :unset '("C-c")
		       :set '())
;;; windmove

;;; gsettings
(when (gsettings-available?)
  (blink-cursor-mode (if (gsettings-get "org.gnome.desktop.interface" "cursor-blink") 1 -1)))

;;; transient
(ollijh/keymap-rewrite transient-map
                       :unset '("ESC")
                       :set '(("<escape>" . transient-quit-one)))
(custom-set-variables
 '(transient-history-file (f-join user-state-directory "transient/history.el"))
 '(transient-levels '((magit-pull (transient:magit-pull:--autostash . 1)))))

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

;;; windsize

;;; popper
(custom-set-variables
 '(popper-group-function #'popper-group-by-project)
 ;;'(popper-display-function #'display-buffer-in-child-frame)
 '(popper-reference-buffers '("\\*Messages\\*$"
                              "\\*Warnings\\*$"
			      "\\*Async Shell Command\\*$"
			      "\\*Pp Eval Output\\*$"
                              help-mode
                              helpful-mode
			      apropos-mode
                              compilation-mode
			      special-mode)))
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
;; (better-jumper-mode +1)
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (advice-add 'switch-to-buffer :around #'ollijh/set-jump-maybe)
;;             (advice-add 'beginning-of-buffer :around #'ollijh/set-jump-maybe)
;;             (advice-add 'end-of-buffer :around #'ollijh/set-jump-maybe)
;;             (advice-add 'xref-find-definitions :around #'ollijh/set-jump-maybe)
;;             (advice-add 'search-forward :around #'ollijh/set-jump-maybe)
;;             (advice-add 'search-backward :around #'ollijh/set-jump-maybe)
;;             (advice-add 'consult-line :around #'ollijh/set-jump-if-moved))
;;           100)

;;; dogears
(dogears-mode 1)

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
 '(corfu-auto t))
(eldoc-add-command #'corfu-insert)
(global-corfu-mode 1)
;;; corfu-popupinfo
(custom-set-variables
 '(corfu-popupinfo-delay 0.3))
(corfu-popupinfo-mode 1)
;;; corfu-history
(corfu-history-mode 1)
(add-to-list 'savehist-additional-variables 'corfu-history)

;;; consult
(custom-set-variables
 '(consult-line-start-from-top t))
(keymap-set prog-mode-map "M-p M-s" #'consult-imenu)
(keymap-set prog-mode-map "C-'" #'next-error)

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
(keymap-set embark-identifier-map "]" #'eglot-code-actions)

;;; embark-consult
(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode) ; TODO: is this useful?
(add-hook 'embark-collect-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))

;;; with-editor
(ollijh/keymap-rewrite with-editor-mode-map
                       :unset '("C-c")
                       :set '(("C-<return>" . with-editor-finish)
                              ("<escape>" . with-editor-cancel)))

;;; restclient
(ollijh/keymap-rewrite restclient-mode-map
                       :unset '("C-c")
                       :set '(("C-<return>" . restclient-http-send-current)
                              ("C-<up>" . restclient-jump-prev)
                              ("C-<down>" . restclient-jump-next)))
(ollijh/keymap-rewrite restclient-outline-mode-map
		       :unset '("C-c"))
(add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))

;;; wgrep
(custom-set-variables '(wgrep-enable-key "e"))
(ollijh/keymap-rewrite wgrep-mode-map
                       :unset '("C-c")
                       :set '(("C-<return>" . wgrep-finish-edit)
			      ("C-<escape>" . wgrep-abort-changes)
			      ("M-k" . wgrep-mark-deletion)
			      ("M-<backspace>" . wgrep-remove-change)
			      ("<escape>" . wgrep-exit)))

;;; mw-thesaurus

;;; ellama

;;; magit
(ollijh/keymap-rewrite git-commit-mode-map
		       :unset '("C-c")
		       :set '(("M-<up>" . git-commit-prev-message)
			      ("M-<down>" . git-commit-next-message)))
(ollijh/keymap-rewrite magit-section-mode-map
                       :unset '("TAB" "C-<tab>" "M-<tab>" "<backtab>" "p" "n" "M-p" "M-n" "DEL" "S-SPC" "ESC")
                       :set '(("C-<down>" . magit-section-forward) ("C-<up>" . magit-section-backward)))
(ollijh/keymap-rewrite magit-diff-section-map
		       :unset '("C-x" "C-c"))
(ollijh/keymap-rewrite magit-revision-mode-map
                       :unset '("C-x" "C-c"))
(ollijh/keymap-rewrite magit-mode-map
                       :unset '("TAB" "C-w" "C-c" "C-M-i" "<" ">" "DEL" "S-SPC" "M-w" "C-<return>" "C-<tab>" "M-<tab>" "<backtab>" "ESC")
                       :set '(("<right>" . magit-section-show)
                              ("<left>" . magit-section-hide)
                              ("M-g M-w" . magit-browse-thing)
                              ("<f5>" . magit-refresh)
                              ("M-<return>" . magit-stage-file)
                              ("M-<backspace>" . magit-unstage-file)))
(ollijh/keymap-rewrite magit-log-mode-map
		       :unset '("C-c")
		       :set '(("M-<left>" . magit-go-backward)
			      ("M-<right>" . magit-go-forward)))
(ollijh/keymap-rewrite magit-log-select-mode-map
		       :unset '("C-c")
		       :set '(("C-<return>" . magit-log-select-pick)
			      ("ESC" . magit-log-select-quit)
			      ("M-<left>" . undefined)
			      ("M-<right>" . undefined)))

(custom-set-variables
 '(magit-wip-mode t))

;;; forge
(custom-set-variables
 '(forge-database-file (f-join user-state-directory "forge/database.sqlite")))
(ollijh/keymap-rewrite forge-topic-mode-map
		       :unset '("C-c"))
(ollijh/keymap-rewrite forge-post-mode-map
		       :unset '("C-c")
		       :set '(("C-<return>" . forge-post-submit)
			      ("ESC" . forge-post-cancel)))

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

;;; caser

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
			      ("M-o M-a" . markdown-table-align)
			      ("<f2>" . eglot-rename)))

;;; grip-mode
(custom-set-variables
 '(grip-preview-use-webkit t)
 '(grip-update-after-change nil)
 '(grip-binary-path (f-join nixpkgs/grip "bin/grip")))
(let* ((info (car (auth-source-search
		   :host "api.github.com"
		   :max 1
		   :require '(:user :secret))))
       (user (plist-get info :user))
       (password (auth-info-password info)))
  (setq grip-github-user user)
  (setq grip-github-password password))

;;; adoc-mode
;; TODO: customize adoc-code-lang-modes
(ollijh/keymap-rewrite adoc-mode-map :unset '("C-c"))
(add-hook 'adoc-mode-hook (lambda () (setq fill-column 80)))
(keymap-set adoc-mode-map "M-p M-s" #'consult-imenu)

;;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; prism
(add-hook 'bash-ts-mode-hook #'prism-mode)
(add-hook 'c-ts-mode-hook #'prism-mode)
(add-hook 'emacs-lisp-mode-hook #'prism-mode)
(add-hook 'python-ts-mode-hook #'prism-whitespace-mode)
(add-hook 'rust-ts-mode-hook #'prism-mode)
(add-hook 'yaml-ts-mode-hook #'prism-whitespace-mode)

;;; expand-region
(custom-set-variables '(expand-region-fast-keys-enabled nil))
(add-to-list 'er/try-expand-list 'eglot-expand-region)

;;; consult-eglot

;;; aggressive-indent
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(ollijh/keymap-rewrite aggressive-indent-mode-map :unset '("C-c"))

;;; scopeline
(add-hook 'tree-sitter-mode-hook #'scopeline-mode)

;;; breadcrumb
(breadcrumb-mode 1)

;;; elisp-autofmt
(keymap-set emacs-lisp-mode-map "M-o M-o" 'elisp-autofmt-buffer)

;;; nix-mode
(add-hook 'nix-mode-hook #'eglot-ensure)
(add-hook 'nix-mode-hook
          (lambda ()
            (let ((default-directory (project-root (project-current t))))
              (setq-local compile-command
                          (cond ((f-exists? "flake.nix") "nix -L build")
                                (buffer-file-name (concat "nix -L build -f " buffer-file-name))
                                (t "make -k "))))))

;;; nickel-mode
(add-hook 'nickel-mode-hook #'eglot-ensure)

;;; yaml-mode
(add-hook 'yaml-mode-hook #'eglot-ensure)

;;; flymake-yamllint
(add-hook 'yaml-mode-hook 'flymake-yamllint-setup)

;;; flymake-hadolint
(add-hook 'dockerfile-ts-mode-hook 'flymake-hadolint-setup)

;;; hcl-mode
(ollijh/keymap-rewrite hcl-mode-map
                       :unset '("ESC"))

;;; terraform-mode
(add-hook 'terraform-mode-hook #'eglot-ensure)
(add-hook 'terraform-mode-hook (lambda () (setq-local compile-command "terraform plan")))
(ollijh/keymap-rewrite terraform-mode-map
		       :unset '("C-c")
                       :set '(("M-o M-o" . terraform-format-buffer)))

;;; jq-mode
(add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))

;;; haskell-mode
(add-hook 'haskell-mode-hook #'eglot-ensure)
(ollijh/keymap-rewrite haskell-mode-map
                       :unset '("C-c"))

;;; kotlin-mode
(add-hook 'kotlin-mode-hook #'eglot-ensure)
(add-hook 'kotlin-mode-hook (lambda () (setq-local compile-command "gradle build")))
(ollijh/keymap-rewrite kotlin-mode-map
                       :unset '("C-c"))

;;; jarchive
(jarchive-mode)

;;; csv-mode
(ollijh/keymap-rewrite csv-mode-map :unset '("C-c"))
(add-hook 'csv-mode-hook 'csv-guess-set-separator)

;;; tuareg
(add-hook 'tuareg-mode-hook #'eglot-ensure)
(ollijh/keymap-rewrite tuareg-mode-map :unset '("C-c" "C-x" "C-h" "ESC"))

;;; reason-mode
(ollijh/keymap-rewrite reason-mode-map :unset '("C-c"))
(add-hook 'reason-mode-hook #'eglot-ensure)

;;; merlin
(add-hook 'caml-mode-hook #'merlin-mode)
(ollijh/keymap-rewrite merlin-mode-map :unset '("C-c"))

;;; merlin-eldoc
(add-hook 'reason-mode-hook 'merlin-eldoc-setup)

;;; dune
(ollijh/keymap-rewrite dune-mode-map :unset '("C-c"))

;;; himalaya

;;; kind-icon
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

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
                       '(("RET" . default-indent-new-line)
                         ("TAB" . indent-for-tab-command)
                         ("C-o" . find-file)
                         ("C-M-o" . project-switch-project)
                         ("C-s" . save-buffer)
                         ("C-q" . save-buffers-kill-terminal)

                         ("M-/" . execute-extended-command)
                         ("M-?" . execute-extended-command-for-buffer)

                         ("<escape>" . keyboard-quit)

                         ;; Searching
                         ("C-f" . ollijh/consult-line)
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
			 ("C-M-<down>" . ollijh/lineify-region)

                         ;; Manipulation
                         ("<delete>" . delete-forward-char)
                         ("<backspace>" . backward-delete-char-untabify)
                         ("C-<backspace>" . ollijh/backward-delete-word)
                         ("C-<delete>" . ollijh/forward-delete-word)
                         ("M-," . move-dup-duplicate-down) ; TODO: duplicate-dwim and move down?
                         ("M-k" . ollijh/delete-whole-line)
                         ("M-j" . crux-top-join-line)
                         ("M-u" . ollijh/toggle-case)
			 ("M-U M-C" . caser-camelcase-dwim)
			 ("M-U M-S" . caser-snakecase-dwim)
			 ("M-U M-D" . caser-dashcase-dwim)

                         ;; Navigation
                         ("C-<tab>" . ollijh/switch-to-other-buffer)
                         ("M-d" . consult-project-extra-find)
                         ("M-D" . consult-buffer)
                         ("C-<f4>" . kill-current-buffer)
                         ("C-<prior>" . previous-buffer)
                         ("C-<next>" . next-buffer)
                         ;;("M-<left>" . better-jumper-jump-backward)
                         ;;("M-<right>" . better-jumper-jump-forward)
                         ("M-<left>" . dogears-back)
                         ("M-<right>" . dogears-forward)
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
			 ("C-<up>" . ollijh/prev-imenu-item)
			 ("C-<down>" . ollijh/next-imenu-item)

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
                         ("M-. M-<delete> M-<delete>" . delete-window)
                         ("M-. M-<delete> M-<up>" . windmove-delete-up)
                         ("M-. M-<delete> M-<down>" . windmove-delete-down)
                         ("M-. M-<delete> M-<left>" . windmove-delete-left)
                         ("M-. M-<delete> M-<right>" . windmove-delete-right)
			 ("M-. M-s" . window-swap-states)
                         ("C-`" . popper-toggle)

                         ;; Tools
                         ("M-<home>" . treemacs-select-window)
                         ("C-." . emoji-insert)
                         ("M-'" . embark-act)
                         ("M-(" . pp-eval-expression)
                         ("C-(" . eval-expression)

                         ;; Documentation
                         ("<f1>" . helpful-at-point)
                         ("C-/" . describe-bindings)

                         ;; Copying
                         ("C-M-c C-M-p" . dired-copy-filename-as-kill)
                         ("C-M-c C-M-w" . browse-at-remote-kill)

			 ;; VC
			 ("<f4>" . magit-status)
                         ("M-v M-b" . magit-branch)
                         ("M-v M-p" . magit-push)
                         ("M-v M-f" . magit-pull)

                         ;; Other
                         ("<f10>" . menu-bar-open)
                         ("S-<f10>" . context-menu-open)
                         ("C--" . text-scale-adjust)
                         ("C-=" . text-scale-adjust)
                         ("C-0" . text-scale-adjust)
                         )
                       )

(use-global-map ollijh/global-map)

;;; Theme
(ollijh/choose-theme-from-appearance (ollijh/currently-preferred-appearance))
(ollijh/register-appearance-change-handler #'ollijh/choose-theme-from-appearance)

;;; envrc
(envrc-global-mode) ;; Must be as late as possible
