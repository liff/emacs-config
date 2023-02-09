;;; ollijh -- My utility functions &c.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'simple)
(require 'keymap)
(require 'dbus)
(require 'crux)
(require 'better-jumper)

(defun ollijh/keymap-set-all (map bindings)
  "Add all BINDINGS to MAP."
  (dolist (binding bindings)
    (keymap-set map (car binding) (cdr binding))))

(defun ollijh/keymap-unset-all (map keys)
  "Remove all KEYS from MAP."
  (dolist (key keys)
    (keymap-unset map key t)))

(cl-defun ollijh/keymap-rewrite (map &key (purge nil) (unset '()) (set '()))
  "Rewrite bindings of MAP. 
   If PURGE is set to non-nil, clear the whole keymap.
   Then invoke `ollijh/keymap-unset-all' and `ollijh/keymap-set-all' on the UNSET and SET
   arguments respectively."
  (when purge
    (setcdr map nil))
  (ollijh/keymap-unset-all map unset)
  (ollijh/keymap-set-all map set))

(defun ollijh/other-window-backwards ()
  "Select another window in cyclic ordering of windows, in reverse order."
  (interactive)
  (other-window -1))

(defun ollijh/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun ollijh/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (ollijh/delete-word (- arg)))

(defun ollijh/forward-delete-word (arg)
  "Delete characters forward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (ollijh/delete-word arg))

(defun ollijh/delete-whole-line ()
  (interactive)
  (if (and (eobp) (save-excursion (forward-visible-line 0) (eobp)))
      (forward-visible-line -1))
  (save-excursion (delete-region (point) (progn (forward-visible-line 0) (point))))
  (delete-region (point) (progn (forward-visible-line 1) (point)))
  (crux-move-to-mode-line-start))

(defun ollijh/save-whole-line ()
  (interactive)
  (if (and (eobp) (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (save-excursion
    (kill-ring-save (point) (progn (forward-visible-line 0) (point)))
    (kill-ring-save (point) (progn (forward-visible-line 1) (point)))))

(defun ollijh/kill-whole-line-or-region (&optional arg)
  (interactive "p")
  (if (use-region-p) (kill-region (mark) (point) 'region) (kill-whole-line arg)))

(defun ollijh/save-whole-line-or-region (&optional arg)
  (interactive)
  (if (use-region-p) (kill-ring-save (mark) (point) 'region) (ollijh/save-whole-line)))

(defun ollijh/switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun ollijh/appearance-name (code)
  "Convert numeric appearance CODE to a symbolic name.
Return `none', `light' or `dark'."
  (pcase code
    (0 'none)
    (1 'dark)
    (2 'light)
    (_ (error "Unrecognized appearance code"))))

(defun ollijh/currently-preferred-appearance ()
  "Get current appearance preference.
Return `none', `light' or `dark'."
  (ollijh/appearance-name
   (caar (dbus-call-method
	  :session
	  "org.freedesktop.portal.Desktop"
	  "/org/freedesktop/portal/desktop"
	  "org.freedesktop.portal.Settings"
	  "Read"
	  "org.freedesktop.appearance"
	  "color-scheme"))))

(defun ollijh/register-appearance-change-handler (handler)
  (dbus-register-signal
   :session
   "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop"
   "org.freedesktop.portal.Settings"
   "SettingChanged"
   (lambda (namespace key value)
     (if (and
	  (string= namespace "org.freedesktop.appearance")
	  (string= key "color-scheme"))
	 (funcall handler (ollijh/appearance-name (car value)))))))

(defun ollijh/choose-theme-from-appearance (appearance)
  (let* ((theme (pcase appearance
		  ('none 'modus-operandi)
		  ('light 'modus-operandi)
		  ('dark 'twilight-anti-bright)))
	 (themes (list theme)))
    (custom-set-variables
     `(custom-enabled-themes (quote (,theme))))))

(defun ollijh/open-project (dir)
  (interactive (list (project-prompt-project-dir)))
  (let ((project-current-directory-override dir))
    (call-interactively 'project-vc-dir)))

(defun ollijh/set-jump-if-moved (oldfun &rest args)
  (let ((old-pos (point)))
    (apply oldfun args)
    (unless (equal (point) old-pos)
      (message "set-jump-if-moved %S" old-pos)
      (better-jumper-set-jump old-pos))))

(defun ollijh/set-jump-maybe (fn &rest args)
  "Set a jump point if fn actually moves the point. Stolen from Doom."
  (let ((origin (point-marker))
        (result (let* ((better-jumper--jumping t))
		  (apply fn args)))
        (dest (point-marker)))
    (unless (equal origin dest)
      (with-current-buffer (marker-buffer origin)
        (better-jumper-set-jump
         (if (markerp (car args))
	     (car args)
	   origin))))
    (set-marker origin nil)
    (set-marker dest nil)
    result))

(provide 'ollijh)
