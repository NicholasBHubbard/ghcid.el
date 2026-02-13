;;; ghcid.el --- ghcid support in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2026 Nicholas B. Hubbard <nicholashubbard@posteo.net>
;;
;; Licensed under the same terms as Emacs and under the MIT license.

;; SPDX-License-Identifier: MIT

;; Author: Nicholas B. Hubbard <nicholashubbard@posteo.net>
;; URL: https://github.com/NicholasBHubbard/ghcid.el
;; Package-Requires: ((emacs "26.1"))
;; Version: 1.0
;; Created: 2026-02-05
;; By: Nicholas B. Hubbard <nicholashubbard@posteo.net>
;; Keywords: tools, files, Haskell

;;; Commentary:

;; Use M-x ghcid-start to launch

;;; Code:

(require 'ansi-color)
(require 'comint)
(require 'project)

(defgroup ghcid nil
  "Run ghcid in Emacs."
  :group 'tools)

(defcustom ghcid-default-opts '("--reverse-errors" "--color=always")
  "Default opts to pass to ghcid."
  :type '(repeat string)
  :group 'ghcid)

(defcustom ghcid-ghci-default-opts
  '("-ferror-spans" "-fdiagnostics-color=always")
  "Default opts to pass to the underlying GHCi invocation."
  :type '(repeat string)
  :group 'ghcid)

(defcustom ghcid-default-buffer-name "*ghcid*"
  "Default name for the ghcid buffer when not in a project."
  :type 'string
  :group 'ghcid)

(defcustom ghcid-project-system 'auto-detect
  "Project system used to determine current project.

When nil, ghcid.el auto-detects, preferring projectile when available,
otherwise falling back to `project.el'."
  :type '(choice (const :tag "no project system" none)
                 (const :tag "auto-detect (prefers projectile)" auto-detect)
                 (const :tag "project.el" project)
                 (const :tag "projectile" projectile))
  :group 'ghcid)

(defcustom ghcid-buffer-maximum-lines 500
  "Maximum number of lines for a ghcid buffer.")

(defvar ghcid-buffer-name nil
  "Var to override default ghcid buffer name.")

(defvar ghcid-extra-opts '()
  "Extra opts to pass to ghcid.")

(defvar ghcid-ghci-extra-opts '()
  "Extra opts to pass to the underlying GHCi invocation.")

(defconst ghcid--osc-title-regexp "\e\\]0;.*?\e\\\\"
  "Regexp matching OSC terminal title escape sequences.")

(defun ghcid--default-directory ()
  "Return the directory to use as `default-directory' for ghcid.

The directory is chosen according to `ghcid-project-system'.

The returned value is always an absolute directory name via `expand-file-name'."
  (expand-file-name
   (or (cond ((eq ghcid-project-system 'none)
              nil)
             ((eq ghcid-project-system 'project)
              (ignore-errors (project-root (project-current))))
             ((eq ghcid-project-system 'projectile)
              (projectile-project-root))
             ((eq ghcid-project-system 'auto-detect)
              (let ((ghcid-project-system (if (featurep 'projectile)
                                              'projectile 'project)))
                (ghcid--default-directory))))
       default-directory)))

(defun ghcid--project-name ()
  "Return the name of the current project or basename of `default-directory'."
  (file-name-nondirectory (directory-file-name (ghcid--default-directory))))

(defun ghcid--buffer-name ()
  "Return the ghcid buffer name.

If `ghcid-buffer-name' is non-nil use it, otherwise determine if currently in
a project, and return a name based off the project name. If not in a project
then return `ghcid-default-buffer-name'. "
  (or ghcid-buffer-name
      (when-let ((proj-name (ghcid--project-name)))
        (format "*ghcid %s*" proj-name))
      ghcid-default-buffer-name))

(defun ghcid--get-buffer-create ()
  "Return the ghcid buffer, creating it if needed.

The buffer name is determined from the output of `ghcid--buffer-name'."
  (get-buffer-create (ghcid--buffer-name)))

(defun ghcid--get-buffer ()
  "Like `ghcid--get-buffer-create' but doesn't create buffer if doesn't exist."
  (get-buffer (ghcid--buffer-name)))

(defun ghcid--determine-build-system (&optional dir)
  "Heuristically determine the build system for DIR.

DIR defaults to the project root directory or `default-directory'. The return
value is one of the symbols `stack', `cabal', or `ghci', based on the presence
of marker files in DIR:

- `stack' if a \"stack.yaml\" file is present.
- `ghci' if a \".ghci\" file is present.
- `cabal' otherwise.

Note that these are essentially the same heuristics that ghcid itself uses."
  (let ((default-directory
         (or dir (ghcid--default-directory) default-directory)))
    (cond ((file-exists-p "stack.yaml") 'stack)
          ((file-exists-p ".ghci") 'ghci)
          (t 'cabal))))

(defun ghcid--ghci-command ()
  "Build the ghcid --command=... string for the current project."
  (let* ((bs (ghcid--determine-build-system))
         (ghci-opts (append ghcid-ghci-extra-opts ghcid-ghci-default-opts))
         (c
          (cond
           ((eq bs 'stack)
            (concat
             "stack ghci"
             (and ghci-opts
                  (concat " " (mapconcat
                               (lambda (o) (format "--ghci-options=%s" o))
                               ghci-opts " ")))))
           ((eq bs 'ghci)
            (concat
             "ghci"
             (and ghci-opts (concat " " (mapconcat #'identity ghci-opts " ")))))
           (t
            (concat
             "cabal repl"
             (and ghci-opts
                  (concat " " (mapconcat
                               (lambda (o) (format "--repl-options=%s" o))
                               ghci-opts " "))))))))
    (format "--command=%s" c)))

(defun ghcid--exec-ghcid (buf)
  "Start (or restart) a ghcid process in BUF.

The process is started with `comint-exec' using arguments from
`ghcid-extra-opts', `ghcid-default-opts', and `ghcid--ghci-command'."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when-let ((proc (get-buffer-process buf)))
        (when (process-live-p proc)
          (delete-process proc)))
      (comint-exec (current-buffer) "ghcid" "ghcid" nil
                   (cons (ghcid--ghci-command)
                         (append ghcid-extra-opts ghcid-default-opts))))))

(defun ghcid-start ()
  "Start/restart ghcid for the current project and display its buffer."
  (interactive)
  (let ((buf (ghcid--get-buffer-create)))
    (with-current-buffer buf
      (setq-local default-directory (ghcid--default-directory))
      (ghcid-mode))
    (ghcid--exec-ghcid buf)
    (pop-to-buffer buf)))

(defun ghcid-pop-to-buffer ()
  "Pop to the ghcid buffer if it exists, raising an error if it doesn't exist."
  (interactive)
  (if-let ((buf (ghcid--get-buffer)))
      (pop-to-buffer buf)
    (user-error "No buffer named %s" (ghcid--buffer-name))))

(defun ghcid-pop-to-buffer-or-start ()
  "Pop to the project's ghcid buffer if it exists, otherwise start ghcid."
  (interactive)
  (if-let ((buf (ghcid--get-buffer)))
      (pop-to-buffer buf)
    (ghcid-start)))

(define-derived-mode ghcid-mode comint-mode "ghcid"
  "Major mode for interacting with ghcid."
  :group 'ghcid
  :interactive nil
  (setq-local comint-buffer-maximum-size ghcid-buffer-maximum-lines
              comint-prompt-read-only t
              comint-move-point-for-output 'all
              ansi-color-for-comint-mode t
              switch-to-buffer-preserve-window-point nil)
  (add-hook 'comint-preoutput-filter-functions
            #'(lambda (s)
                (replace-regexp-in-string ghcid--osc-title-regexp "" s)) nil t)
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer nil t)
  (add-hook 'comint-output-filter-functions
            #'(lambda (&rest _)
                (unless (get-buffer-window (current-buffer) t)
                  (goto-char (point-max)))) nil t))

(provide 'ghcid)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; ghcid.el ends here
