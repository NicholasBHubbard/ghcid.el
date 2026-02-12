;;; ghcid.el --- ghcid support in Emacs with compilation-mode -*- lexical-binding: t -*-

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

(require 'comint)
(require 'ansi-color)

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
  "Default name for the ghcid buffer."
  :type 'string
  :group 'ghcid)

(defcustom ghcid-buffer-maximum-lines 500
  "Maximum number of lines for a ghcid buffer.")

(defvar ghcid-buffer-name nil
  "Var to override default ghcid buffer name (`ghcid-default-buffer-name').")

(defvar ghcid-extra-opts '()
  "Extra opts to pass to ghcid.")

(defvar ghcid-ghci-extra-opts '()
  "Extra opts to pass to the underlying GHCi invocation.")

(defconst ghcid--osc-title-regexp "\e\\]0;.*?\e\\\\"
  "Regexp matching OSC terminal title escape sequences.")

(defun ghcid--buffer-name ()
  "Return the ghcid buffer name.

If `ghcid-buffer-name' is non-nil, use it; otherwise use
`ghcid-default-buffer-name'."
  (or ghcid-buffer-name ghcid-default-buffer-name))

(defun ghcid--get-buffer-create ()
  "Return the ghcid buffer, creating it if needed.

The buffer name is determined from the output of `ghcid--buffer-name'."
  (get-buffer-create (ghcid--buffer-name)))

(defun ghcid--determine-build-system (&optional dir)
  "Heuristically determine the build system for DIR.

DIR defaults to `default-directory'. The return value is one of the
symbols `stack', `cabal', or `ghci', based on the presence of marker
files in DIR:

- `stack' if a \"stack.yaml\" file is present.
- `ghci' if a \".ghci\" file is present.
- `cabal' otherwise.

Note that these are the same heuristics that ghcid itself uses."
  (let ((default-directory (or dir default-directory)))
    (cond ((file-exists-p "stack.yaml")
           'stack)
          ((file-exists-p ".ghci")
           'ghci)
          (t
           'cabal))))

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
      (comint-exec (current-buffer) (ghcid--buffer-name) "ghcid" nil
                   (cons (ghcid--ghci-command)
                         (append ghcid-extra-opts ghcid-default-opts))))))

(defun ghcid-start ()
  "Start ghcid in a comint buffer and display it.

Create (or reuse) the ghcid buffer, enable `ghcid-mode', restart the
ghcid process, and display the buffer in a window."
  (interactive)
  (let ((buf (ghcid--get-buffer-create)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (ghcid-mode))
      (ghcid--exec-ghcid buf))
    (pop-to-buffer buf)))

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
