;;; ghcid.el --- ghcid support in Emacs with compilation-mode -*- lexical-binding: t -*-

;; Copyright (C) 2026 Nicholas B. Hubbard <nicholashubbard@posteo.net>
;;
;; Licensed under the same terms as Emacs and under the MIT license.

;; SPDX-License-Identifier: MIT

;; Author: Nicholas B. Hubbard <nicholashubbard@posteo.net>
;; URL: https://github.com/NicholasBHubbard/ghcid.el
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.0
;; Created: 2026-02-05
;; By: Nicholas B. Hubbard <nicholashubbard@posteo.net>
;; Keywords: tools, files, Haskell

;;; Commentary:

;; Use M-x ghcid to launch

;;; Code:

(require 'compile)
(require 'comint)

(defcustom ghcid-default-opts nil
  "TODO"
  )

(defcustom ghcid-extra-opts nil
  "TODO"
  )

(defcustom ghcid-buffer-name nil
  "TODO")

(defcustom ghcid-buffer-maximum-lines 500
  "TODO")

(defun ghcid--buffer-name (&rest _)
  "TODO"
  (or ghcid-buffer-name "*ghcid*"))

(defun ghcid--command ()
  "Return a string containing a shell command to run ghcid."
  (mapconcat #'shell-quote-argument
             (cons "ghcid" (append ghcid-extra-opts ghcid-default-opts)) " "))

(defun ghcid--get-buffer-create ()
  "TODO"
  (get-buffer-create (ghcid--buffer-name)))

(defun ghcid-start ()
  "TODO"
  (interactive)
  (when-let ((buf (ghcid--get-buffer-create)))
    (with-current-buffer buf
      (unless ghcid-mode
        (let ((compilation-buffer-name-function #'ghcid--buffer-name))
          (compile (ghcid--command) t))
        (ghcid-mode 1)))
    buf))

(define-minor-mode ghcid-mode
  "Toggle `ghcid-mode'."
  :global nil
  :group 'ghcid-mode
  :require 'ghcid-mode
  (if ghcid-mode
      (progn
        (setq-local comint-buffer-maximum-size ghcid-buffer-maximum-lines)
        (add-hook 'comint-output-filter-functions
                  #'comint-truncate-buffer nil t))
    (remove-hook 'comint-output-filter-functions
                 #'comint-truncate-buffer t)))

(provide 'ghcid)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; ghcid.el ends here
