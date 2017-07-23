;;; applenotes.el --- Use Apple Notes in Emacs through AppleScript

;; Copyright (C) 2017 David A. Shamma

;; Author: David A. Shamma
;; Version: 0.1
;; Keywords: apple,notes emacs-applenotes
;; Package-Requires: ((emacs "24"))
;; URL: http://github.com/ayman/emacs-applenotes

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package wraps Apple Notes into elisp via AppleScript.

;;; Code:
(defgroup applenotes nil
  "Interact with Apple Notes through emacs."
  :group 'tools
  :group 'convenience)

(defcustom applenotes-default-account "iCloud"
  "Default account to store notes."
  :group 'applenotes
  :type 'string)

(defvar applenotes-mode-hook nil)

(defvar applenotes-mode-map
  (let ((map (make-keymap)))
    (define-key map "q" 'kill-this-buffer)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    map)
  "Keymap for AppleNotes major mode")

(defface applenotes-title-face
  '((t :inherit button))
  "Face for a title string in buffers."
  :group 'applenotes-mode)

(defface applenotes-list-face
  '((t :inherit button :bold semi-bold :underline nil))
  "Face for a highlighted list."
  :group 'applenotes-mode)

(defface applenotes-extra-keywords-face
  '((t :inherit font-lock-builtin-face))
  "Face for extra highlights."
  :group 'applenotes-mode)

(defconst applenotes-font-lock-keywords-1
  (list
   '("\\(^\w+$\\)" . font-lock-constant-face))
  "Minimal highlighting keywords for applenotes mode")

(defconst applenotes-font-lock-keywords-2
  (append applenotes-font-lock-keywords-1
          (list '("\\(^ \+.*\\)" . applenotes-list-face)))
  "Additional Keywords to highlight in applenotes mode")

(defconst applenotes-font-lock-keywords-3
  (append applenotes-font-lock-keywords-2
          (list '(" : \\(.+\\)$" . applenotes-extra-keywords-face)))
  "Additional Keywords to highlight in applenotes mode")

(defvar applenotes-font-lock-keywords applenotes-font-lock-keywords-3
  "Default highlighting expressions for applenotes mode")

(defun applenotes-mode ()
  "Major mode for navigation Apple Notes mode listings."
  (kill-all-local-variables)
  (use-local-map applenotes-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(applenotes-font-lock-keywords))
  (setq major-mode 'applenotes-mode)
  (setq mode-name "Apple Notes")
  (run-hooks 'applenotes-mode-hook))

(provide 'applenotes-mode)

;;;###autoload
(defun applenotes--get-account-list ()
  "docstring"
  (do-applescript
   "tell application \"Notes\"
	set noteList to \"\"
	repeat with f in every folder
		set noteList to noteList & name of (container of f) & \"\t\" & name of f & \"\t\" & id of f & \"\t\" & \"\n\"
	end repeat
	return noteList
   end tell"))

(defun applenotes--get-notes-list (location)
  "docstring"
  (do-applescript (concat
   "tell application \"Notes\"
	set noteList to \"\"
	repeat with n in notes of folder id \"" location "\"
		set noteList to noteList & name of n & \"\t\" & id of n & \"\t\" & modification date of n & \"\n\"
	end repeat
    end tell")))

(defun applenotes--get-note-body (location)
  "docstring"
  (do-applescript (concat
   "tell application \"Notes\"
	set n to note id \"" location "\"
        return body of n
    end tell")))

(defun applenotes--set-note-body (location body)
  "docstring"
  (do-applescript (concat
   "tell application \"Notes\"
	set n to note id \"" location "\"
        set body of n to \"" body "\"        
    end tell")))

(defun applenotes-account-list ()
  "Show the list of list of notes"
  (interactive)
  (let* ((notes-list-raw (applenotes--get-account-list))
         (notes-list (substring notes-list-raw 1 -2))
         (lines (split-string notes-list "\n"))
         (accounts-buffer-name "*Apple Notes Accounts List*")
         (accounts-buffer (get-buffer-create accounts-buffer-name)))
    (with-current-buffer accounts-buffer
      (switch-to-buffer accounts-buffer)
      (read-only-mode 0)
      (erase-buffer)
      (setq last-account "")
      (while lines
        (let* ((l (car lines))
               (ll (split-string l "\t"))
               (account (car ll))
               (notebook (cadr ll))
               (location (caddr ll)))
          (when (not (string= last-account account))              
              (insert (concat account "\n"))
              (setq last-account account))
          (insert " + ")
          (insert-button notebook
                         'follow-link t
                         'help-echo (concat "Open notes in "
                                             account ":" notebook)
                         'name notebook
                         'link location
                         'parent account
                         'action (lambda (b)
                                   (applenotes--notes-list
                                    (button-get b 'link)
                                    (button-get b 'name)
                                    (button-get b 'parent))))
          (insert "\n")
          (setq lines (cdr lines))))
      (goto-char (point-min))
      (read-only-mode)
      (applenotes-mode))
    (other-window 1)))

(defun applenotes--notes-list (folder name parent)
  "Show the list of list of notes"
  (interactive)
  (let* ((notes-list-raw (applenotes--get-notes-list folder))
         (notes-list (substring notes-list-raw 1 -2))
         (lines (split-string notes-list "\n"))
         (notes-buffer-name (concat "*Apple Notes " parent "-" name " List*")))
    (with-current-buffer (get-buffer-create notes-buffer-name)
      (switch-to-buffer notes-buffer-name)
      (read-only-mode 0)
      (erase-buffer)
      (while lines        
        (let* ((l (car lines))
               (ll (split-string l "\t"))
               (title (car ll))
               (location (cadr ll))
               (mod-date (caddr ll)))
          (insert " + ")
          (insert-button title
                         'follow-link t
                         'help-echo (concat "Modified: " mod-date)
                         'name title
                         'link location
                         'action (lambda (b)
                                   (applenotes--note-open
                                    (button-get b 'link)
                                    (button-get b 'name))))
          (insert "\n")
          (setq lines (cdr lines))))
      (goto-char (point-min))
      (read-only-mode)
      (applenotes-mode))
    (other-window 1)))

;; save the location somewhere hidden if we can ya?
(defun applenotes--note-open (location title)
  (let* ((note-body-raw (applenotes--get-note-body location))
         (note-body (substring note-body-raw 1 -2))
         (note-buffer-name (concat title " Apple Note"))
         (note-buffer (get-buffer-create note-buffer-name)))
    (with-current-buffer note-buffer
      (switch-to-buffer note-buffer)
      (setq major-mode 'markdown-mode)
      (set (make-local-variable 'applenotes--is-note) 't)
      (set (make-local-variable 'applenotes--loc) location)
      (set (make-local-variable 'applenotes--name) title)
      (display-buffer note-buffer-name)
      (read-only-mode 0)
      (erase-buffer)
      (insert (s-replace
               "\n\n" "\n"
               (s-replace
                "</div>" "\n"
                (s-replace
                 "<div>" ""
                 (s-replace
                  "</div> " "</div>"
                  (html-to-markdown-string
                   (substring note-body)))))))
      (local-set-key "\C-x\C-s" 'applenotes--note-save)
      (goto-char (point-min))
      (not-modified))
    (other-window 1)))

(defun applenotes--note-save ()
  (interactive)
  (when (local-variable-if-set-p 'applenotes--is-note)
    (applenotes--set-note-body applenotes--loc
                           (applenotes--make-html-from-md (buffer-string)))
    (not-modified)
        (message (concat "Saved Apple Note: " applenotes--name))))

(defun applenotes--make-html-from-md (md)
  (let* ((html (s-replace "\n" "</div>\n<div>" md))
         (html (concat "<div>" html "</div>"))
         (html (s-replace " *" " <b>" html ))
         (html (s-replace "* " "<\b> " html ))
         (html (s-replace "*. " "<\b>. " html ))
         (html (s-replace "*? " "<\b>? " html ))
         (html (s-replace "*! " "<\b>! " html ))
         (html (s-replace " _" " <i>" html ))
         (html (s-replace "_ " " <\i>" html ))
         (html (s-replace "_. " " <\i>." html ))
         (html (s-replace "_? " " <\i>?" html ))
         (html (s-replace "_! " " <\i>!" html )))
    html))

(provide 'applenotes)
;;; applenotes.el ends here
