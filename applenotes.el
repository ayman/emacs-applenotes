(defun anotes--get-account-list ()
  "docstring"
  (do-applescript
   "tell application \"Notes\"
	set noteList to \"\"
	repeat with f in every folder
		set noteList to noteList & name of (container of f) & \"\t\" & name of f & \"\t\" & id of f & \"\t\" & \"\n\"
	end repeat
	return noteList
   end tell"))

(defun anotes--get-notes-list (location)
  "docstring"
  (do-applescript (concat
   "tell application \"Notes\"
	set noteList to \"\"
	repeat with n in notes of folder id \"" location "\"
		set noteList to noteList & name of n & \"\t\" & id of n & \"\t\" & modification date of n & \"\n\"
	end repeat
    end tell")))

(defun anotes--get-note-body (location)
  "docstring"
  (do-applescript (concat
   "tell application \"Notes\"
	set n to note id \"" location "\"
        return body of n
    end tell")))

(defun anotes--set-note-body (location body)
  "docstring"
  (do-applescript (concat
   "tell application \"Notes\"
	set n to note id \"" location "\"
        set body of n to \"" body "\"        
    end tell")))

(defun anotes-account-list ()
  "Show the list of list of notes"
  (interactive)
  (let* ((notes-list-raw (anotes--get-account-list))
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
                                   (anotes--notes-list
                                    (button-get b 'link)
                                    (button-get b 'name)
                                    (button-get b 'parent))))
          (insert "\n")
          (setq lines (cdr lines))))
      (goto-char (point-min))
      (read-only-mode))))

(defun anotes--notes-list (folder name parent)
  "Show the list of list of notes"
  (interactive)
  (let* ((notes-list-raw (anotes--get-notes-list folder))
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
                                   (anotes--note-open
                                    (button-get b 'link)
                                    (button-get b 'name))))
          (insert "\n")
          (setq lines (cdr lines))))
      (goto-char (point-min))
      (read-only-mode))))

;; save the location somewhere hidden if we can ya?
(defun anotes--note-open (location title)
  (let* ((note-body-raw (anotes--get-note-body location))
         (note-body (substring note-body-raw 1 -2))
         (note-buffer-name (concat title " Apple Note"))
         (note-buffer (get-buffer-create note-buffer-name)))
    (with-current-buffer note-buffer
      (switch-to-buffer note-buffer)
      (setq major-mode 'markdown-mode)
      (set (make-local-variable 'anotes--is-note) 't)
      (set (make-local-variable 'anotes--loc) location)
      (set (make-local-variable 'anotes--name) title)
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
      (local-set-key "\C-x\C-s" 'anotes--note-save)
      (goto-char (point-min))
      (not-modified))))

(defun anotes--note-save ()
  (interactive)
  (when (local-variable-if-set-p 'anotes--is-note)
    (anotes--set-note-body anotes--loc
                           (anotes--make-html-from-md (buffer-string)))
    (not-modified)
        (message (concat "Saved Apple Note: " anotes--name))))

(defun anotes--make-html-from-md (md)
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
