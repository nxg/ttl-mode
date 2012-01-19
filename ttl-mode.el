;;; ttl-mode.el --- mode for Turtle(RDF)
;; Based on n3-mode.el --- mode for Notation 3,
;; at http://larve.net/people/hugo/2003/scratchpad/NotationThreeEmacsMode.html
;; Also draws on http://dishevelled.net/elisp/turtle-mode.el (which is for the _other_ turtle!)

;; Copyright (c) 2003-2007 Hugo Haas <hugo@larve.net>
;; Extended 2011-2012, by Norman Gray <http://nxg.me.uk>
;;
;; See Hugo's commentary for original goals and further discussion.

;; Project hosted at <https://bitbucket.org/nxg/ttl-mode>.  See there for updates.

;; To use:
;;
;; (autoload 'ttl-mode "ttl-mode" "Major mode for OWL or Turtle files" t)
;; (add-hook 'ttl-mode-hook    ; Turn on font lock when in ttl mode
;;           'turn-on-font-lock)
;; (setq auto-mode-alist
;;       (append
;;        (list
;;         '("\\.n3" . ttl-mode)
;;         '("\\.ttl" . ttl-mode))
;;        auto-mode-alist))

;;; Code:

(require 'generic)

(define-generic-mode 'ttl
  ;; comment char
  (list "# ")
  ;; keywords
  (list "this" "a")
  ;; additional font-lock'ing
  '(("\\(@prefix\\)\\>" 1 font-lock-keyword-face t)   ;keywords
    ("\\^\\^[^,;.]+" 0 font-lock-preprocessor-face t) ;literal types
    ("@[[:word:]_]+" 0 font-lock-preprocessor-face t) ;languages
    ("\\(\\S-*?:\\)" 1 font-lock-type-face nil)       ;prefix
    (":\\([[:word:]_-]+\\)\\>" 1 font-lock-constant-face nil) ;suffix
    ("\\(<.*?>\\)" 1 font-lock-function-name-face t) ;resources
    ("\\(\\\".*?\\\"\\)" 1 font-lock-string-face t)  ;strings
    ("\\(\\\"\\\"\\\".*?\\\"\\\"\\\"\\)" 1 font-lock-string-face t) ;doesn't work over newlines?
; Bug: some trailing characters are highlighted; restricting comments regexp
;    ("\\(#.*\\)" 1 font-lock-comment-face t)
    ("^\\s-*\\(#.*\\)" 1 font-lock-comment-face t) ;comment
    )
  ;; auto-mode
  (list "\\.n3$" "\\.ttl")
  ;; additional setup
  nil
  ;; description
  "Mode for Turtle RDF documents."
  )

(defun ttl-base ()
  (interactive)
  (generic-mode "ttl"))

(defun ttl-indent-line ()
  (interactive)
  (indent-line-to
   (or (ignore-errors (ttl-calculate-indentation))
       0)))

(defun ttl-calculate-indentation ()
  (save-excursion
    (beginning-of-line) (skip-chars-forward "\t ")
    (cond ((ttl-in-string-p) 8)
          ((looking-at "$")
           ;; empty line -- use same indentation as previous line
           (save-excursion
             (forward-line -1)
             (skip-chars-forward "\t ")
             (current-column)))
          ((looking-at "@") 0)        ;@prefix or @base
          ((looking-at "#") 0)
          ((looking-at "<");(looking-at "\\S-+\\s-*$")
           ;;only a single expression on the line -- a subject
           0)
          ((save-excursion
             (forward-line -1)
             (end-of-line)
             (if (not (looking-back "\\["))
                 nil
               (beginning-of-line)
               (skip-chars-forward "\t ")
               (+ (current-column) 4))))
          (t 4))))

;; (defun turtle-indent-block ()
;;   (interactive)
;;   (indent-region (point)
;;                  (save-excursion (forward-xxx) (point))))

(defvar ttl-mode-map (make-sparse-keymap))
(define-derived-mode ttl-mode ttl-base
  "Turtle RDF"
  (setq indent-tabs-mode nil)
  (ttl-mode-variables)
;  (define-key ttl-mode-map (kbd "C-M-q")
;    'turtle-indent-block)
  (define-key ttl-mode-map (kbd "\;") 'ttl-electric-semi)
  (define-key ttl-mode-map (kbd "\.") 'ttl-electric-dot)
  (define-key ttl-mode-map [backspace] 'ttl-hungry-delete-backwards)
  (use-local-map ttl-mode-map))

(defcustom ttl-electric-semi-mode t
  "*If non-nil, `\;' will self insert, reindent the line, and do a newline.
If nil, just insert a `\;'.  (To insert while t, do: \\[quoted-insert] \;)."
  :group 'ttl
  :type 'boolean)

(defun beginning-of-stanza ()
  "Find the beginning of a stanza, indicated by non-whitespace at the beginning of a line."
  (re-search-backward "^\\S-" (point-min) t))

(defun ttl-in-string-p ()
  "Is point inside a string or a long-string?"
  (save-excursion
    (let ((here (point))
          (in-p nil))
      (beginning-of-stanza)
      (condition-case nil
          (progn
            (while (<= (point) here)
              (if in-p
                  (progn
                    (search-forward in-p (point-max) nil)
                    (setq in-p nil))
                (re-search-forward "\"\\(\"\"\\)?" (point-max) nil)
                (if (char-equal (char-before (- (point) 1)) ?\")
                    (setq in-p "\"\"\"")
                  (setq in-p "\""))))
            (not in-p))
        (search-failed                  ;reached EOF
         (if in-p                       ;this indicates that we're inside a string
             (message "Unbalanced quotes -- reached EOF inside string")
           nil))))))

(defun *ttl-search-in-line-for-comment (limit)
  "Search for a comment character from the current position, before point LIMIT; changes current position.  Return location of comment, or nil if none can be found"
  (let ((new-point (re-search-forward "[#<]" limit t)))
    (cond ((not new-point) nil)
          ((looking-back "#") new-point) ;found comment
          (t                            ;looking-back "<"
           (if (not (search-forward ">" limit t))
               nil                                        ;starting point is within resource
             (*ttl-search-in-line-for-comment limit)))))) ;recurse

(defun ttl-in-comment-p ()
  "Is point inside a comment?"
  (save-excursion
    (let ((here (point)))
      (beginning-of-line)
      (*ttl-search-in-line-for-comment here))))

(defun ttl-in-resource-p ()
  "Is point within a resource, marked by <...>?"
  (save-excursion
    (and (re-search-backward "[<> ]" nil t)
         (looking-at "<"))))

(defun ttl-skip-ws-backwards ()  ;adapted from cc-mode
  "Move backwards across whitespace."
  (while (progn
           (skip-chars-backward " \t\n\r\f\v")
           (and (eolp)
                (eq (char-before) ?\\)))
    (backward-char)))

(defun ttl-hungry-delete-backwards ()
  "Delete backwards, either all of the preceding whitespace, 
or a single non-whitespace character if there is no whitespace before point."
  (interactive)
  (let ((here (point)))
    (ttl-skip-ws-backwards)
    (if (/= (point) here)
        (delete-region (point) here)
      (backward-delete-char-untabify 1))))

(defun ttl-insulate ()
  "Return true if this location should not be electrified"
  (or (ttl-in-string-p)
      (ttl-in-comment-p)
      (ttl-in-resource-p)))

(defun ttl-electric-semi ()
  "Insert a \;.
If variable `ttl-electric-semi-mode' is t, indent the current line, insert
a newline, and indent."
  (interactive)
  (insert "\;")
  (if (and ttl-electric-semi-mode
           (not (ttl-insulate)))
      (reindent-then-newline-and-indent)))

(defun ttl-electric-dot ()
  "Insert a \..
If variable `ttl-electric-semi-mode' is t, indent the current line, insert
a newline, and indent."
  (interactive)
  (insert "\.")
  (if (and ttl-electric-semi-mode
           (not (ttl-insulate)))
      (reindent-then-newline-and-indent)))

(defun ttl-mode-variables ()
  (set (make-local-variable 'indent-line-function) 'ttl-indent-line))
