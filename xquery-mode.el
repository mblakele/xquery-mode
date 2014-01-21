;;; xquery-mode.el --- A simple mode for editing xquery programs
;; Time-stamp: <2010-08-10 12:15:14 mblakele>

;;; Copyright (C) 2005 Suraj Acharya
;;; Copyright (C) 2006-2012 Michael Blakeley

;; Authors:
;;   Suraj Acharya <sacharya@cs.indiana.edu>
;;   Michael Blakeley <mike@blakeley.com>

;; This file is not part of GNU Emacs.

;; xquery-mode.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;;


;;; History:
;;
;; 2011-10-08 mostly rewritten, knows about some MarkLogic extensions
;;
;; 2005-03-26 release by sacharya
;;   to http://www.emacswiki.org/cgi-bin/wiki/xquery-mode.el
;;

(require 'font-lock)

;; TODO 'if()' is highlighted as a function

;; TODO requiring nxml-mode excludes XEmacs - just for colors?
;; TODO test using featurep 'xemacs
(require 'nxml-mode)

;; TODO use nxml for element completion?

(define-generic-mode 'xquery-mode
  '()
  '()
  '() ;font-lock-list
  '(".xq\\'") ;auto-mode-list
  '(xquery-set-indent-function nil) ;function list
  "A Major mode for editing xquery.")

;; customization hook
(defcustom xquery-mode-hook nil
  "Hook run after entering XQuery mode."
  :type 'hook
  :options '(turn-on-xquery-indent turn-on-font-lock))

(defvar xquery-toplevel-bovine-table nil "Top level bovinator table")

(defvar xquery-mode-syntax-table () "Syntax table for xquery-mode")

(setq xquery-mode-syntax-table
  (let ((xquery-mode-syntax-table (make-syntax-table)))
    ;; single-quotes are equivalent to double-quotes
    (modify-syntax-entry ?' "\"" xquery-mode-syntax-table)
    ;; treat underscores as punctuation
    (modify-syntax-entry ?\_ "." xquery-mode-syntax-table)
    ;; treat hypens as punctuation
    (modify-syntax-entry ?\- "." xquery-mode-syntax-table)
    ;; colons are both punctuation and comments
    ;; the space after '.' indicates an unused matching character slot
    (modify-syntax-entry ?\: ". 23" xquery-mode-syntax-table)
    ;; XPath step separator / is punctuation
    (modify-syntax-entry ?/ "." xquery-mode-syntax-table)
    ;; xquery doesn't use backslash-escaping, so \ is punctuation
    (modify-syntax-entry ?\\ "." xquery-mode-syntax-table)
    ;; set-up the syntax table correctly for all the different braces
    (modify-syntax-entry ?\{ "(}" xquery-mode-syntax-table)
    (modify-syntax-entry ?\} "){" xquery-mode-syntax-table)
    (modify-syntax-entry ?\[ "(]" xquery-mode-syntax-table)
    (modify-syntax-entry ?\] ")]" xquery-mode-syntax-table)
    ;;(modify-syntax-entry ?\< "(" xquery-mode-syntax-table)
    ;;(modify-syntax-entry ?\> ")" xquery-mode-syntax-table)
    ;; parens may indicate a comment, or may be a sequence
    ;; note that (: will balance ), ( will balance ::), etc.
    ;; note 'n' for comment nesting
    (modify-syntax-entry ?\( "()1n" xquery-mode-syntax-table)
    (modify-syntax-entry ?\) ")(4n" xquery-mode-syntax-table)
    xquery-mode-syntax-table))

(defvar xquery-mode-keywords () "Keywords for xquery-mode")

(defvar xquery-mode-comment-start "(: "
  "String used to start an XQuery mode comment.")
;;(make-local-variable 'comment-start)


(defvar xquery-mode-comment-end " :)"
  "String used to end an XQuery mode comment.")


(defvar xquery-mode-comment-fill ":"
  "String used to fill an XQuery mode comment.")


(defvar xquery-mode-comment-start-skip "(:\\s-+"
  "Regexp to match an XQuery mode comment and any following whitespace.")


;; NOTE - derived-mode will automatically copy some vars
;;   xquery-map as keymap
;;   xquery-syntax-table as syntax-table
;;   xquery-abbrev-table as abbrev-table
;;   xquery-hook as initialization hook
;;;###autoload
(define-derived-mode xquery-mode fundamental-mode "XQuery"
  "A major mode for W3C XQuery 1.0"
  ;; indentation
  (set (make-local-variable 'indent-line-function) 'xquery-indent-line)
  ;; apparently it's important to set at least an empty list up-front
  (set (make-local-variable 'font-lock-defaults)
    (list (list ())))
  (set (make-local-variable 'comment-start) xquery-mode-comment-start)
  (set (make-local-variable 'comment-end) xquery-mode-comment-end)
  (set (make-local-variable 'comment-fill)  xquery-mode-comment-fill)
  (set (make-local-variable 'comment-start-skip) xquery-mode-comment-start-skip)
  )

;; XQuery doesn't have keywords, but these usually work...
;; TODO remove as many as possible, in favor of parsing
(setq xquery-mode-keywords
  (list
    ;; FLWOR
    ;;"let" "for"
    "at" "in"
    "where"
    "stable order by" "order by"
    "ascending" "descending" "empty" "greatest" "least" "collation"
    "return"
    ;; XPath axes
    "self" "child" "descendant" "descendant-or-self"
    "parent" "ancestor" "ancestor-or-self"
    "following" "following-sibling"
    "preceding" "preceding-sibling"
    ;; conditionals
    "if" "then" "else"
    "typeswitch" ;"case" "default"
    ;; quantified expressions
    "some" "every" "construction" "satisfies"
    ;; schema
    "schema-element" "schema-attribute" "validate"
    ;; operators
    "intersect" "union" "except" "to"
    "is" "eq" "ne" "gt" "ge" "lt" "le"
    "or" "and"
    "div" "idiv" "mod"
    ))

;; to match only word-boundaries, we turn the keywords into a big regex
(defvar xquery-mode-keywords-regex () "Keywords regex for xquery mode")

;; transform the list of keywords into regex
;; check for word-boundaries instead of whitespace
(setq xquery-mode-keywords-regex
  (concat (concat "\\b\\("
            (mapconcat
              (function (lambda (r)
                          (if (string-match "[ \t]+" r)
                            (replace-match "[ \t]+" nil t r) r)))
              xquery-mode-keywords "\\|"))
    "\\)\\b"))

;;(message xquery-mode-keywords-regex)

;; XQuery syntax - TODO build a real parser
(defvar xquery-mode-ncname () "NCName regex, in 1 group")
(setq xquery-mode-ncname "\\(\\sw[-_\\.[:word:]]*\\)")

;; highlighting needs a group, even if it's "" - so use (...?) not (...)?
;; note that this technique treats the local-name as optional,
;; when the prefix should be the optional part.
(defvar xquery-mode-qname () "QName regex, in 3 groups")
(setq xquery-mode-qname
  (concat
    xquery-mode-ncname "\\(:?\\)" "\\(" xquery-mode-ncname "?\\)"))

;; highlighting
;; these are "matcher . highlighter" forms
(font-lock-add-keywords
  'xquery-mode
  `(
     ;; prolog version decl
     ("\\(xquery\\s-+version\\)\\s-+"
       (1 font-lock-keyword-face))
     ;; namespace default decl for 0.9 or 1.0
     (,(concat
         "\\(\\(declare\\)?"
         "\\(\\s-+default\\s-+\\(function\\|element\\)\\)"
         "\\s-+namespace\\)\\s-+")
       (1 font-lock-keyword-face))
     ;; namespace decl
     (,(concat
         "\\(declare\\s-+namespace\\)\\s-+")
       (1 font-lock-keyword-face))
     ;; option decl
     (,(concat "\\(declare\\s-+option\\s-+" xquery-mode-qname "\\)")
       (1 font-lock-keyword-face))
     ;; import module decl - must precede library module decl
     ("\\(import\\s-+module\\)\\s-+\\(namespace\\)?\\s-+"
       (1 font-lock-keyword-face)
       (2 font-lock-keyword-face))
     ;; library module decl, for 1.0 or 0.9-ml
     ("\\(module\\)\\s-+\\(namespace\\)?\\s-*"
       (1 font-lock-keyword-face)
       (2 font-lock-keyword-face))
     ;; import schema decl
     ("\\(import\\s-+schema\\)\\s-+\\(namespace\\)?\\s-+"
       (1 font-lock-keyword-face)
       (2 font-lock-keyword-face))
     ;; variable decl
     ("\\(for\\|let\\|declare\\s-+variable\\|define\\s-+variable\\)\\s-+\\$"
       (1 font-lock-keyword-face))
     ;; variable name
     (,(concat "\\($" xquery-mode-qname "\\)")
       (1 font-lock-variable-name-face))
     ;; function decl
     (,(concat
         "\\(declare\\s-+function\\"
         "|declare\\s-+private\\s-+function\\"
         "|define\\s-+function\\)\\s-+\\("
         xquery-mode-qname "\\)(")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face))
     ;; schema test or type decl
     (,(concat
         "\\("
         "case"
         "\\|instance\\s-+of\\|castable\\s-+as\\|treat\\s-+as\\|cast\\s-+as"
         ;; "as" must be last in the list
         "\\|as"
         "\\)"
         "\\s-+\\(" xquery-mode-qname "\\)"
         ;; type may be followed by element() or element(x:foo)
         "(?\\s-*\\(" xquery-mode-qname "\\)?\\s-*)?")
       (1 font-lock-keyword-face)
       (2 font-lock-type-face)
                                        ; TODO the second qname never matches
       (3 font-lock-type-face))
     ;; function call
     (,(concat "\\(" xquery-mode-qname "\\)(")
       (1 font-lock-function-name-face))
     ;; named node constructor
     (,(concat "\\(attribute\\|element\\)\\s-+\\(" xquery-mode-qname "\\)\\s-*{")
       (1 font-lock-keyword-face)
       (2 font-lock-constant-face))
     ;; anonymous node constructor
     ("\\(binary\\|comment\\|document\\|text\\)\\s-*{"
       (1 font-lock-keyword-face))
     ;; typeswitch default
     ("\\(default\\s-+return\\)\\s-+"
       (1 font-lock-keyword-face)
       (2 font-lock-keyword-face))
     ;;
     ;; highlighting - use nxml config to font-lock directly-constructed XML
     ;;
     ;; xml start element start
     (,(concat "<" xquery-mode-qname)
       (1 'nxml-element-prefix-face)
       (2 'nxml-element-colon-face)
       (3 'nxml-element-prefix-face))
     ;; xml start element end
     ("\\(/?\\)>"
       (1 'nxml-tag-slash-face))
     ;; xml end element
     (,(concat "<\\(/\\)" xquery-mode-qname ">")
       (1 'nxml-tag-slash-face)
       (2 'nxml-element-prefix-face)
       (3 'nxml-element-colon-face)
       (4 'nxml-element-local-name-face))
     ;; TODO xml attribute or xmlns decl
     ;;    (,(concat xquery-mode-qname "=\\([\"']\\)\\(.*?\\)\\([\"']\\)")
     ;;     (1 'nxml-attribute-prefix-face)
     ;;     (2 'nxml-attribute-colon-face)
     ;;     (3 'nxml-attribute-local-name-face)
     ;;     (4 'nxml-attribute-value-delimiter-face)
     ;;     (5 'nxml-attribute-value-face)
     ;;     (6 'nxml-attribute-value-delimiter-face))
     ;; xml comments
     ("\\(<!--\\)\\([^-]*\\)\\(-->\\)"
       (1 'nxml-comment-delimiter-face)
       (2 'nxml-comment-content-face)
       (3 'nxml-comment-delimiter-face))
     ;; highlighting XPath expressions, including *:foo
     ;; TODO this doesn't match expressions unless they start with slash
     ;; TODO but matching without a leading slash overrides all the keywords
     (,(concat "\\(//?\\)\\(*\\|\\sw*\\)\\(:?\\)" xquery-mode-ncname)
       (1 font-lock-constant-face)
       (2 font-lock-constant-face)
       (3 font-lock-constant-face)
       (4 font-lock-constant-face))
     ;;
     ;; highlighting pseudo-keywords - must be late, for problems like 'if ()'
     ;;
     (,xquery-mode-keywords-regex (1 font-lock-keyword-face))
     ))

;; file-extension mappings
;;;###autoload
(add-to-list 'auto-mode-alist '(".xq[erxy]\\'" . xquery-mode))

(defun xquery-forward-sexp (&optional arg)
  "XQuery forward s-expresssion.
This function is not very smart. It tries to use
`nxml-forward-balanced-item' if it sees '>' or '<' characters in
the current line (ARG), and uses the regular `forward-sexp'
otherwise."
  (if (> arg 0)
    (progn
      (if (looking-at "\\s-*<")
        (nxml-forward-balanced-item arg)
        (let ((forward-sexp-function nil)) (forward-sexp arg))))
    (if (looking-back ">\\s-*")
      (nxml-forward-balanced-item arg)
      (let ((forward-sexp-function nil)) (forward-sexp arg)))))

;; indentation
(defvar xquery-indent-size tab-width "The size of each indent level.")

;; (setq debug-on-error t) ;\ DEBUG ::)

(defvar xquery-indent-debug nil)

;; (setq xquery-indent-debug t) ;\ DEBUG ::)

(defun xquery-toggle-debug-indent ()
  "Toggle the debug flag used in `xquery-calculate-indentation'."
  (interactive)
  (setq xquery-indent-debug (not xquery-indent-debug))
  (message "xquery-indent-debug is %sabled"
    (if xquery-indent-debug "en" "dis")))

(defun xquery-indent-debug-toggle ()
  "Toggle the debug flag used in `xquery-calculate-indentation'."
  (interactive) (xquery-toggle-debug-indent))

(defun xquery-indent-debug-message (results)
  "Utility function to display debug messages for indentation.
RESULTS must be a list of a column number and a string message."
  (if xquery-indent-debug
    (let ((cc (car results))
           (msg (cdr results)))
      (message "xquery-indent-debug: (%d) %S" cc msg)) ) )

(defun xquery-set-indent-function ()
  "Set the indent function for xquery mode."
  (setq nxml-prolog-end (point-min))
  (setq nxml-scan-end (copy-marker (point-min) nil))
  (set (make-local-variable 'indent-line-function) 'xquery-indent-line)
  (make-local-variable 'forward-sexp-function)
  (setq forward-sexp-function 'xquery-forward-sexp)
  (local-set-key "/" 'nxml-electric-slash))

(defun xquery-indent-line ()
  "Indent current line as xquery code."
  (interactive)
  (let ((savept (> (current-column) (current-indentation)))
         (results (xquery-calculate-indentation)))
    (xquery-indent-debug-message results)
    (let ( (indent (car results)) )
      (if (> indent -1)
        (if savept
          (save-excursion (indent-line-to indent))
          (indent-line-to (max 0 indent)) ) ) ) ) )

(defun xquery-indent-via-nxml ()
  "This function uses nxml to calculate the indentation."
  (let ((nxml-prolog-end (point-min))
         (nxml-scan-end (copy-marker (point-min) nil)) )
    (nxml-compute-indent) ) )

;; to make debugging easier, use setq to set the actual values
(defvar xquery-indent-regex ""
  "A regular expression indicating an indentable xquery sub-expression.")

(setq xquery-indent-regex
  (concat "^\\s-*\\("
    "typeswitch\\|for\\|let\\|where\\|order\\s-+by\\|return"
    "\\|if\\|then\\|else"
    "\\)\\s-*$") )

(defun xquery-calculate-indentation ()
  "Calculate the indentation for a line of XQuery.
This function returns the column to which the current line should be indented,
and a debug expression."
  (save-excursion
    (beginning-of-line)
    (cond

      ;; TODO this sort of works, but needs to set some state
      ;; TODO once we have state, how and when do we reset it?
      ;;      ((save-excursion
      ;;         (previous-line)
      ;;         (message "current-word = %S" (current-word)) ; DEBUG
      ;;         (message "looking-at xquery-indent-regex = %S"
      ;;                  (looking-at xquery-indent-regex)) ; DEBUG
      ;;          (looking-at xquery-indent-regex))
      ;;       (save-excursion
      ;;         (previous-line)
      ;;         (list
      ;;          (+ xquery-indent-size (current-indentation))
      ;;          "previous line starts new block")))

      ;; default, using sexp parser
      (t
        ;; calculate indent for beginning of line indent, then end of line
        (let* ((point-bol (point))
                (results-bol (parse-partial-sexp (point-min) point-bol))
                ;; 0. depth in parens.
                (paren-level-bol (car results-bol))
                ;; 1. character address of start of innermost containing list.
                (list-start-bol  (car (cdr results-bol)))
                ;; 2. character address of start of last complete sexp.
                (sexp-start-bol  (car (cdr (cdr results-bol))) )
                ;; 3. non-nil if inside a string.
                (stringp-bol     (car (cdr (cdr (cdr results-bol)))) )
                ;; 4. nil if outside comment, t if inside non-nesting comment,
                ;;    else integer comment nesting.
                (comment-level-bol
                  (car (cdr (cdr (cdr (cdr results-bol))))) )
                ;; 5. t if following a quote character.
                (quotep-bol
                  (car (cdr (cdr (cdr (cdr (cdr results-bol)))))) )
                ;; 6. the minimum paren-depth encountered during this scan.
                (min-level-bol
                  (car (cdr (cdr (cdr (cdr (cdr (cdr results-bol))))))) )
                ;; 7. t if in a comment of style b;
                ;;    symbol 'syntax-table' if the comment is generic.
                (bcommentp-bol
                  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr results-bol)))))))) )
                ;; 8. character address of start of comment or string, else nil.
                (comment-start-bol
                  (car (cdr (cdr
                              (cdr (cdr (cdr (cdr (cdr
                                                    (cdr results-bol))))))))))
                ;; 9. intermediate data for continuation of parsing. (not used)

                (point-eol (save-excursion (end-of-line) (point)))
                ;; undocumented, but parse-partial-sexp seems to change point
                ;; TODO use state-bol? seems to have problems
                (results-eol (save-excursion
                               (parse-partial-sexp (point-min) point-eol)))
                ;; what would nxml do?
                (results-nxml
                  (cond
                    ((looking-at "\\s-*<!--")
                      (list (xquery-indent-via-nxml) "xml start-comment"))
                    ((looking-at "\\s-*-->")
                      (list (xquery-indent-via-nxml) "xml end-comment"))
                    ((looking-at "\\s-*<\\sw+")
                      (list (xquery-indent-via-nxml) "xml start-element"))
                    ((looking-at "\\s-*</?\\sw+")
                      (list (xquery-indent-via-nxml) "xml end-element"))
                    (t nil) ) )
                ;; later we will multiple by xquery-indent-size
                (nxml-indent
                  (if results-nxml
                    (/ (car results-nxml) xquery-indent-size)))
                )
          (if xquery-indent-debug
            (progn
              (message "point-bol = %S" point-bol)
              (message "point-eol = %S" point-eol)
              (message "point = %S" (point))
              (message "results-eol = %S" results-eol)
              (message "results-nxml = %S" results-nxml)))
          (let* (
                  ;; 0. depth in parens
                  (paren-level-eol (car results-eol))
                  (indent
                    (cond
                      (comment-level-bol
                                        ; within a multi-line comment
                                        ; start of comment indentation + 1
                        (+ 1 (save-excursion
                               (goto-char comment-start-bol)
                               (current-indentation) )) )
                                        ; TODO multi-line prolog variable?
                      (nil -1)
                                        ; mult-line module import?
                      ((and (save-excursion
                              (beginning-of-line)
                              (looking-at "^\\s-*at\\s-+"))
                         (save-excursion
                           (beginning-of-line)
                           (previous-line)
                           (looking-at "^\\s-*import\\s-+module\\s-+")))
                        xquery-indent-size)
                                        ; multi-line function decl?
                                        ; TODO handle more than 1 line previous
                      ((and (save-excursion
                              (beginning-of-line)
                              (looking-at "^\\s-*as\\s-+"))
                         (save-excursion
                           (beginning-of-line)
                           (previous-line)
                           (looking-at
                             "^\\s-*\\(define\\|declare\\)\\s-+function\\s-+")))
                        xquery-indent-size)
                                        ; default - use paren-level-bol
                      (t (* xquery-indent-size
                                        ; special when simply closing 1 level
                           (cond
                             ((and (= paren-level-bol (+ 1 paren-level-eol))
                                (looking-at "^\\s-*\\s)[,;]?\\s-*$") )
                               paren-level-eol)
                                        ; factor in the nxml-indent
                             ((and
                                nxml-indent (> nxml-indent paren-level-bol))
                               nxml-indent)
                             (t paren-level-bol)))))))
            (list (min 70 indent) results-bol results-eol)))))))

(provide 'xquery-mode)

;;; xquery-mode.el ends here
