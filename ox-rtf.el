;;; ox-rtf.el -- Probably not an RTF backend for Org
;;; Borrowing heavily from John Kitchin's Scimax `ox-rtf.el' at
;;;
;;;    https://github.com/jkitchin/scimax/blob/master/ox-rtf.el
;;;

;; (require 'ox-org)
(require 'ox)

(defun org-rtf-bold (bold contents info)
  (format "{\\b %s}" contents))

(defun org-rtf-italic (el contents info)
  (format "{\\i %s}" contents))

(defun org-rtf-underline (el contents info)
  (format "{\\ul %s}" contents))

(defun org-rtf-strike-through (el contents info)
  (format "{\\strike %s}" contents))

(defun org-rtf-code (el contents info)
  (format "{\\f2 %s}" contents))

(defun org-rtf-verbatim (el contents info)
  (format "{\\f2 %s}" contents))

;; No new line before `\\par' because string already ends in a new line?
(defun org-rtf-paragraph (paragraph contents info)
  (format "{\\pard\\sb180\\sa180\\f1\n%s\\par}" (or contents "")))

(defun org-rtf-make-preamble (info)
  "{\\rtf1"
  info)

(defun org-rtf-template (contents info)
  "Return complete document string after RTF conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   "{\\rtf1\\ansi\\deff0\n"
   ;;  (org-rtf-make-preamble info)
   ;; Document's body
   contents
   "} End RTF"))
  

;;; Kitchin's work
(org-export-define-derived-backend 'RTF 'ascii
  :translate-alist '((bold . org-rtf-bold)
		     (italic . org-rtf-italic)
		     (underline . org-rtf-underline)
		     ;; (superscript . rtf-super)
		     ;; (subscript . rtf-sub)
		     (verbatim . org-rtf-verbatim)
		     (code . org-rtf-code)
		     (strike-through . org-rtf-strike-through)
		     (paragraph . org-rtf-paragraph)
		     ;; (headline . rtf-headline)
		     ;; (src-block . rtf-src)
		     ;; (table . rtf-table) 
		     ;; (fixed-width . rtf-fixed-width)
		     ;; (link . rtf-link)
		     ;; (latex-fragment . rtf-latex-fragment)
		     ;; (footnote-reference . rtf-footnote-reference)
		     ;;(footnote-definition . rtf-footnote-definition))
		     (template . org-rtf-template)
		     ))

;;; End-user functions

;;; Taken straight from `https://raw.githubusercontent.com/emacsmirror/org/master/lisp/ox-ascii.el'
;;; with query replace of ascii -> rtf

;;;###autoload
(defun org-rtf-export-as-rtf
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title and
table of contents from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org RTF Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'RTF "*Org RTF Export*"
    async subtreep visible-only body-only ext-plist (lambda () (text-mode))))

(provide 'ox-rtf)

;;; ox-rtf.el ends here
