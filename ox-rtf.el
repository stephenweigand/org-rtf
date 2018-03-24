;;; ox-rtf.el --- Probably not an RTF Back-End for Org Export Engine

;; Author: Stephen Weigand <weigand dot stephen at gmail dot com>
;;
;; Borrowing heavily from John Kitchin's Scimax `ox-rtf.el' at
;;
;;    https://github.com/jkitchin/scimax/blob/master/ox-rtf.el
;;

;;; Commentary:

;; This library implements an RTF back-end for Org generic exporter.
;; See Org manual for more information.

;;; Code:

;;; Dependencies
(require 'ox)

;; Mostly taken from Kitchin's `ox-rtf.el' but prepending `org-' to functions

;;; Transcode Functions (alphabetical)

;;;; Bold

(defun org-rtf-bold (bold contents info)
  "Transcode BOLD from Org to RTF.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "{\\b %s}" contents))

;;;; Code (`org-ascii-code' allows customization)

(defun org-rtf-code (code _contents info)
  "Return a CODE object from Org to RTF.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "{\\f2 %s}" (org-element-property :value code)))


;;;; Italic

(defun org-rtf-italic (_italic contents _info)
  "Transcode italic from Org to RTF.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "{\\i %s}" contents))

;;;; Strike-through

(defun org-rtf-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH from Org to RTF.
CONTENTS is text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "{\\strike %s}" contents))


;;;; Underline

(defun org-rtf-underline (_underline contents _info)
  "Transcode UNDERLINE from Org to RTF.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "{\\ul %s}" contents))

;;;; Verbatim

(defun org-rtf-verbatim (verbatim _contents info)
  "Return a VERBATIM object from Org to RTF.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "{\\f2 %s}" ;(plist-get info :ascii-verbatim-format)
	  (org-element-property :value verbatim)))



;; No new line before `\\par' because string already ends in a new line?
(defun org-rtf-paragraph (paragraph contents info)
  (format "{\\pard\\sb180\\sa180\\f1\n%s\\par}" (or contents "")))


;; Pure Kitchin here and I have no idea what the property stuff is
(defun org-rtf-headline (headline contents info)
  "Transcode a HEADLINE element from Org to RTF.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (format
   ; Why two `%s'? Second is `contents' (rest of document?)
   ; "{\\b %s}\\par %s"
   "{\\pard\\b\\fs28\\myheadline %s\\par}\n\n%s"
   (or (org-element-property :raw-value headline) "") contents))

(defun org-rtf-make-preamble (info)
  "{\\rtf1\\"
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
		     (headline . org-rtf-headline)
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
