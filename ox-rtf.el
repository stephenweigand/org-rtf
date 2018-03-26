;;; ox-rtf.el --- Probably not an RTF Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Author: Stephen Weigand <weigand dot stephen at gmail dot com>

;;
;; Initially borrowing from John Kitchin's Scimax `ox-rtf.el' at
;;
;;    https://github.com/jkitchin/scimax/blob/master/ox-rtf.el
;;
;; Currently trying to emulate `ox-ascii.el' and `ox-html.el' and
;; friends in terms of doc strings function arguments, etc.

;;; Commentary:
;;
;; This library implements an RTF back-end for Org generic exporter.
;; See Org manual for more information.

;;; Code:

;;; Dependencies (SDW is not sure which are needed)
(require 'ox)
(require 'cl-lib)
(require 'format-spec)

;;; Define Back-end
(org-export-define-backend 'RTF
  '((bold . org-rtf-bold)
    ;; (center-block . org-rtf-center-block)
    ;; (clock . org-rtf-clock)
    (code . org-rtf-code)
    ;; (drawer . org-rtf-drawer)
    ;; (dynamic-block . org-rtf-dynamic-block)
    ;; (entity . org-rtf-entity)
    ;; (example-block . org-rtf-example-block)
    ;; (export-block . org-rtf-export-block)
    ;; (export-snippet . org-rtf-export-snippet)
    (fixed-width . org-rtf-fixed-width)
    ;; (footnote-reference . org-rtf-footnote-reference)
    (headline . org-rtf-headline)
    ;; (horizontal-rule . org-rtf-horizontal-rule)
    ;; (inline-src-block . org-rtf-inline-src-block)
    ;; (inlinetask . org-rtf-inlinetask)
    ;; (inner-template . org-rtf-inner-template)
    (italic . org-rtf-italic)
    ;; (item . org-rtf-item)
    ;; (keyword . org-rtf-keyword)
    ;; (latex-environment . org-rtf-latex-environment)
    ;; (latex-fragment . org-rtf-latex-fragment)
    (line-break . org-rtf-line-break)
    ;; (link . org-rtf-link)
    ;; (node-property . org-rtf-node-property)
    (paragraph . org-rtf-paragraph)
    ;; (plain-list . org-rtf-plain-list)
    ;; (plain-text . org-rtf-plain-text)
    ;; (planning . org-rtf-planning)
    ;; (property-drawer . org-rtf-property-drawer)
    ;; (quote-block . org-rtf-quote-block)
    ;; (radio-target . org-rtf-radio-target)
    (section . org-rtf-section)
    ;; (special-block . org-rtf-special-block)
    ;; (src-block . org-rtf-src-block)
    ;; (statistics-cookie . org-rtf-statistics-cookie)
    (strike-through . org-rtf-strike-through)
    (subscript . org-rtf-subscript)
    (superscript . org-rtf-superscript)
    ;; (table . org-rtf-table)
    ;; (table-cell . org-rtf-table-cell)
    ;; (table-row . org-rtf-table-row)
    ;; (target . org-rtf-target)
    (template . org-rtf-template)
    ;; (timestamp . org-rtf-timestamp)
    (underline . org-rtf-underline)
    (verbatim . org-rtf-verbatim))
    ;; (verse-block . org-rtf-verse-block)
  :menu-entry
  '(?r "Export to RTF" org-rtf-export-as-rtf)
  :options-alist ;; Not sure what this does
  '((:subtitle "SUBTITLE" nil nil parse)))



;;; User Configurable Variables (None now)

;;; Transcode Functions (alphabetical)

;;;; Bold

(defun org-rtf-bold (bold contents info)
  "Transcode BOLD from Org to RTF.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "{\\b %s}" contents))

;;;; Code

(defun org-rtf-code (code _contents info)
  "Return a CODE object from Org to RTF.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "{\\f2 %s}" (org-element-property :value code)))

;;;; Headline

(defun org-rtf-headline (headline contents info)
  "Transcode a HEADLINE element from Org to RTF.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (format
   ; Why two `%s'? Second is `contents' (rest of document?)
   ;; "{\\b %s}\\par %s"
   "{\\pard\\b\\fs28\\myheadline %s\\par}\n\n%s" 
   (or (org-element-property :raw-value headline) "") contents))

;;;; Italic

(defun org-rtf-italic (_italic contents _info)
  "Transcode italic from Org to RTF.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "{\\i %s}" contents))

;;;; Line Break

(defun org-rtf-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to RTF.
CONTENTS is nil.  INFO is a plist holding contextual
  information."
  (concat "\\line" hard-newline))

;;;; Plain Text

(defun org-rtf-plain-text (text info)
  "Transcode a TEXT string from Org to RTF.
INFO is a plist used as a communication channel."
  ;; (let ((utf8p (eq (plist-get info :ascii-charset) 'utf-8)))
  ;;   (when (and utf8p (plist-get info :with-smart-quotes))
  ;;     (setq text (org-export-activate-smart-quotes text :utf-8 info)))
  ;;   (if (not (plist-get info :with-special-strings)) text
  ;;     (setq text (replace-regexp-in-string "\\\\-" "" text))
  ;;     (if (not utf8p) text
	;; Usual replacements in utf-8 with proper option set.
	(replace-regexp-in-string
	 "\\.\\.\\." "..."; "…"
	 (replace-regexp-in-string
	  "--" "{\\endash}"
	  (replace-regexp-in-string "---" "{\\emdash}" text))))

;;;; Section

(defun org-rtf-section (section contents info)
  "Transcode a SECTION element from Org to RTF.
CONTENTS is the contents of the section.  INFO is a plist holding
contextual information."
  contents)

;;;; Strike-through

(defun org-rtf-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH from Org to RTF.
CONTENTS is text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "{\\strike %s}" contents))

;;;; Subscript

(defun org-rtf-subscript (subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to RTF.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "{\\sub %s}" contents))


;;;; Superscript

(defun org-rtf-superscript (superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to RTF.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "{\\super %s}" contents))


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



(defun org-rtf-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to RTF.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (format "{\\pard\\sb180\\sa180\\f1\n%s\\par}" contents));; (or contents "")))



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
  

;;; End-user functions

;;; Taken straight from `ox-ascii.el' with query replace of ascii -> rtf

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
