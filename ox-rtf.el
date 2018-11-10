;; ox-rtf.el --- Probably not an RTF Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2018 

;; Author: Stephen Weigand <weigand dot stephen at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp

;; NEXT: handle title which needs defaults
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
(require 'ox-ascii)

;;; Define Back-end
(org-export-define-backend 'rtf
  '((bold . org-rtf-bold)
    (center-block . org-rtf-center-block)
    ;; (clock . org-rtf-clock)
    (code . org-rtf-code)
    ;; (drawer . org-rtf-drawer)
    ;; (dynamic-block . org-rtf-dynamic-block)
    ;; (entity . org-rtf-entity)
    (example-block . org-rtf-example-block)
    (export-block . org-rtf-export-block)
    (export-snippet . org-rtf-export-snippet)
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
  '((:subtitle "SUBTITLE" nil nil parse)
    (:rtf-bullets nil nil org-rtf-bullets)))



;;; User Configurable Variables

(defgroup org-export-rtf nil
  "Options for exporting Org mode files to RTF."
  :tag "Org Export RTF"
  :group 'org-export)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Customization-Types.html
;; FIXME: There may be a way to name these elements but
;;        they are standard bullet, white bullet, and triangular bullet
(defcustom org-rtf-bullets '("{\\u8226*}" "{\\u9702o}" "{\\u8227^}")
  "Bullet characters for headlines converted to lists in RTF export.

List of three strings representing the bullet symbols.

The first string is used for the first level considered as low
level, and so on.  If there are more levels than characters given
here, the list will be repeated.

Note that this variable doesn't affect plain lists
representation."
  :group 'org-export-rtf
  :type '(list string string string))

;;;; TODO This can be improved with a list of lists with number, family, and name
(defcustom org-rtf-font-table-fonts
  '("\\f0\\froman Times New Roman;"
    "\\f1\\fswiss Arial;"
    "\\f2\\fmodern Courier New;")
  "Fonts available in the RTF document. The font name is a string
and the font family is among the following table taken from

  URL `http://www.biblioscape.com/rtf15_spec.htm#Heading12'

nil      Unknown or default fonts (the default) 	
roman    Roman, proportionally spaced serif fonts (e.g., Times New Roman, Palatino)
swiss    Swiss, proportionally spaced sans serif fonts (e.g., Arial)
modern   Fixed-pitch serif and sans serif fonts (e.g., Courier New, Pica)
script   Script fonts (e.g., Cursive)
decor    Decorative fonts (e.g., Old English, ITC Zapf Chancery)
tech     Technical, symbol, and mathematical fonts (e.g., Symbol)
bidi     Arabic, Hebrew, or other bidirectional font (e.g., Miriam)"
  :group 'org-export-rtf
  :type '(repeat
	  (cons (string :tag "Font name")
		(choice (const :tag "nil" nil)
			(const :tag ""

(defcustom org-rtf-colors
  '((0 0 0)
    (255 0 0)
    (0 255 0)
    (0 0 255))
  "Colors available in the RTF document in the form of a list of
three-integer lists. In RTF colors are declared in a color table
group and specified in terms of their red, green, and blue
components via the three commands

  \redN\greenN\blueN;

where N is an integer ranging from 0 to 255. Colors are
implicitly assigned sequential numbers starting with zero (which
is usually black) and color markup in the RTF document can be
obtained by referring to the color number.  The RTF v1.5 spec (URL
`http://www.biblioscape.com/rtf15_spec.htm#Heading16') gives this
example of RTF:

  {\\f1\\cb1\\cf2 This is colored text. The background is color1
  and the foreground is color 2.}

The default forground and background are specified with \\cf0 and
\\cf0 and can be omitted."
  :group 'org-exort-rtf
  :type '(repeat
	  (list (integer :tag "  Red")
		(integer :tag "Green")
		(integer :tag " Blue"))))


(defcustom org-rtf-character-set "ansi"
  "The character set for the RTF document."
  :group 'org-export-rtf
  :type '

(defcustom org-rtf-default-font 0
  "The default font number for the RTF document as specified
by \\deff."
  :group 'org-export-rtf
  :type 'integer)


(defcustom org-rtf-quasi-styles
  '((paragraph . ((outline-level . nil)
		  (first-line-indent . 0)
		  (font-number . 0)
		  (font-size . 12)
		  (text-color . 0)
		  (space-before . 0)
		  (space-after . 0)
		  (space-between-lines . 1000)
		  (align . "left")
		  (hypenate . nil)
		  (keep-intact . nil)
		  (extra-commands . ""))))
  "Nested alist of formatting characteristics of standard paragraph,
level 1 heading, level 2 heading, etc. In RTF most text is part
of a paragraph and there are no semantic sectioning commands such
as \\section in LaTeX or <h1> in HTML. Instead, a paragraph in RTF can
be made to look like a level 1 heading by, for example, making it bold,
enlarging the font, or changing the color, etc. This alist allows one
to customize the formatting for a default paragraph and for a heading of
any level.

Within a level, the elements are as follows:

  outline-level         \\outlinelevelN (N=0-8)  A metadata rather than semantic tag
  first-line-indent     \\fi
  font-number           \\fn
  font-size             \\fs
  text-color            \\cf
  space-before          \\sb
  space-after           \\sa
  space-between-lines   \\sl Space between lines in points
  align                 \\q? Paragraph aligment among left, right, or center
  hyphenate             \\hypthpar Whether to hyphenate
  keep-intact
  extra-commands


      
  
  
  
  ("extra" . "")
;;; Template



;;; Transcode Functions (alphabetical)

;;;; Bold

(defun org-rtf-bold (bold contents info)
  "Transcode BOLD from Org to RTF.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "{\\b %s}" contents))

;;;; Center Block
;; This is from https://www.emacswiki.org/emacs/rtf-mode.el
;; and used to find patterns for code highlighting.
;; Useage: After this sexp do C-u C-x C-c
;; (rtf-make-loudcmd-re '(pard))
(defun rtf-make-loudcmd-re (cmdlist)
  (concat "\\(\\\\\\)\\("
	  (regexp-opt (mapcar 'symbol-name cmdlist))
	  "\\)\\b\\( ?\\)"))


(defun org-rtf-center-block (_center-block contents _info)
  "Transcode a CENTER-BLOCK element from Org to RTF.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  ;; Replace standard paragraph left justification (\ql) with
  ;; center justification (\qc).
  ;; `replace-regexp-in-string' has args
  ;; REGEXP
  ;; REP (replacement)
  ;; STRING
  ;; optional FIXEDCASE
  ;; optional LITERAL (which we want it inserts REP literally
  (replace-regexp-in-string "\\(\\\\\\)\\(\\(?:ql\\)\\)\\b\\( ?\\)"
			    "\\qc" contents nil t))


;;;; Code

(defun org-rtf-code (code _contents info)
  "Return a CODE object from Org to RTF.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "{\\f2 %s}" (org-element-property :value code)))

;;;; Example block

;; Unlike `center-block' this is not already a paragraph
;; so I need to wrap as a paragraph. I don't want a
;; `\\line' on the last line so I trim the string. But that
;; kills the last newline and so to male `\par}' on a line
;; by itself I add a new line after `%s' (which `org-rtf-paragraph'
;; does not need.
(defun org-rtf-example-block (example-block _contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to RTF.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format
   "{\\pard\\ql\\sb180\\sa180\\f2\n%s\n\\par}"
   ;; Remove `\\line' at very end
   (substring
    (replace-regexp-in-string
     "\n" "\\line\n"
     (org-export-format-code-default example-block info) nil t)
    0 -6)))
    
;;;; Export Block
(defun org-rtf-export-block (export-block _contents info)
  "Transcode a EXPORT-BLOCK element from Org to RTF.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "RTF")
    ;; Via `org-ascii-export-block' realized this is what holds the lines
    (org-element-property :value export-block)))

;;;; Export Snippet

(defun org-rtf-export-snippet (export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object from Org to RTF.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'rtf)
    (org-element-property :value export-snippet)))


;;;; Headline

(defun org-rtf-headline (headline contents info)
  "Transcode a HEADLINE element from Org to RTF.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  ;; (format
  ;;  ; Why two `%s'? Second is `contents' (rest of document?)
  ;;  ;; "{\\b %s}\\par %s"
  ;;  "{\\pard\\b\\fs28\\myheadline %s\\par}\n\n%s" 
  ;;  ;;(org-export-get-headline-number headline info) ;; A list like '(1 1)'
  ;;  ;; (or (org-element-property :raw-value headline) "") contents))
  ;;  (org-ascii--build-title headline info 60 t)
  ;;  contents))

  ;; SDW: Essentially a copy of  `org-ascii-headline'
  ;; Don't export footnote section, which will be handled at the end
  ;; of the template.
  (unless (org-element-property :footnote-section-p headline)
    (let* ((low-level (org-export-low-level-p headline info))
	   (width 80) ;(org-ascii--current-text-width headline info))
	   ;; Export title early so that any link in it can be
	   ;; exported and seen in `org-ascii--unique-links'.
	   (title (org-ascii--build-title headline info width (not low-level)))
	   ;; Blank lines between headline and its contents.
	   ;; `org-ascii-headline-spacing', when set, overwrites
	   ;; original buffer's spacing.
	   (pre-blanks
	    (make-string (or (car (plist-get info :ascii-headline-spacing))
			     (org-element-property :pre-blank headline)
			     0)
			 ?\n))
	   (links (and (plist-get info :ascii-links-to-notes)
		       (org-ascii--describe-links
			(org-ascii--unique-links headline info) width info)))
	   ;; Re-build contents, inserting section links at the right
	   ;; place.  The cost is low since build results are cached.
	   (body
	    (if (not (org-string-nw-p links)) contents
	      (let* ((contents (org-element-contents headline))
		     (section (let ((first (car contents)))
				(and (eq (org-element-type first) 'section)
				     first))))
		(concat (and section
			     (concat (org-element-normalize-string
				      (org-export-data section info))
				     "\n\n"))
			links
			(mapconcat (lambda (e) (org-export-data e info))
				   (if section (cdr contents) contents)
				   ""))))))
      ;; Deep subtree: export it as a list item.
      (if low-level 
	  (let* ((bullets org-rtf-bullets)
		 (bullet
		  (format "%s" (nth (mod (1- low-level) (length bullets)) bullets))))
	    (concat bullet "{\\tab}" title "\n"))
		 
	;; Else: Standard headline.
	(concat
	 "{\\pard\\standardheadline\n"
	 title "\n\\par}\n" pre-blanks body)))))


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

;;;; Paragraph

(defun org-rtf-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to RTF.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (format "{\\pard\\ql\\sb180\\sa180\\f0\n%s\\par}"
	  (org-rtf--fill-string contents fill-column info 'left)))

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



;; TODO
;; Where does this section go within this file?
;; How to I use `template' intelligently
(defun org-rtf-make-preamble (info); &optional template)
  "Return a formatted RTF preamble.
INFO is a plist used as a communication channel.  Optional
argument TEMPLATE, when non-nil, is the header template string,
as expected by `org-splice-latex-header'."
  (concat "{\\rtf1\\" "test"))
;;  info)

(defun org-rtf-template (contents info)
  "Return complete document string after RTF conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; "{\\rtf1\\ansi\\deff0\n"
   (org-rtf-make-preamble info)
   ;; Document's body
   ;; (org-export-data (plist-get info :title) info) ;; how `ox-latex.el' does it by why not just `(plist-get info :title)'
   contents
   "\n} RTF file ends here"))


;;; Internal Functions

;;; This is from `ox-ascii.el' and I'm using it to make a
;;; fill command to wrap paragraphs with a space on lines >= 2
;;; Like this:
;;; {\pard 
;;; This is my
;;;  paragraph and I have
;;;  wrapped the lines so there
;;;  is space between words (and
;;;  it's at the start of the line).
;;; \par}
(defun org-rtf--fill-string (s text-width info &optional justify)
  "Fill a string with specified text-width and return it.

S is the string being filled.  TEXT-WIDTH is an integer
specifying maximum length of a line.  INFO is the plist used as
a communication channel.

Optional argument JUSTIFY can specify any type of justification
among `left', `center', `right' or `full'.  A nil value is
equivalent to `left'.  For a justification that doesn't also fill
string, see `org-ascii--justify-lines' and
`org-ascii--justify-block'.

Return nil if S isn't a string."
  (when (stringp s)                ;; when S is a string
    (let ((double-space-p sentence-end-double-space))
      (with-temp-buffer  ;; in a temp buffer `let' variables then `insert' into buffer a filled region
	;; These set arguments for `fill-region' to use
	(let ((fill-column text-width) ;; `fill-column' from `text-width'
	      (use-hard-newlines t)    ;; Set to true
	      (sentence-end-double-space double-space-p))
	  (insert (if (plist-get info :preserve-breaks)
		      (replacev-regexp-in-string "\n" hard-newline s)
		    s))
	  (fill-region (point-min) (point-max) justify)) ; End second `let'
	;; TODO: Is this OK? We are in a temporary buffer so I can navigate it
	;; Trying to start each line after first with a space
	(goto-char (point-min))
	(let ((line-num 1)
	      (total-lines (count-lines (point-min) (point-max))))
	   (while (< line-num total-lines)
	     (forward-line)
	     (insert " ")
	     (setq line-num (+ line-num 1))))
	
	(buffer-string))); end first let ;; return contents of current buffer as a string
    ) ; end `when'
  )

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
  (org-export-to-buffer 'rtf "*Org RTF Export*"
    async subtreep visible-only body-only ext-plist (lambda () (text-mode))))

(provide 'ox-rtf)

;; Local variables:
;; coding: utf-8
;; End:

;;; ox-rtf.el ends here
    
