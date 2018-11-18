;; Code

;;; Code:

;; Dependencies
;; I am not sure which of these are needed.

;;; Dependencies
(require 'ox)
(require 'cl-lib)
(require 'format-spec)
(require 'ox-ascii)

;; Define Back-End
;; #+NAME: Define back end

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

;; Define the group

(defgroup org-export-rtf nil
  "Options for exporting Org mode files to RTF."
  :tag "Org Export RTF"
  :group 'org-export)

;; Customize bullets

;; See
;; [[https://www.gnu.org/software/emacs/manual/html_node/elisp/Customization-Types.html]]. There
;; may be a way to name these elements but they are standard bullet,
;; white bullet, and triangular bullet. They are specified in RTF's
;; decimal based Unicode.


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

;; TODO This can be improved with a list of lists with number, family, and name

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

;; TODO Interface with Emacs color system somehow

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

  {\\f1\\cb1\\cf2 This is colored text. The background is color
   1 and the foreground is color 2.}

The default forground and background are specified with \\cf0 and
\\cb0 and can be omitted."
  :group 'org-exort-rtf
  :type '(repeat
	  (list (integer :tag "  Red")
		(integer :tag "Green")
		(integer :tag " Blue"))))
