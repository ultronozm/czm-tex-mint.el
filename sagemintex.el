;;; sagemintex.el --- Executable minted sage blocks in tex buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/sagemintex.el
;; Package-Requires: ((emacs "26.1") (mmm-mode "0.5.9") (sage-shell-mode "0.3"))
;; Keywords: tex, tools, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package makes it easy to work with minted sage blocks in LaTeX
;; documents.  You can write them and execute them.  You'll have a
;; good time.  Trust me.

;;; Code:

(require 'mmm-auto)
(require 'mmm-region)
(require 'sage-shell-mode)
(require 'preview)
(require 'tex-fold)


(defun sagemintex-initialize ()
  "Initialize `sagemintex-mode'.
Define the `latex-minted-sage' class and add it to `latex-mode'."
  (mmm-add-classes
   '((latex-minted-sage
      :submode sage-shell:sage-mode
      :face mmm-default-submode-face
      :front "\\\\begin{minted}{sage}"
      :back "\\\\end{minted}"
      :save-matches 1
      :insert ((?s sagecode nil @ "\\begin{minted}{sage}" @ "\n" _ "\n" @ "\\end{minted}" @))
      :submode-hook (lambda () (sagemintex-mode 1)))))
  (mmm-add-mode-ext-class 'latex-mode "\\.tex\\'" 'latex-minted-sage))

(defvar sagemintex-mode-map (make-sparse-keymap)
  "Keymap for `sagemintex-mode'.")

(define-minor-mode sagemintex-mode
  "Minor mode for minted blocks with sage submode."
  :init-value nil
  :lighter " MS"
  :keymap sagemintex-mode-map)

(defun sagemintex--indent-line-narrowed ()
  "An indent function which works on some modes where `mmm-indent-line' doesn't.
Works like `mmm-indent-line', but narrows the buffer before indenting to
appease modes which rely on constructs like (point-min) to indent."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (mmm-update-submode-region)
    (let ((indent-function (get
                            (if (and mmm-current-overlay
                                     (> (overlay-end mmm-current-overlay) (point)))
                                mmm-current-submode
                              mmm-primary-mode)
                            'mmm-indent-line-function)))
      (if mmm-current-overlay
          (save-restriction
            (narrow-to-region (overlay-start mmm-current-overlay)
                              (overlay-end mmm-current-overlay))
	    (save-excursion
	      ;; no idea why this works, but it does
	      ;; (goto-char (point-min))
	      ;; (newline 1)
	      ;; (backward-delete-char 1)
	      )
            (funcall indent-function))
        (funcall indent-function)))))

(defun sagemintex-enable ()
  "Enable `sagemintex-mode' in the current buffer."
  (setq-local indent-line-function #'sagemintex--indent-line-narrowed)
  (sagemintex-mode 1))

(defun sagemintex-disable ()
  "Disable `sagemintex-mode' in the current buffer."
  (sagemintex-mode 0))

(defgroup sagemintex nil
  "Executable minted sage blocks in tex buffers."
  :group 'tex
  :prefix "sagemintex-")

(defcustom sagemintex-auto-fold-results t
  "If non-nil, apply `TeX-fold-region' to new result blocks."
  :type 'boolean
  :group 'sagemintex)

(defmacro sagemintex-with-auto-fold (&rest body)
  "Conditionally fold the region created by BODY.
If `sagemintex-auto-fold-results' is non-nil, then fold the
region between the initial position of point and the final
position after evaluating BODY."
  `(let ((pos (point)))
     ,@body
     (when sagemintex-auto-fold-results
       (TeX-fold-region pos (point)))))

(cl-defun sagemintex-evaluate (&key latex)
  "Evaluate the current minted sage code block.
If optional argument LATEX is non-nil, then split the result into
lines and enclose with equation* blocks."
  (interactive)
  (when-let ((ovl (mmm-overlay-at (point))))
    (let* ((code
	    (buffer-substring-no-properties
             (overlay-start ovl) (overlay-end ovl)))
	   (wrapped-code
	    (if (not latex) code
	      (let* ((code-lines (split-string code "\n"))
		     (wrapped-code-lines
		      (mapcar (lambda (line)
			        (if (and
				     (> (length line) 0)
				     (not (string-match-p "^load*" line))
				     (not (string-match-p "^[^=]*=\\([^=]\\|$\\)" line)))
				    (concat "latex(" line ")")
				  line))
			      code-lines)))
		(string-join wrapped-code-lines "\n"))))
	   (sage-buffer
	    (or
	     (get-buffer "*Sage*")
	     (error "No *Sage* buffer found")))
	   ;; TODO: rewrite to use ob-sagemath instead
	   (result (sage-shell:send-command-to-string wrapped-code sage-buffer)))
      (with-current-buffer (get-buffer-create "*DebugMintedSage*")
	(goto-char (point-max))
	(insert (format-time-string "%Y-%m-%d %T.%3N\n"))
	(insert "code:\n"
		wrapped-code
		"\n"
		"result:\n"
		result))
      (save-excursion
	(goto-char (overlay-end ovl))
	(end-of-line)
	;; if the next line is a \begin{results} or \begin{results*} block,
	;; delete up to the matching \end{results}.
	(when (looking-at "\n\\\\begin{results\\*?}")
	  (let ((end (save-excursion
		       (re-search-forward "\n\\\\end{results\\*?}")
		       (point))))
	    (delete-region (point) end)))
	(newline 1)
	(sagemintex-with-auto-fold (insert "\\begin{results*}\n"))
	(let (beg end)
	  (if latex
	      (progn
		(let ((result-lines (split-string result "\n")))
		  (setq beg (point))
		  (insert "\\begin{align*}\n")
		  (dolist (line result-lines)
		    ;;  check if line is nonempty
		    (when (> (length line) 0)
		      (insert
		       line "\n"
		       ;; (format
		       ;; 	"\\begin{equation*}\n%s\n\\end{equation*}\n" line)
		       )))
		  (insert "\\end{align*}\n")
		  (setq end (point))))
	    (sagemintex-with-auto-fold (insert "\\begin{verbatim}\n"))
	    (insert result)
	    (sagemintex-with-auto-fold (insert "\\end{verbatim}\n")))
	  (sagemintex-with-auto-fold (insert "\\end{results*}"))
	  (if beg
	      (preview-region beg end)))))))

(defun sagemintex-evaluate-latex ()
  "Evaluate current block.  Wrap result in equation* blocks."
  (interactive)
  (sagemintex-evaluate :latex t))


(provide 'sagemintex)
;;; sagemintex.el ends here
