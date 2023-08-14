;;; sagemintex.el --- Execute minted sage blocks in tex buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
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

;; This package lets you write and execute minted sage blocks in LaTeX
;; buffers.  It's a bit like what happens with org mode source blocks.
;; Inspired by the `xenops' package.

;;; Code:

(require 'mmm-auto)
(setq mmm-global-mode 'maybe)

;; Define latex-minted-sage class
(mmm-add-classes
 '((latex-minted-sage
    :submode sage-shell:sage-mode
    :face mmm-default-submode-face
    :front "\\\\begin{minted}{sage}"
    :back "\\\\end{minted}"
    :save-matches 1
    :insert ((?s sagecode nil @ "\\begin{minted}{sage}" @ "\n" _ "\n" @ "\\end{minted}" @))
    :submode-hook (lambda () (sagemintex-mode 1)))))

;; Add the class to LaTeX-mode
(mmm-add-mode-ext-class 'latex-mode "\\.tex\\'" 'latex-minted-sage)

;; Optional: Customize mmm-default-submode-face
(custom-set-faces
 '(mmm-default-submode-face ((t (:background "#ddffff")))))

(defgroup sagemintex nil
  "Executable minted sage blocks in tex buffers."
  :group 'tex
  :prefix "sagemintex-")

;; customization variable that determines whether to automatically use
;; tex-fold on new result regions:
(defcustom sagemintex-auto-fold-results t
  "If non-nil, apply `TeX-fold-region' to new result blocks."
  :type 'boolean
  :group 'sagemintex)

(cl-defun sagemintex-evaluate (&key latex)
  "Evaluate the current minted sage code block.
This function takes an optional argument 'latex' which, if non-nil,
causes the result to be split into lines and inserted into equation*
blocks."
  (interactive)
  (when-let ((ovl (mmm-overlay-at (point))))
    (let ((code
	   (buffer-substring-no-properties (overlay-start ovl) (overlay-end
								ovl))))
      (if latex
	  (sagemintex-evaluate-latex)
	(sagemintex-evaluate-no-latex)))))

(defmacro sagemintex-with-auto-fold (&rest body)
  "If `sagemintex-auto-fold-results' is non-nil, fold the region
created by the BODY expressions."
  `(let ((pos (point)))
     ,@body
     (when sagemintex-auto-fold-results
       (TeX-fold-region pos (point)))))

(cl-defun sagemintex-evaluate (&key latex)
  "Evaluate the current minted sage code block.
This function takes an optional argument 'latex' which, if non-nil,
causes the result to be split into lines and inserted into equation*
blocks."
  (interactive)
  (when-let ((ovl (mmm-overlay-at (point))))
    (buffer-substring-no-properties (overlay-start ovl) (overlay-end ovl))
    (let* ((code
	    (buffer-substring-no-properties (overlay-start ovl) (overlay-end
								 ovl)))
	   (wrapped-code
	    (if (not latex) code
	      (let*
		  (
		   (code-lines (split-string code "\n"))
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
		result
		))
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
  (interactive)
  (sagemintex-evaluate :latex t)
  )

(defvar sagemintex-mode-map (make-sparse-keymap)
  "Keymap for `sagemintex-mode'.")

(define-minor-mode sagemintex-mode
  "Minor mode for minted blocks with sage submode."
  :init-value nil
  :lighter " MS"
  :keymap sagemintex-mode-map)


(defun sagemintex-enable ()
  (setq-local indent-line-function #'my-indent-line-narrowed)
  (sagemintex-mode 1))

(defun sagemintex-disable ()
  (sagemintex-mode 0))

(defun my-indent-line-narrowed ()
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


(provide 'sagemintex)
;;; sagemintex.el ends here

