;;; czm-tex-mint.el --- Evaluate minted sage blocks in TeX buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-tex-mint.el
;; Package-Requires: ((emacs "26.1") (mmm-mode "0.5.9") (sage-shell-mode "0.3") (auctex "11.86.1") (ob-sagemath "0.4"))
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

;; This package provides commands for evaluating minted sage blocks in
;; a LaTeX buffer using ob-sagemath, and displaying the results in
;; either verbatim or align* blocks.  See the README for details.

;;; Code:

(require 'mmm-auto)
(require 'mmm-region)
(require 'sage-shell-mode)
(require 'preview)
(require 'tex-fold)
(require 'ob-sagemath)

(defvar czm-tex-mint--mode-map (make-sparse-keymap)
  "Keymap for `czm-tex-mint--mode'.")

;;;###autoload
(define-minor-mode czm-tex-mint--mode
  "Minor mode for minted blocks with sage submode."
  :init-value nil
  :lighter " MS"
  :keymap czm-tex-mint--mode-map)

(defun czm-tex-mint--initialize ()
  "Initialize `czm-tex-mint--mode'.
Define the `latex-minted-sage' class and add it to `latex-mode'."
  (mmm-add-classes
   '((latex-minted-sage
      :submode sage-shell:sage-mode
      :face mmm-default-submode-face
      :front "\\\\begin{minted}{sage}"
      :back "\\\\end{minted}"
      :save-matches 1
      :insert ((?s sagecode nil @ "\\begin{minted}{sage}" @ "\n" _ "\n" @ "\\end{minted}" @))
      :submode-hook (lambda () (czm-tex-mint--mode 1)))))
  ;; (mmm-add-mode-ext-class 'latex-mode "\\.tex\\'" 'latex-minted-sage)
  (mmm-add-mode-ext-class 'latex-mode nil 'latex-minted-sage))

(defun czm-tex-mint--enable ()
  "Enable `czm-tex-mint--mode' in the current buffer."
  (setq-local indent-line-function #'czm-tex-mint--indent-line-narrowed)
  (czm-tex-mint--mode 1))

(defun czm-tex-mint--disable ()
  "Disable `czm-tex-mint--mode' in the current buffer."
  (czm-tex-mint--mode 0))

(defun czm-tex-mint--indent-line-narrowed ()
  "An indent function which works on some modes where `mmm-indent-line' doesn't.
Works like `mmm-indent-line', but narrows the buffer before indenting to
appease modes which rely on constructs like (point-min) to indent."
  (interactive)
  (let ((indent-function
         (save-excursion
           (back-to-indentation)
           (mmm-update-submode-region)
           (get
            (if (and mmm-current-overlay
                     (> (overlay-end mmm-current-overlay)
                        (point)))
                mmm-current-submode
              mmm-primary-mode)
            'mmm-indent-line-function))))
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
      (funcall indent-function))))

(defgroup czm-tex-mint nil
  "Executable minted sage blocks in tex buffers."
  :group 'tex
  :prefix "czm-tex-mint-")

(defcustom czm-tex-mint-auto-fold-results t
  "If non-nil, apply `TeX-fold-region' to new result blocks."
  :type 'boolean
  :group 'czm-tex-mint)

(defmacro czm-tex-mint-with-auto-fold (&rest body)
  "Conditionally fold the region created by BODY.
If `czm-tex-mint-auto-fold-results' is non-nil, then fold the
region between the initial position of point and the final
position after evaluating BODY."
  `(let ((pos (point)))
     ,@body
     (when czm-tex-mint-auto-fold-results
       (TeX-fold-region pos (point)))))

(defvar czm-tex-mint--debug nil
  "If non-nil, print debug information to *DebugMintedSage* buffer.")

;;;###autoload
(cl-defun czm-tex-mint-evaluate (&key latex)
  "Evaluate the current minted sage code block.
If optional argument LATEX is non-nil, then split the result into
lines and enclose with align* blocks.  Otherwise, enclose the
result in a verbatim block."
  (interactive)
  (when-let ((ovl (mmm-overlay-at (point))))
    (let* ((code (buffer-substring-no-properties
                  (overlay-start ovl)
                  (overlay-end ovl)))
	          (wrapped-code
	           (if (not latex)
                code
	             (let* ((code-lines (split-string code "\n"))
		                   (wrapped-code-lines
                                        ; wrap result lines in "latex()".
                                        ; slightly hacky.
		                    (mapcar (lambda (line)
			                             (if (and
				                                 (> (length line)
                                        0)
				                                 (not (string-match-p "^load*" line))
				                                 (not (string-match-p "^[^=]*=\\([^=]\\|$\\)" line)))
				                                (concat "latex(" line ")")
				                              line))
			                           code-lines)))
		              (string-join
                 wrapped-code-lines
                 "\n"))))
	          (result
            (with-temp-buffer
              (insert (format "#+begin_src sage :results silent\n%s\n#+end_src" wrapped-code))
              (goto-char (point-min))
              (let ((python-indent-guess-indent-offset-verbose nil)
                    (inhibit-message t))
	               (org-babel-execute-src-block)))))
      (when czm-tex-mint--debug
        (with-current-buffer (get-buffer-create "*DebugMintedSage*")
	         (goto-char (point-max))
	         (insert (format-time-string "\n\n%Y-%m-%d %T.%3N\n"))
	         (insert "code:\n"
		                wrapped-code
		                "\n"
		                "result:\n"
		                result)))
      (save-excursion
	       (goto-char (overlay-end ovl))
	       (end-of-line)
	       ;; if the next line is a \begin{results} or \begin{results*} block,
	       ;; delete up to the matching \end{results}.
	       (when (looking-at "\n\\\\begin{results\\*?}")
	         (let ((end (save-excursion
		                     (re-search-forward "\n\\\\end{results\\*?}")
		                     (point))))
	           (delete-region (point)
                           end)))
	       (newline 1)
	       (czm-tex-mint-with-auto-fold (insert "\\begin{results*}\n"))
	       (let (beg end)
	         (if latex
	             (progn
		              (let* ((result-lines (split-string result "\n"))
                       (nonempty-result-lines
                        (seq-filter (lambda (line)
                                      (> (length line)
                                         0))
                                    result-lines)))
		                (setq beg (point))
		                (insert "\\begin{align*}\n")
		                (insert (mapconcat #'identity
                                     nonempty-result-lines
                                     "\\\\ \n"))
		                (insert "\n\\end{align*}\n")
		                (setq end (point))))
	           (czm-tex-mint-with-auto-fold (insert "\\begin{verbatim}\n"))
	           (insert result)
            (newline 1)
	           (czm-tex-mint-with-auto-fold (insert "\\end{verbatim}\n")))
	         (czm-tex-mint-with-auto-fold (insert "\\end{results*}"))
	         (if beg
	             (preview-region beg end)))))))

;;;###autoload
(defun czm-tex-mint-evaluate-latex ()
  "Evaluate current block.  Wrap results in align* blocks."
  (interactive)
  (czm-tex-mint-evaluate :latex t))

;;;###autoload
(defun czm-tex-mint-new-block ()
  "Create a new minted sage block."
  (interactive)
  (insert "\\begin{minted}{sage}\n")
  (save-excursion
    (insert "\n\\end{minted}\n"))
  (let* ((beg (save-mark-and-excursion
                (LaTeX-find-matching-begin)
                (point)))
         (end (save-mark-and-excursion
                (LaTeX-find-matching-end)
                (line-beginning-position 2))))
    (mmm-parse-region beg end)))


(provide 'czm-tex-mint)
;;; czm-tex-mint.el ends here
