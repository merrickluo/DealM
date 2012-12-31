;; -*- Emacs-Lisp -*-
;; Last modified: <2012-11-29 14:17:53 Thursday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

;; For Jade mode
;; ------------------------------------------[For Jade mode]
(add-to-list 'load-path (concat plugins-path-r "jade-mode/"))

(autoload 'sws-mode "sws-mode" "" t)
(autoload 'jade-mode "jade-mode" "" t)

(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))


;; For html
;; -----------------------------------------------[For html]
(defun html-end-of-line ()
  "If there is an HTML tag at the end of the line, then go to start of tag.
 Otherwise go to the real end of the line."
  (interactive)
  (if (or (looking-at ".*>$") ; if we're on a line that ends with a tag
          (and (= (char-before) 62)
               (= (point) (save-excursion
                            (end-of-line)
                            (point)))))
                                        ; or we're at the end of a line
                                        ; with a tag
      (let ((where-now (point)))
        (narrow-to-region
         (save-excursion
           (beginning-of-line)
           (point))
         (save-excursion
           (end-of-line)
           (point)))
        (end-of-line)
        (re-search-backward "<" nil t)
        (if (= (point) where-now)
            (end-of-line))
        (widen))
    (end-of-line)))


;; fold do what I mean
(autoload 'fold-dwim-toggle "fold-dwim" "" t)


;; Hooks
;; --------------------------------------------------[Hooks]
(add-hook 'sgml-mode-hook
          (lambda ()
            (folding-mode)
            (define-key sgml-mode-map (kbd "C-c c")  'fold-dwim-toggle)
            (define-key sgml-mode-map "\C-e"         'html-end-of-line)
            (define-key sgml-mode-map (kbd "C-c c")  'folding-toggle-show-hide)
            (define-key folding-mode-map  (kbd "C-c c") 'folding-toggle-show-hide)))


(provide 'webdev-settings)
;; webdev-settings ends here.
;;;
