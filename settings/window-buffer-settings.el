;; -*- Emacs-Lisp -*-
;; Last modified: <2016-09-26 11:56:22 Monday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.2
;; PUBLIC LICENSE: GPLv3


;; Buffer settings
;; ------------------------------------------------------------------
(use-package recentf
  :commands (recentf-mode)
  :init     ; before code load
  (setq recentf-exclude '("\\.windows\\'"
                          "/ssh"
                          "/sudo:"
                          "/tmp/")
        recentf-save-file (concat emacs-root-path ".recentf")
        recentf-max-saved-items 500
        recentf-max-menu-items 60)
  (recentf-mode 1)
  (defun xsteve-ido-choose-from-recentf ()
    "Use ido to select a recently opened file from the `recentf-list'"
    (interactive)
    (let ((home (expand-file-name (getenv "HOME"))))
      (find-file
       (ido-completing-read "Recentf open: "
                            (mapcar (lambda (path)
                                      (replace-regexp-in-string home "~" path))
                                    recentf-list)
                            nil t))))
  :bind (("C-M-o" . smart-ido-recentf))
  :config
  (use-package projectile
    :config   ; execute code after a package is loaded
    (defun smart-ido-recentf ()
      (interactive)
      (recentf-mode 1)
      (if (projectile-project-p)
          (call-interactively 'projectile-recentf)
        (call-interactively 'xsteve-ido-choose-from-recentf)))
    (define-key isearch-mode-map (kbd "C-M-o") 'smart-ido-recentf)))

;; Immediately close the current buffer.
(use-package menu-bar
  :defer t
  :bind (("C-x k" . kill-this-buffer)))

(use-package buffer-move
  :defer t
  :bind (("C-x w u" . buf-move-up)
         ("C-x w d" . buf-move-down)
         ("C-x w l" . buf-move-left)
         ("C-x w r" . buf-move-right)))

(use-package window
  :config
  (defvar temp-window-configuration nil "store temp window configuration.")
  (defun smart-window-customize()
    "Delete all other-window when not occupy whole window.
otherwise restore."
    (interactive)
    (let ((wincount 0))
      (walk-windows (lambda(w)
                      (setq wincount (1+ wincount))))
      (cond ((> wincount 1)
             (setq temp-window-configuration (current-window-configuration))
             (delete-other-windows))
            ((= wincount 1)
             (if temp-window-configuration
                 (set-window-configuration temp-window-configuration)
               (split-window-vertically))))))
  :bind (("M-N" . next-buffer)
         ("M-P" . previous-buffer)
         ("<M-return>" . smart-window-customize)))


;; fullscreen settings
;; ------------------------------------------------------------------
(use-package frame
  :if window-system
  :defer t
  :bind (("<f10>" . toggle-frame-maximized)
         ("M-<f10>" . toggle-frame-maximized)
         ("<f11>" . toggle-frame-fullscreen)
         ("M-<f11>" . toggle-frame-fullscreen)))


(provide 'window-buffer-settings)
;; window-buffer-settings ends here.
;;;
