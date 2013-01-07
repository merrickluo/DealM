;; -*- Emacs-Lisp -*-
;; Last modified: <2013-01-07 18:36:29 Monday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.2
;; PUBLIC LICENSE: GPLv3


;; Buffer settings
;; ------------------------------------------------------------------
;;recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)

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
(defalias 'recent-files 'recentf-open-files "Open recent file list.")

(global-set-key (kbd "C-M-o") 'xsteve-ido-choose-from-recentf)

(eval-after-load "projectile"
  `(progn
     (defun smart-ido-recentf ()
       (interactive)
       (if (projectile-project-p)
           (call-interactively 'projectile-recentf)
         (call-interactively 'xsteve-ido-choose-from-recentf)))

     ;; global-set-key settings
     (global-set-key (kbd "C-M-o") 'smart-ido-recentf))
  )
;; Immediately close the current buffer.
(global-set-key (kbd "C-x k")   'kill-this-buffer)


;; buffer-move settings.
(require 'buffer-move)
(global-set-key [M-S-up]    'buf-move-up)
(global-set-key [M-S-down]  'buf-move-down)
(global-set-key [M-S-left]  'buf-move-left)
(global-set-key [M-S-right] 'buf-move-right)

(global-set-key [M-left]    'windmove-left)
(global-set-key [M-right]   'windmove-right)
(global-set-key [M-up]      'windmove-up)
(global-set-key [M-down]    'windmove-down)

;; select buffer-stable one
(global-set-key (kbd "M-N") 'next-buffer)
(global-set-key (kbd "M-P") 'previous-buffer)

(defun switch-to-other-buffer ()
  "切换到最近访问的buffer"
  (interactive)
  (unless (minibufferp)
    (switch-to-buffer (other-buffer))))

(global-set-key (kbd "M-'") 'previous-buffer)



;; fullscreen settings
;; ------------------------------------------------------------------
(when window-system
  (defun fullscreen ()
    "Fullscreen."
    (if (string= system-type "windows-nt")
        (message "I don't hot to fullscreen in windows-nt.")
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           ;; if first parameter is '1', can't toggle fullscreen status
                           '(1 "_NET_WM_STATE_FULLSCREEN" 0))
      ))

  (defun fullscreen-toggle ()
    "Toggle fullscreen status."
    (interactive)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           ;; if first parameter is '2', can toggle fullscreen status
                           '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
  ;; fullscreen with chromium
  (global-set-key '[f11] 'fullscreen-toggle)

  (fullscreen))


;; Window settings
;; ------------------------------------------------------------------
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

(global-set-key [(meta return)]       ' smart-window-customize)


;; Wheel settings
;; ==================================================================

;; Wheel up.
(global-set-key (kbd "<C-mouse-4>") 'font-small)

;; Wheel down.
(global-set-key (kbd "<C-mouse-5>") 'font-big)


(provide 'window-buffer-settings)
;; window-buffer-settings ends here.
;;;
