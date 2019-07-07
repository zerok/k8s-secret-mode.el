;;; k8s-secret-mode.el --- Minor mode for Kubernetes secret definitions -*- lexical-binding: t -*-

;;; Copyright (C) 2019 Horst Gutmann

;;; Author: Horst Gutmann <zerok@zerokspot.com>

;;; Commentary:
;; This package defines a new minor-mode which adds some encoding and
;; decoding functionality for Kubernetes Secret resources.
;;
;; Right now, this package requires also the installation of a custom
;; binary.  You can find details about the setup process on
;; https://github.com/zerok/k8s-secret-mode.el.

;;; Code:
(require 'auto-minor-mode)

(setq k8s-secret-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'k8s-secret/toggle)
    map))

(define-minor-mode k8s-secret-mode
  "Toggle k8s-secret-mode."
  :keymap k8s-secret-mode-map
  :lighter " k8s-secret"
  (when k8s-secret-mode
    (setq k8s-secret--status nil)))

(defvar k8s-secret--status nil)
(make-variable-buffer-local 'k8s-secret--status)

(defcustom k8s-secret-mode-hook nil
  "*Hook run by `k8s-secret-mode'."
  :type 'hook
:group 'k8s-secret)

(defun k8s-secret-mode-magic-matches ()
  (interactive)
  (save-excursion
    (progn
      ; This function is usually triggered inside a narrowed
      ; context. We need to widen it as the kind of a YAML file might
      ; be after the data.
      (widen)
      (let ((matches nil))
        (goto-char (point-min))
        (let ((curline (line-number-at-pos))
              (prevline -1))
      
          (while (not (or matches (equalp curline prevline)))
            (setq prevline curline)
            (forward-line)
            (let* ((start (progn
                            (beginning-of-line)
                            (point)))
                   (end (progn
                          (end-of-line)
                          (point)))
                   (line (buffer-substring start end)))
              (when (equalp line "kind: Secret")
                (setq matches t)))
            (setq curline (line-number-at-pos)))
          )
        (when (called-interactively-p 'any)
          (message "Matches secret: %s" matches))
        ; Restore narrowing for all other magic checks:
        (narrow-to-region (point-min)
                          (min (point-max)
                               (+ (point-min) magic-mode-regexp-match-limit)))
        matches))))
      
;; (add-to-list 'auto-minor-mode-magic-alist '(k8s-secret-mode-magic-matches . k8s-secret-mode))
(setq auto-minor-mode-magic-alist (list '(k8s-secret-mode-magic-matches . k8s-secret-mode)))

(defcustom k8s-secret-codec-bin (expand-file-name "~/src/github.com/zerok/k8s-secret-mode.el/bin/k8s-secret-codec")
  "*Path to the `k8s-secret-codec` binary."
  :type 'string
:group 'k8s-secret)

(defun k8s-secret/toggle ()
  "Encode or decode the current buffer depending on the current state."
  (interactive)
  (when (equal k8s-secret--status nil)
    (setq k8s-secret--status  "encoded"))

  (if (equal "encoded" k8s-secret--status)
      (k8s-secret/decode)
    (k8s-secret/encode)))

(defun k8s-secret/decode ()
  "Decoded the content of the current buffer."
  (interactive)
  (shell-command-on-region (point-min) (point-max) (format "%s --decode" k8s-secret-codec-bin) (current-buffer) t)
  (setq k8s-secret--status "decoded"))

(defun k8s-secret/encode ()
  "Decoded the content of the current buffer."
  (interactive)
  (shell-command-on-region (point-min) (point-max) (format "%s --encode" k8s-secret-codec-bin) (current-buffer) t)
  (setq k8s-secret--status "encoded"))

(provide 'k8s-secret-mode)
;;; k8s-secret-mode.el ends here
