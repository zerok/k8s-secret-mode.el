;;; k8s-secret-mode.el --- Minor mode for Kubernetes secret definitions -*- lexical-binding: t -*-

;;; Copyright (C) 2019 Horst Gutmann

;;; Author: Horst Gutmann <zerok@zerokspot.com>

;;; Commentary:
;;

;;; Code:
(define-minor-mode k8s-secret-mode
  "Toggle k8s-secret-mode."
  nil
  " k8s-secret")

(setq k8s-secret-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'k8s-secret/toggle)
    map))

(defvar k8s-secret--status nil)
(make-variable-buffer-local 'k8s-secret--status)

(defcustom k8s-secret-mode-hook nil
  "*Hook run by `k8s-secret-mode'."
  :type 'hook
:group 'k8s-secret)

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
