;;;;;
;; This file is part of gCoKe [ http://www.gcoke.org ]
;;
;; Copyright (C) 2010-  Sebastien Mosser
;;
;; gCoKe is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as 
;; published by the Free Software Foundation; either version 2 of 
;; the License, or (at your option) any later version.
;;
;; gCoKe is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public 
;; License along with gCoKe; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;;
;; @author   Main    Sebastien Mosser  [ sm@gcoke.org ]
;;;;;;

(require 'comint) ;; used to interact witht the gCoKe executable process

;;;;
;; Interacting with the compiler
;;;;
(defun gcoke-compile () (interactive)
  (let ((f (buffer-file-name)) (current (selected-frame)) 
        (frame (make-frame'((background-color . "grey80"))))
	(buffer (generate-new-buffer "gCoKe Compiled source file")))
    (select-frame frame)
    (switch-to-buffer buffer)
    (prolog-mode)
    (call-process-shell-command "gckc.sh" nil buffer t "-in" f)
    (select-frame current)))


;;;;
;; Interacting with the gCoKe logical engine (interactive mode)
;;;;

(defvar gcoke-engine-frame nil "gCoKe displayed frame")
(defvar gcoke-engine-process nil "gCoke running process")

(defun gcoke-init-display (b)
  (setq gcoke-engine-frame (make-frame '((background-color . "bisque2"))))
  (select-frame gcoke-engine-frame) 
  (switch-to-buffer b))

(defun gcoke-kill-engine () (interactive)
  (if gcoke-engine-process 
      (progn 
	(kill-buffer (process-buffer gcoke-engine-process))
	(delete-process gcoke-engine-process)))
  (setq gcoke-engine-process nil)
  (if gcoke-engine-frame (delete-frame gcoke-engine-frame))
  (setq gcoke-engine-frame nil))


(defun gcoke-run-engine () (interactive)
  (gcoke-kill-engine)
  (let* ((f (buffer-file-name))
	 (b (make-comint "gCoKe" "gcoke.sh" nil f "true")))
    (setq gcoke-engine-process (get-buffer-process b))
    (gcoke-init-display b)
    (setq mode-name "gCoKe"
	  comint-prompt-regexp "^| [ ?][- ] *")))

;;;;
;; gCoKe Keymap
;;;;

(defvar gcoke-map nil "Keymap used in the gCoKe major mode")
(setq gcoke-map (make-sparse-keymap "gCoKe keymap"))

(define-key gcoke-map (kbd "C-c C-r") 'gcoke-run-engine)
(define-key gcoke-map (kbd "C-c C-k") 'gcoke-kill-engine)


;;;;
;; gCoKe major mode definition & exportation
;;;;

(define-derived-mode gcoke-mode c-mode
  "gCoKe mode"
  "gCoke Major mode (support edition of gCoKe textual artefacts)"
  (setq mode-name "gCoKe Editor")
  (setq c-basic-offset 2)
  (use-local-map gcoke-map)

  (defvar source-keywords (regexp-opt '("require" "sniff") 'words))
  (defvar graph-keywords (regexp-opt '("graph") 'words))
  (setq gcoke-font-lock-keywords
	`((,source-keywords . font-lock-builtin-face)
	  (,graph-keywords . font-lock-keyword-face)))
  (setq font-lock-defaults '((gcoke-font-lock-keywords))))

(defun gcoke-mode-reload () (interactive)
  (unload-feature 'gcoke-mode)
  (load-file (concat (getenv "GCOKE_HOME") "/gcoke.el"))
  (gcoke-mode)
  (message "gCoKe: Major mode reloaded!"))
  
(provide 'gcoke-mode)
(setq auto-mode-alist (append '(("\\.gck$" . gcoke-mode)) auto-mode-alist))

