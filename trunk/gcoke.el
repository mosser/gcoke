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
;; Emacs mode for the ADORE language
;;
;; References and inspirations: 
;;   - http://xahlee.org/emacs/elisp_syntax_coloring.html
;;   - http://www.logic.at/prolog/ediprolog/ediprolog.el
;;   - Emacs usual prolog.el mode
;;   - ADORE mode (http://www.adore-design.org)
;;;;

;;;;
;; Interactive function (called from M-x, menu or keyboard shortcut)
;;;;

;; Prompt for a graph name, and display the associated PNG picture
(defun gcoke-display-graph-as-png () (interactive)
  (let ((graph (read-from-minibuffer "Graph name: ")))
    (gcoke-build-png-from-graph graph)))

;;Prompt for a graph name, and display the associated Graphviz code
(defun gcoke-display-graph-as-dot () (interactive)
  (let* ((graph (read-from-minibuffer "Graph name: "))
 	 (f (make-temp-file "gcoke_dot_generation" nil ".dot"))
	 (goal (concat "graph:pull_from_db(" graph 
 		       ",G),graph_to_dot(G,'" f "')")))
    (gcoke-do-n-show goal f "graph -> dot transformation")))

;; Prompt for a composition name, and display the associated picture
(defun gcoke-display-composition-as-png () (interactive)
  (let ((compo (read-from-minibuffer "Composition name: ")))
    (gcoke-build-png-from-composition compo)))

;; Prompt for a composition name, and display the associated dot code
(defun gcoke-display-composition-as-dot () (interactive)
  (let* ((compo (read-from-minibuffer "Composition name: "))
 	 (f (make-temp-file "gcoke_dot_generation" nil ".dot"))
	 (goal (concat "build_compiled_composition(" compo 
 		       ",C),composition:as_dot_file([C],'" f "')")))
    (gcoke-do-n-show goal f "compo -> dot transformation")))

;; Display the complete composition flow as described in the file
(defun gcoke-display-composition-flow () (interactive)
  (gcoke-exec "composition:show_all" "composition flow visualization" t))

;; Display the current element (graph/composition), as a PNG picture
(defun gcoke-display-current-element ()
  (interactive)
  (let* ((info (gcoke-extract-current-element-information))
	 (kind (car info)) (name (cadr info)))
    (cond ((equal "graph" kind) (gcoke-build-png-from-graph name))
	  ((equal "composition" kind) (gcoke-build-png-from-composition name))
	  (t (message (concat "Unknown element type: " kind))))))

;; Prompt for a goal, and then shoot it on the current file
(defun gcoke-shoot ()
  (interactive)
  (let ((goal (read-from-minibuffer "Goal: ")))
    (gcoke-exec goal "gCoKe shooted goal" (not t))))

;; Naviguate to a required file
(defun gcoke-open-required ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let* ((raw-info (split-string (gcoke-read-current-line) " "))
	   (kind (car raw-info)) (raw-file-name (cadr raw-info))
	   (file-name (gcoke-match-regexp 
		       "\\([a-zA-Z0-9_]+/\\)*[a-zA-Z0-9_\.]+" 
		       raw-file-name))
	   (frame (make-frame)))
      (select-frame frame)
      (switch-to-buffer (find-file-noselect file-name))
      (cond ((equal kind "require") (gcoke-mode))
	    ((equal kind "require_raw") (prolog-mode))
	    (t (fundamental-mode))))))
;;;;
;; gCoKe Keymap
;;;;

(defvar gcoke-map nil "Keymap used in the gCoKe major mode")
(setq gcoke-map (make-sparse-keymap "gCoKe keymap"))

;; Keyboard shortcut
(define-key gcoke-map (kbd "C-c C-c") 'gcoke-compile)
(define-key gcoke-map (kbd "C-c C-r") 'gcoke-run-engine)
(define-key gcoke-map (kbd "C-c C-k") 'gcoke-kill-engine)
(define-key gcoke-map (kbd "C-c C-p") 'gcoke-display-current-element)
(define-key gcoke-map (kbd "C-c C-o") 'gcoke-open-required)

;; Menu bar definition
(let ((menuMap (make-sparse-keymap "gCoKe menu Keymap")))
  (define-key gcoke-map [menu-bar gcoke] (cons "gCoKe" menuMap))
  (define-key menuMap [reload] '("Reload gCoKe Mode" . gcoke-mode-reload))
  (define-key menuMap [s0] '("--"))
   (define-key menuMap [display-current] 
     '("Display current" . gcoke-display-current-element))
   (define-key menuMap [navigate-required] 
     '("Open required" . gcoke-open-required))
  (define-key menuMap [s1] '("--"))
  ;; Prolog sub menu
  (let ((prologMap (make-sparse-keymap "Prolog Keymap")))
    (define-key menuMap [prolog] (cons "Prolog" prologMap))
    (define-key prologMap [show] '("Show Prolog" . gcoke-compile))
    (define-key prologMap [shoot] '("Shoot!" . gcoke-shoot)))
  ;; Visualization submenu
  (let ((visualMap (make-sparse-keymap "gCoKe visualization keymap")))
    (define-key menuMap [visualization] (cons "Visualization" visualMap))
    (define-key visualMap [compo-flow] 
      '("Show composition flow" . gcoke-display-composition-flow))
    (define-key visualMap [s1] '("--"))
    (define-key visualMap [compo-dot] 
      '("Composition as Graphviz..." . gcoke-display-composition-as-dot))
    (define-key visualMap [compo-png] 
      '("Composition as PNG..." . gcoke-display-composition-as-png))
    (define-key visualMap [s0] '("--"))
    (define-key visualMap [graph-dot] 
      '("Graph as Graphviz..." . gcoke-display-graph-as-dot))
    (define-key visualMap [graph-png] 
      '("Graph as PNG..." . gcoke-display-graph-as-png)))
  ;; Engine submenu
  (let ((repMap (make-sparse-keymap "rep Keymap")))
    (define-key menuMap [rep] (cons "Engine" repMap))
    (define-key repMap [stop] '("Kill existing session" . gcoke-kill-engine))
    (define-key repMap [start] '("Run interactive session" . gcoke-run-engine)))
  ) ;; end of menu bar definition

;;;;
;; gCoKe major mode definition, reload hack & exportation
;;;;

;; the gcoke-mode is derived from the c-mode (e.g., syntax indentation rules)
(define-derived-mode gcoke-mode c-mode
  "gCoKe mode"
  "gCoke Major mode (support edition of gCoKe textual artefacts)"
  (setq mode-name "gCoKe Editor")
  (setq c-basic-offset 2)
  (use-local-map gcoke-map)
  
  ;; Syntax highlighing
  (defvar source-keywords 
    (regexp-opt '("require" "require_raw" "sniff") 'words))
  (defvar element-keywords 
    (regexp-opt '("graph" "composition") 'words))
  (defvar composition-regexp "[a-z][a-zA-Z0-9_]*[:blank:]*:")
  (defvar property-regexp "[a-z][a-zA-Z0-9_]*[:blank:]*=")
  (setq gcoke-font-lock-keywords
	`((,source-keywords . font-lock-builtin-face)
	  (,element-keywords . font-lock-keyword-face)
	  (,composition-regexp . font-lock-type-face)
	  (,property-regexp . font-lock-constant-face)))
  (setq font-lock-defaults '((gcoke-font-lock-keywords))))

;; Reload the gcoke-mode directly from Emacs (no restart needed)
(defun gcoke-mode-reload () (interactive)
  (unload-feature 'gcoke-mode)
  (load-file (concat (getenv "GCOKE_HOME") "/gcoke.el"))
  (gcoke-mode)
  (message "gCoKe: Major mode reloaded!"))
  
(provide 'gcoke-mode)
(setq auto-mode-alist (append '(("\\.gck$" . gcoke-mode)) auto-mode-alist))

;;;;
;; GCK compiler interface
;;;;

;; Compile the file associated to the current buffer, using gckc.sh
(defun gcoke-compile () (interactive)
  (let ((f (buffer-file-name)) (current (selected-frame)) 
        (frame (make-frame '((background-color . "grey95"))))
	(buffer (generate-new-buffer "gCoKe compiled source file")))
    (select-frame frame)
    (switch-to-buffer buffer)
    (prolog-mode)
    (call-process-shell-command "gckc.sh" nil buffer t "-in" f)
    (beginning-of-buffer)))

;;;;
;; Interacting with the gCoKe logical engine (interactive mode)
;;;;

;; The frame to be used to display the interactive engine
(defvar gcoke-engine-frame nil "gCoKe displayed frame")
;; The underlying swipl process
(defvar gcoke-engine-process nil "gCoKe running process")

;; Kill the existing interpreter (if any), and close the associated frame.
(defun gcoke-kill-engine () (interactive)
  (if gcoke-engine-process 
      (progn 
	(kill-buffer (process-buffer gcoke-engine-process))
	(delete-process gcoke-engine-process)))
  (setq gcoke-engine-process nil)
  (if gcoke-engine-frame (delete-frame gcoke-engine-frame))
  (setq gcoke-engine-frame nil))

;; Start a NEW interpreter on the current file, and display the associated frame
(defun gcoke-run-engine () (interactive)
  (gcoke-kill-engine)
  (let* ((file (buffer-file-name))
	 (b (make-comint "gCoKe" "gcoke.sh" nil file "true")))
    (setq gcoke-engine-process (get-buffer-process b))
    (setq gcoke-engine-frame (make-frame '((background-color . "bisque"))))
    (select-frame gcoke-engine-frame) 
    (switch-to-buffer b)
    (setq mode-name "gCoKe"
	  comint-prompt-regexp "^| [ ?][- ] *")))
;;;;
;; Non-interactive engine handling
;;;;

;; Execute a prolog goal on the current file
;;   @param goal: the goal to be executed
;;   @param buffer-name the name associated to the execution buffer
;;   @param shouldKill if 'true', the execution buffer is killed after success.
(defun gcoke-exec (goal buffer-name shouldKill)
  (let ((f (buffer-file-name)) (current (selected-frame)) 
	(frame (make-frame)) (buffer (generate-new-buffer buffer-name)))
    (select-frame frame) (switch-to-buffer buffer)
    (let ((code (call-process-shell-command "gcoke.sh" nil buffer t
					    f (concat "\"" goal ",halt.\""))))
      (if (and shouldKill (= code 0)) (delete-frame frame)) code)))

;; Execute a prolog goal on the current file, and open a buffer on the 
;; resulting file.
;;   @param goal the goal to be executed
;;   @param output-file the name of the file which contains the final result
;;   @param buffer-name the name associated to the execution buffer
(defun gcoke-do-n-show (goal output-file buffer-name)
  (let ((code (gcoke-exec goal buffer-name t)))
    (if (= 0 code)
	(let ((frame (make-frame '((background-color . "grey95")))))
	  (select-frame frame)
	  (switch-to-buffer (find-file-noselect output-file))))))

;;;;
;; Helper functions 
;;;;

;; Read the content of the current line
(defun gcoke-read-current-line ()
    (let ((start (point))) 
      (forward-line 1) (forward-char -1) 
      (let ((raw (buffer-substring start (point))))
	(substring-no-properties raw))))

;; match a regular expression and return the matched data, nil instead
(defun gcoke-match-regexp (regexp str)
  (if (string-match regexp str)
      (substring str (car (match-data)) (cadr (match-data)))
    nil))

;; Extract information (as a list '(kind name)) about the current element
(defun gcoke-extract-current-element-information ()
  (save-excursion
    (c-beginning-of-defun)
    (let* ((line (gcoke-read-current-line)) (raw (split-string line " "))
	   (kind (car raw)) 
	   (name (gcoke-match-regexp "[a-z][a-zA-Z0-9_]*" (cadr raw))))
      (list kind name))))

;; For a given graph, display the associated PNG file
(defun gcoke-build-png-from-graph (graph-name) 
  (gcoke-exec (concat "graph:pull_from_db(" graph-name ",G),graph_show(G)")
	      "graph -> png transformation" t))

(defun gcoke-build-png-from-composition (compo-name) 
  (gcoke-exec (concat "composition:pull_from_db(" compo-name ",G)"
		      ",composition:show([G])")
	      "composition -> png transformation" t))
