;
; File:          .emacs
; Programmer:    Brian Green
;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; add melpa
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;
; Append my private lisp libraries
;
(setq load-path (append load-path (list "/home/brian/lisp")))

;
; setup ansi color for shell mode
;
(load-library "ansi-color")
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;
; Make the system clipboard (C-c, C-v, etc.. work
; with emacs ring-save and yank (M-w, C-y etc...)
;
(setq x-select-enable-clipboard t)

;
; turn on line truncation for side by side windows
;
(setq truncate-partial-width-windows nil)

;
; only scroll one line please
;
(setq scroll-step 1)

;
; Make Text mode the default mode for new buffers.
;
(setq default-major-mode 'text-mode)

;
; Always use line number mode
;
(line-number-mode 1)

;
; Get rid of the ctrl-m in shell buffers
;
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;
; load python mode for SConstruct and SConscript
;
(setq auto-mode-alist (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("SConscript" . python-mode) auto-mode-alist))
; python mode hook
; no tabs
(add-hook 'python-mode-hook (lambda()
			      (setq indent-tabs-mode nil)))

;
; cc-mode customization
;
(setq-default indent-tabs-mode nil)
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (setq c-basic-offset 4)
  (setq c-echo-syntactic-information-p t)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label 0)
  (c-set-offset 'statement-block-intro 4)
  (c-set-offset 'innamespace 0)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

; Use Shift mouse2 to browse a url
(global-set-key [S-mouse-2] 'browse-url-at-mouse)


;
; new-shell command
;
(defun new-shell ()
"This function creates shell prosess buffer by different name. 
Created buffer names are shell-0, shell-1, shell-2 ... things. 
If there is no shell buffer, in this case at first shell-0 
is going to create. and next is shell-1 ....
After that if you despose shell buffer. For instance there are 4 
shell buffers from shell-0 to shell-3, then dispose shell-2. 
In this case next execution of this function will create 
shell-2 buffer instead of shell-4.  For internal use, 
You can't use *shell-keep-buffer*.
See also find-shell."
(interactive)
(let ((pre-buffer (get-buffer "*shell*"))
      (target-buffer-name))
  (if pre-buffer
      (progn
	(set-buffer "*shell*")
	(rename-buffer "*shell-keep-buffer*")))
  (setq target-buffer-name
	(let ((tmp-buffer)
	      (tmp-buffer-name nil)
	      (loop-counter 0)
	      (max-loop-counter 64))
	  (while (< loop-counter max-loop-counter)
	    (setq tmp-buffer-name (format "*shell-%d*" loop-counter))
	    (setq tmp-buffer (get-buffer tmp-buffer-name))
	    (if tmp-buffer
		(setq loop-counter (1+ loop-counter))
	      (setq loop-counter max-loop-counter)))
	  tmp-buffer-name))
  (shell)
  (rename-buffer target-buffer-name)
  (if pre-buffer
      (progn
	(set-buffer "*shell-keep-buffer*")
	(rename-buffer "*shell*")))))

;
; find-shell command
;
(defun find-shell (name)
"Find shell buffer named as NAME. If there is a buffer which name is NAME,
switch and display this NAME buffer. If there is not a buffer. This function
create new shell buffer named NAME and switch to it.  If you don't 
define NAME. This function calls new-shell command. You can get
new shell buffer which name has number from zero.
For internal use. You can't use *shell-keep-buffer*.
See also new-shell."
(interactive "sShell buffer name : ")
(if (not (equal name ""))
    (let (target-buffer)
      (setq target-buffer (get-buffer name))
      (if target-buffer
	  (switch-to-buffer name)
	(let (pre-buffer (get-buffer "*shell*"))
	  (if pre-buffer
	      (progn
		(set-buffer "*shell*")
		(rename-buffer "*shell-keep-buffer*")))
	  (shell)
	  (rename-buffer name)
	  (if pre-buffer
	      (progn
		(set-buffer "*shell-keep-buffer*")
		(rename-buffer "*shell*"))))))
  (new-shell)))

;
; emacs server
;
(server-start)

;
; I hate the blinking cursor!
;
(blink-cursor-mode 0)

;
; And tooltips also annoy me
;
(tooltip-mode 0)

;
; who needs a menubar?
;
(menu-bar-mode 0)
(tool-bar-mode 0)

;
; column number mode is pretty neat
;
(setq column-number-mode t)

;
; white on black
;
(defun bj-white-on-black ()
  (interactive)
  (set-background-color "black")
  (set-foreground-color "white"))
(global-set-key [f5] 'bj-white-on-black)

;
; white on grey
;
(defun bj-white-on-grey ()
  (interactive)
  (set-background-color "#222222")
  (set-foreground-color "white"))
(global-set-key [f6] 'bj-white-on-grey)

;
; black on grey
;
(defun bj-black-on-grey ()
  (interactive)
  (set-background-color "#DDDDDD")
  (set-foreground-color "black"))
(global-set-key [f7] 'bj-black-on-grey)

;
; black on white
;
(defun bj-black-on-white ()
  (interactive)
  (set-background-color "white")
  (set-foreground-color "black"))
(global-set-key [f8] 'bj-black-on-white)

;
; company
;
(require 'company)
(setq company-idle-delay 0.3)
(global-company-mode 1)
(global-set-key (kbd "C-<tab>") 'company-complete)

