(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)

(mapc
 (lambda (line)
   (when (string-match "\\(.*\\)=\\(.*\\)" line)
     (setenv (match-string 1 line) (match-string 2 line))))
 (split-string
  (shell-command-to-string "opam config -env")
  ";[ \r\n\t]*"))

(setq utop-command "opam config exec -- utop -emacs")

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
  (autoload 'merlin-mode "merlin" nil t nil)
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)))

(defun ocaml-keys ()
    "For use in `go-mode-hook'."
    (local-set-key (kbd "C-c C-e") 'utop-eval-phrase)
    ;; more stuff here
    )

(add-hook 'tuareg-mode-hook 'ocaml-keys)
(add-hook 'tuareg-mode-hook 'merlin-mode)

; Make company aware of merlin
(with-eval-after-load 'company
 (add-to-list 'company-backends 'merlin-company-backend))
; Enable company on merlin managed buffers
(add-hook 'merlin-mode-hook 'company-mode)
; Or enable it globally:
; (add-hook 'after-init-hook 'global-company-mode)

(progn 
  (require 'tuareg))

