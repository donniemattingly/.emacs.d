(require 'alchemist)

;; Use paredit for most things, smartparens here

(add-hook 'elixir-mode-hook 'alchemist-mode)
(add-hook 'elixir-mode-hook 'company-mode)

(add-hook 'alchemist-iex-mode-hook '(lambda()
                                      (local-set-key (kbd "<tab>") 'company-complete)))


(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

(add-hook 'elixir-mode-hook
          (lambda ()
             (setq autopair-dont-activate t)))

(require 'smartparens-config)
(add-hook 'elixir-mode-hook #'smartparens-mode)

(setq company-idle-delay 0.1)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-flip-when-above t)
