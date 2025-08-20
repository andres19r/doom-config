;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Andres Rivero"
      user-mail-address "andres19rivero@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 13))
;; doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'kanagawa-lotus)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq company-minimum-prefix-length 1
      company-idle-delay 0.1)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(map! :leader
      :desc "Run LSP"
      "l" #'lsp)

(map! :leader
      :desc "No highlight"
      "hh" #'evil-ex-nohighlight)

(map! :leader
      :desc "Prettier everything"
      "cp" #'prettier-js-prettify)

(map! :leader
      :desc "Activate python venv"
      "va" #'pyvenv-activate)

(map! :leader
      :desc "Deactivate python venv"
      "vd" #'pyvenv-deactivate)

(map! :leader
      :desc "Dired create file"
      "d" #'dired-create-empty-file)

(map! :leader
      :desc "Python black buffer"
      "cb" #'python-black-buffer)

(map! :leader
      :desc "Go to next tab"
      "j" #'+tabs:next-or-goto)

(map! :leader
      :desc "Go to previous tab"
      "k" #'+tabs:previous-or-goto)

(let ((alternatives '("dragon.png"
                      "img-1.png"
                      "img-2.png"
                      "img-3.png"
                      "doom.svg")))
  (setq fancy-splash-image
        (concat doom-user-dir "splash/"
                (nth (random (length alternatives)) alternatives))))

(after! centaur-tabs
  (setq centaur-tabs-set-bar 'right))

(advice-add 'json-parse-string :around
            (lambda (orig string &rest rest)
              (apply orig (s-replace "\\u0000" "" string)
                     rest)))

(advice-add 'json-parse-buffer :around
            (lambda (oldfn &rest args)
	      (save-excursion
                (while (search-forward "\\u0000" nil t)
                  (replace-match "" nil t)))
	      (apply oldfn args)))

;; (setq lsp-headerline-breadcrumb-enable t)
;; (lsp-treemacs-sync-mode 1)

(require 'dap-firefox)
(require 'dap-chrome)
(require 'dap-node)
;; DAP Mode setup
(use-package! dap-mode
  :after lsp-mode
  :config
  ;; Enable dap-mode
  (dap-auto-configure-mode)

  ;; Enable tooltip support
  (dap-tooltip-mode 1)

  ;; Use posframe for tooltips (optional, but looks nicer)
  (when (display-graphic-p)
    (dap-tooltip-mode 1))

  ;; Enable the UI controls
  (dap-ui-controls-mode 1))
(use-package! dap-node
  :after dap-mode
  :config
  ;; Register Node.js debug templates
  (dap-node-setup))

(setq-default tab-width 2)
(setq-default evil-shift-width 2)
;; (add-hook 'after-init-hook #'prettier-js-mode)
;; (set-frame-parameter (selected-frame) 'alpha '(95 95))
;; (add-to-list 'default-frame-alist '(alpha 95 95))
(defun my-treemacs-auto-fit-width ()
  (when (treemacs-is-treemacs-window? (selected-window))
    (treemacs-fit-window-width)))

(advice-add 'treemacs-select-window :after #'my-treemacs-auto-fit-width)
(treemacs-project-follow-mode 1)
(treemacs-follow-mode 1)

(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'scss-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'js-mode-hook 'emmet-mode)
(add-hook 'jsx-mode-hook 'emmet-mode)
(add-hook 'js2-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'prettier-js-mode)
(add-hook 'css-mode-hook 'prettier-js-mode)
(add-hook 'scss-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'jsx-mode-hook 'prettier-js-mode)
(add-hook 'js2-mode-hook 'prettier-js-mode)

(add-hook 'html-mode-hook #'lsp)
(add-hook 'css-mode-hook #'lsp)
(add-hook 'scss-mode-hook #'lsp)

(global-visual-line-mode t)
;; (setq treemacs-default-visit-action 'treemacs-visit-node-close-treemacs)

(use-package! evil-escape :init (setq evil-escape-key-sequence "jk"))

(use-package! org-roam
  :custom
  (org-roam-directory "~/org/roam/")
  :config
  (org-roam-db-autosync-enable))

;; (setq lsp-python-ms-python-executable "python3")
;; Angular debugging templates
(after! dap-mode
  ;; Angular serve debugging (corrected with absolute path)
  (dap-register-debug-template "Angular Serve"
                               (list :type "node"
                                     :request "launch"
                                     :name "Angular Serve"
                                     :program "${workspaceFolder}/node_modules/@angular/cli/bin/ng"
                                     :args ["serve"]
                                     :console "integratedTerminal"
                                     :cwd "${workspaceFolder}"))

  ;; Angular tests debugging (also corrected)
  (dap-register-debug-template "Angular Tests"
                               (list :type "node"
                                     :request "launch"
                                     :name "Angular Tests"
                                     :program "${workspaceFolder}/node_modules/@angular/cli/bin/ng"
                                     :args ["test" "--watch=false"]
                                     :console "integratedTerminal"
                                     :cwd "${workspaceFolder}"))

  ;; Alternative using npx (often more reliable)
  (dap-register-debug-template "Angular Serve (npx)"
                               (list :type "node"
                                     :request "launch"
                                     :name "Angular Serve (npx)"
                                     :program "${workspaceFolder}/node_modules/.bin/npx"
                                     :args ["ng" "serve"]
                                     :console "integratedTerminal"
                                     :cwd "${workspaceFolder}"))

  ;; Chrome debugging for Angular app
  (dap-register-debug-template "Angular Chrome"
                               (list :type "chrome"
                                     :request "launch"
                                     :name "Angular Chrome"
                                     :url "http://localhost:4200"
                                     :webRoot "${workspaceFolder}")))

