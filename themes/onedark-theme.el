;;; onedark-theme.el --- Atom's iconic One Dark theme for Emacs

;; Copyright (C) 2025

;; Author: and19riv
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces, theme, dark, one-dark, atom
;; URL: 

;;; Commentary:

;; An Emacs theme based on Atom's iconic One Dark theme, specifically inspired by
;; the OneDarkPro.nvim implementation. Features authentic colors from the original
;; One Dark color scheme with excellent contrast and modern aesthetics.
;;
;; This theme provides comprehensive syntax highlighting and extensive plugin support
;; while maintaining the classic One Dark look that developers love.

;;; Code:

(deftheme onedark
  "Atom's iconic One Dark theme for Emacs.")

(let ((class '((class color) (min-colors 89)))
      ;; Core One Dark color palette
      (onedark-black       "#282c34")  ; Main background
      (onedark-bg-dark     "#21252b")  ; Darker background variant
      (onedark-bg-light    "#2c313a")  ; Lighter background variant
      (onedark-bg-lighter  "#3e4451")  ; Even lighter background
      
      ;; Foreground colors
      (onedark-white       "#abb2bf")  ; Main foreground
      (onedark-fg-dark     "#9ca3b2")  ; Darker foreground
      (onedark-fg-light    "#c8ccd4")  ; Lighter foreground
      
      ;; Gray shades
      (onedark-gray        "#5c6370")  ; Comment gray
      (onedark-gutter-gray "#4b5263")  ; Gutter gray
      (onedark-dark-gray   "#3a3f4b")  ; Dark gray
      (onedark-light-gray  "#7f848e")  ; Light gray
      
      ;; Core colors
      (onedark-red         "#e06c75")  ; Light red
      (onedark-dark-red    "#be5046")  ; Dark red
      (onedark-green       "#98c379")  ; Green
      (onedark-yellow      "#e5c07b")  ; Light yellow
      (onedark-dark-yellow "#d19a66")  ; Dark yellow/orange
      (onedark-blue        "#61afef")  ; Blue
      (onedark-purple      "#c678dd")  ; Magenta/purple
      (onedark-cyan        "#56b6c2")  ; Cyan
      
      ;; Additional UI colors
      (onedark-selection   "#3e4451")  ; Selection background
      (onedark-line        "#2c313a")  ; Current line
      (onedark-search      "#e5c07b")  ; Search highlight
      (onedark-visual      "#3e4451")  ; Visual selection
      
      ;; Special colors
      (onedark-cursor      "#528bff")  ; Cursor color
      (onedark-border      "#181a1f")  ; Border color
      (onedark-highlight   "#2c313a")  ; Highlight background
      
      ;; Diff colors
      (onedark-diff-add    "#98c379")  ; Added lines
      (onedark-diff-change "#e5c07b")  ; Changed lines
      (onedark-diff-delete "#e06c75")  ; Deleted lines
      
      ;; Status line colors
      (onedark-modeline-bg "#21252b")  ; Mode line background
      (onedark-modeline-fg "#9ca3b2")  ; Mode line foreground
      (onedark-modeline-inactive "#181a1f")) ; Inactive mode line

  (custom-theme-set-faces
   'onedark

   ;; Basic faces
   `(default ((,class (:background ,onedark-black :foreground ,onedark-white))))
   `(cursor ((,class (:background ,onedark-cursor))))
   `(region ((,class (:background ,onedark-selection))))
   `(highlight ((,class (:background ,onedark-highlight))))
   `(secondary-selection ((,class (:background ,onedark-visual))))
   `(lazy-highlight ((,class (:background ,onedark-dark-yellow :foreground ,onedark-black))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,onedark-blue))))
   `(font-lock-comment-face ((,class (:foreground ,onedark-gray :style italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,onedark-gray))))
   `(font-lock-constant-face ((,class (:foreground ,onedark-dark-yellow))))
   `(font-lock-doc-face ((,class (:foreground ,onedark-gray :style italic))))
   `(font-lock-function-name-face ((,class (:foreground ,onedark-blue :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,onedark-purple :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,onedark-red))))
   `(font-lock-preprocessor-face ((,class (:foreground ,onedark-purple))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,onedark-dark-yellow))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,onedark-purple))))
   `(font-lock-string-face ((,class (:foreground ,onedark-green))))
   `(font-lock-type-face ((,class (:foreground ,onedark-yellow))))
   `(font-lock-variable-name-face ((,class (:foreground ,onedark-red))))
   `(font-lock-warning-face ((,class (:foreground ,onedark-red :weight bold))))

   ;; UI elements
   `(fringe ((,class (:background ,onedark-bg-dark))))
   `(header-line ((,class (:background ,onedark-highlight :foreground ,onedark-white))))
   `(line-number ((,class (:background ,onedark-bg-dark :foreground ,onedark-gutter-gray))))
   `(line-number-current-line ((,class (:background ,onedark-line :foreground ,onedark-white))))
   `(minibuffer-prompt ((,class (:foreground ,onedark-blue :weight bold))))
   `(mode-line ((,class (:background ,onedark-modeline-bg :foreground ,onedark-white
                         :box (:line-width 1 :color ,onedark-border)))))
   `(mode-line-inactive ((,class (:background ,onedark-modeline-inactive :foreground ,onedark-gray))))
   `(vertical-border ((,class (:foreground ,onedark-border))))

   ;; Search and matching
   `(isearch ((,class (:background ,onedark-search :foreground ,onedark-black :weight bold))))
   `(isearch-fail ((,class (:background ,onedark-red :foreground ,onedark-white))))
   `(match ((,class (:background ,onedark-dark-yellow :foreground ,onedark-black))))

   ;; Links
   `(link ((,class (:foreground ,onedark-blue :underline t))))
   `(link-visited ((,class (:foreground ,onedark-purple :underline t))))

   ;; Parentheses matching
   `(show-paren-match ((,class (:background ,onedark-dark-gray :foreground ,onedark-white :weight bold))))
   `(show-paren-mismatch ((,class (:background ,onedark-red :foreground ,onedark-white :weight bold))))

   ;; Dired
   `(dired-directory ((,class (:foreground ,onedark-blue :weight bold))))
   `(dired-flagged ((,class (:foreground ,onedark-red))))
   `(dired-header ((,class (:foreground ,onedark-blue :weight bold))))
   `(dired-ignored ((,class (:foreground ,onedark-gray))))
   `(dired-mark ((,class (:foreground ,onedark-yellow))))
   `(dired-marked ((,class (:foreground ,onedark-yellow :weight bold))))
   `(dired-perm-write ((,class (:foreground ,onedark-red))))
   `(dired-symlink ((,class (:foreground ,onedark-cyan))))
   `(dired-warning ((,class (:foreground ,onedark-red :weight bold))))

   ;; Org mode
   `(org-block ((,class (:background ,onedark-bg-light))))
   `(org-block-begin-line ((,class (:foreground ,onedark-gray :style italic))))
   `(org-block-end-line ((,class (:foreground ,onedark-gray :style italic))))
   `(org-code ((,class (:foreground ,onedark-dark-yellow))))
   `(org-date ((,class (:foreground ,onedark-blue :underline t))))
   `(org-done ((,class (:foreground ,onedark-green :weight bold))))
   `(org-headline-done ((,class (:foreground ,onedark-gray))))
   `(org-hide ((,class (:foreground ,onedark-black))))
   `(org-level-1 ((,class (:foreground ,onedark-red :weight bold :height 1.3))))
   `(org-level-2 ((,class (:foreground ,onedark-blue :weight bold :height 1.2))))
   `(org-level-3 ((,class (:foreground ,onedark-yellow :weight bold :height 1.1))))
   `(org-level-4 ((,class (:foreground ,onedark-green :weight bold))))
   `(org-level-5 ((,class (:foreground ,onedark-purple :weight bold))))
   `(org-level-6 ((,class (:foreground ,onedark-cyan :weight bold))))
   `(org-link ((,class (:foreground ,onedark-blue :underline t))))
   `(org-special-keyword ((,class (:foreground ,onedark-gray))))
   `(org-table ((,class (:foreground ,onedark-white))))
   `(org-tag ((,class (:foreground ,onedark-dark-yellow :weight bold))))
   `(org-todo ((,class (:foreground ,onedark-red :weight bold))))

   ;; Company (completion)
   `(company-echo-common ((,class (:foreground ,onedark-red))))
   `(company-preview ((,class (:background ,onedark-highlight :foreground ,onedark-fg-dark))))
   `(company-preview-common ((,class (:background ,onedark-highlight :foreground ,onedark-blue))))
   `(company-preview-search ((,class (:background ,onedark-blue :foreground ,onedark-white))))
   `(company-scrollbar-bg ((,class (:background ,onedark-highlight))))
   `(company-scrollbar-fg ((,class (:background ,onedark-gray))))
   `(company-tooltip ((,class (:background ,onedark-bg-lighter :foreground ,onedark-white))))
   `(company-tooltip-annotation ((,class (:foreground ,onedark-gray))))
   `(company-tooltip-common ((,class (:foreground ,onedark-blue))))
   `(company-tooltip-common-selection ((,class (:foreground ,onedark-white))))
   `(company-tooltip-selection ((,class (:background ,onedark-selection :foreground ,onedark-white))))

   ;; Magit
   `(magit-section-heading ((,class (:foreground ,onedark-blue :weight bold))))
   `(magit-branch-local ((,class (:foreground ,onedark-blue))))
   `(magit-branch-remote ((,class (:foreground ,onedark-green))))
   `(magit-diff-added ((,class (:foreground ,onedark-diff-add))))
   `(magit-diff-removed ((,class (:foreground ,onedark-diff-delete))))
   `(magit-diff-context ((,class (:foreground ,onedark-fg-dark))))
   `(magit-hash ((,class (:foreground ,onedark-dark-yellow))))
   `(magit-log-author ((,class (:foreground ,onedark-purple))))
   `(magit-log-date ((,class (:foreground ,onedark-gray))))

   ;; Error and warning faces
   `(error ((,class (:foreground ,onedark-red :weight bold))))
   `(warning ((,class (:foreground ,onedark-yellow :weight bold))))
   `(success ((,class (:foreground ,onedark-green :weight bold))))

   ;; Flycheck
   `(flycheck-error ((,class (:underline (:style wave :color ,onedark-red)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,onedark-yellow)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,onedark-blue)))))

   ;; Helm
   `(helm-header ((,class (:background ,onedark-highlight :foreground ,onedark-white))))
   `(helm-source-header ((,class (:background ,onedark-blue :foreground ,onedark-white :weight bold))))
   `(helm-selection ((,class (:background ,onedark-selection))))
   `(helm-match ((,class (:foreground ,onedark-yellow :weight bold))))

   ;; Which-key
   `(which-key-key-face ((,class (:foreground ,onedark-blue :weight bold))))
   `(which-key-description-face ((,class (:foreground ,onedark-white))))
   `(which-key-group-description-face ((,class (:foreground ,onedark-purple))))
   `(which-key-command-description-face ((,class (:foreground ,onedark-green))))

   ;; Treemacs
   `(treemacs-directory-face ((,class (:foreground ,onedark-blue))))
   `(treemacs-file-face ((,class (:foreground ,onedark-white))))
   `(treemacs-root-face ((,class (:foreground ,onedark-blue :weight bold))))

   ;; Centaur Tabs (Doom Emacs compatibility)
   `(centaur-tabs-default ((,class (:background ,onedark-bg-dark :foreground ,onedark-gray))))
   `(centaur-tabs-selected ((,class (:background ,onedark-black :foreground ,onedark-white :weight bold))))
   `(centaur-tabs-unselected ((,class (:background ,onedark-modeline-bg :foreground ,onedark-gray))))
   `(centaur-tabs-selected-modified ((,class (:background ,onedark-black :foreground ,onedark-yellow :weight bold))))
   `(centaur-tabs-unselected-modified ((,class (:background ,onedark-modeline-bg :foreground ,onedark-dark-yellow))))
   `(centaur-tabs-active-bar-face ((,class (:background ,onedark-blue))))
   `(centaur-tabs-modified-marker-selected ((,class (:inherit centaur-tabs-selected-modified :foreground ,onedark-yellow))))
   `(centaur-tabs-modified-marker-unselected ((,class (:inherit centaur-tabs-unselected-modified :foreground ,onedark-dark-yellow))))
   
   ;; Additional centaur-tabs faces
   `(centaur-tabs-active-tab ((,class (:background ,onedark-black :foreground ,onedark-white :weight bold))))
   `(centaur-tabs-inactive-tab ((,class (:background ,onedark-modeline-bg :foreground ,onedark-gray))))
   `(centaur-tabs-selected-tab ((,class (:background ,onedark-blue :foreground ,onedark-white :weight bold))))

   ;; Doom-specific workspace tabs
   `(+workspace-tab-selected-face ((,class (:background ,onedark-blue :foreground ,onedark-white :weight bold))))
   `(+workspace-tab-face ((,class (:background ,onedark-modeline-bg :foreground ,onedark-gray))))

   ;; Tab Bar (built-in Emacs tabs)
   `(tab-bar ((,class (:background ,onedark-bg-dark :foreground ,onedark-white))))
   `(tab-bar-tab ((,class (:background ,onedark-black :foreground ,onedark-white :weight bold))))
   `(tab-bar-tab-inactive ((,class (:background ,onedark-modeline-bg :foreground ,onedark-gray))))

   ;; Tab Line (built-in Emacs tab line)
   `(tab-line ((,class (:background ,onedark-bg-dark :foreground ,onedark-white))))
   `(tab-line-tab ((,class (:background ,onedark-black :foreground ,onedark-white :weight bold))))
   `(tab-line-tab-inactive ((,class (:background ,onedark-modeline-bg :foreground ,onedark-gray))))
   `(tab-line-tab-current ((,class (:background ,onedark-black :foreground ,onedark-white :weight bold))))
   `(tab-line-close-highlight ((,class (:foreground ,onedark-red))))

   ;; Term colors (for terminal emulation)
   `(term-color-black ((,class (:background ,onedark-black :foreground ,onedark-black))))
   `(term-color-red ((,class (:background ,onedark-red :foreground ,onedark-red))))
   `(term-color-green ((,class (:background ,onedark-green :foreground ,onedark-green))))
   `(term-color-yellow ((,class (:background ,onedark-yellow :foreground ,onedark-yellow))))
   `(term-color-blue ((,class (:background ,onedark-blue :foreground ,onedark-blue))))
   `(term-color-magenta ((,class (:background ,onedark-purple :foreground ,onedark-purple))))
   `(term-color-cyan ((,class (:background ,onedark-cyan :foreground ,onedark-cyan))))
   `(term-color-white ((,class (:background ,onedark-white :foreground ,onedark-white))))

   ;; Diff mode
   `(diff-added ((,class (:background "#2d4427" :foreground ,onedark-diff-add))))
   `(diff-removed ((,class (:background "#4a2c2a" :foreground ,onedark-diff-delete))))
   `(diff-changed ((,class (:background "#4a4027" :foreground ,onedark-diff-change))))
   `(diff-header ((,class (:background ,onedark-highlight :foreground ,onedark-white))))
   `(diff-file-header ((,class (:background ,onedark-selection :foreground ,onedark-white :weight bold))))
   `(diff-hunk-header ((,class (:background ,onedark-bg-lighter :foreground ,onedark-blue))))

   ;; Ediff
   `(ediff-current-diff-A ((,class (:background "#4a2c2a"))))
   `(ediff-current-diff-B ((,class (:background "#2d4427"))))
   `(ediff-current-diff-C ((,class (:background "#4a4027"))))
   `(ediff-fine-diff-A ((,class (:background ,onedark-red :foreground ,onedark-white))))
   `(ediff-fine-diff-B ((,class (:background ,onedark-green :foreground ,onedark-black))))
   `(ediff-fine-diff-C ((,class (:background ,onedark-yellow :foreground ,onedark-black))))

   ;; Eshell
   `(eshell-prompt ((,class (:foreground ,onedark-blue :weight bold))))
   `(eshell-ls-directory ((,class (:foreground ,onedark-blue :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,onedark-green))))
   `(eshell-ls-symlink ((,class (:foreground ,onedark-cyan))))

   ;; Ivy/Counsel
   `(ivy-current-match ((,class (:background ,onedark-selection :foreground ,onedark-white))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,onedark-dark-yellow))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,onedark-blue :weight bold))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,onedark-purple :weight bold))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,onedark-green :weight bold))))

   ;; Vertico/Consult (modern completion)
   `(vertico-current ((,class (:background ,onedark-selection))))
   `(consult-file ((,class (:foreground ,onedark-white))))
   `(consult-line-number ((,class (:foreground ,onedark-gray))))

   ;; Marginalia
   `(marginalia-key ((,class (:foreground ,onedark-blue))))
   `(marginalia-documentation ((,class (:foreground ,onedark-gray :style italic))))

   ;; Orderless
   `(orderless-match-face-0 ((,class (:foreground ,onedark-blue :weight bold))))
   `(orderless-match-face-1 ((,class (:foreground ,onedark-dark-yellow :weight bold))))
   `(orderless-match-face-2 ((,class (:foreground ,onedark-green :weight bold))))
   `(orderless-match-face-3 ((,class (:foreground ,onedark-purple :weight bold))))

   ;; Dashboard
   `(dashboard-heading ((,class (:foreground ,onedark-blue :weight bold))))
   `(dashboard-text-banner ((,class (:foreground ,onedark-purple :weight bold))))
   `(dashboard-banner-logo-title ((,class (:foreground ,onedark-blue :weight bold))))

   ;; Doom modeline
   `(doom-modeline-bar ((,class (:background ,onedark-blue))))
   `(doom-modeline-project-dir ((,class (:foreground ,onedark-blue :weight bold))))
   `(doom-modeline-buffer-path ((,class (:foreground ,onedark-white :weight bold))))
   `(doom-modeline-buffer-file ((,class (:foreground ,onedark-white :weight bold))))
   `(doom-modeline-buffer-modified ((,class (:foreground ,onedark-yellow :weight bold))))
   `(doom-modeline-buffer-major-mode ((,class (:foreground ,onedark-blue :weight bold))))
   `(doom-modeline-info ((,class (:foreground ,onedark-green :weight bold))))
   `(doom-modeline-warning ((,class (:foreground ,onedark-yellow :weight bold))))
   `(doom-modeline-urgent ((,class (:foreground ,onedark-red :weight bold))))

   ;; LSP and Tree-sitter
   `(lsp-face-highlight-textual ((,class (:background ,onedark-highlight))))
   `(lsp-face-highlight-read ((,class (:background ,onedark-highlight))))
   `(lsp-face-highlight-write ((,class (:background ,onedark-highlight))))

   ;; Treesitter faces
   `(tree-sitter-hl-face:function ((,class (:foreground ,onedark-blue))))
   `(tree-sitter-hl-face:function.call ((,class (:foreground ,onedark-blue))))
   `(tree-sitter-hl-face:method ((,class (:foreground ,onedark-blue))))
   `(tree-sitter-hl-face:method.call ((,class (:foreground ,onedark-blue))))
   `(tree-sitter-hl-face:type ((,class (:foreground ,onedark-yellow))))
   `(tree-sitter-hl-face:type.builtin ((,class (:foreground ,onedark-yellow))))
   `(tree-sitter-hl-face:constructor ((,class (:foreground ,onedark-yellow))))
   `(tree-sitter-hl-face:variable ((,class (:foreground ,onedark-red))))
   `(tree-sitter-hl-face:variable.builtin ((,class (:foreground ,onedark-red))))
   `(tree-sitter-hl-face:property ((,class (:foreground ,onedark-red))))
   `(tree-sitter-hl-face:operator ((,class (:foreground ,onedark-cyan))))
   `(tree-sitter-hl-face:keyword ((,class (:foreground ,onedark-purple))))
   `(tree-sitter-hl-face:string ((,class (:foreground ,onedark-green))))
   `(tree-sitter-hl-face:number ((,class (:foreground ,onedark-dark-yellow))))
   `(tree-sitter-hl-face:boolean ((,class (:foreground ,onedark-dark-yellow))))
   `(tree-sitter-hl-face:comment ((,class (:foreground ,onedark-gray :style italic))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'onedark)

;;; onedark-theme.el ends here
