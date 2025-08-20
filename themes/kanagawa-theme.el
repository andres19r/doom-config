;;; kanagawa-theme.el --- Kanagawa dark theme for Emacs

;; Copyright (C) 2025

;; Author: and19riv
;; Version: 2.0.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces, theme, dark, kanagawa, wave
;; URL: 

;;; Commentary:

;; A dark theme inspired by the famous woodblock print "The Great Wave off Kanagawa"
;; by Katsushika Hokusai. The default "wave" variant captures the essence of the
;; iconic artwork with deep blues, soft whites, and earthy accents that provide
;; excellent contrast for comfortable coding sessions.
;;
;; This theme provides comprehensive syntax highlighting and extensive plugin support
;; while maintaining the classic Kanagawa aesthetic that developers love.

;;; Code:

(deftheme kanagawa
  "Kanagawa - A dark theme inspired by The Great Wave off Kanagawa.")

(let ((class '((class color) (min-colors 89)))
      ;; Background shades
      (kanagawa-bg-0     "#16161D")  ; Dark background (statuslines and floating windows)
      (kanagawa-bg-1     "#1F1F28")  ; Default background
      (kanagawa-bg-2     "#2A2A37")  ; Lighter background (colorcolumn, folds)
      (kanagawa-bg-3     "#363646")  ; Lighter background (cursorline)
      (kanagawa-bg-4     "#54546D")  ; Darker foreground (line numbers, fold column)
      
      ;; Foreground and comments
      (kanagawa-fg-0     "#DCD7BA")  ; Default foreground
      (kanagawa-fg-1     "#C8C093")  ; Dark foreground (statuslines)
      (kanagawa-gray-0   "#727169")  ; Comments
      (kanagawa-gray-1   "#9E9B93")  ; Inactive elements
      (kanagawa-gray-2   "#7A8382")  ; Secondary text
      
      ;; Wave colors (blues and aquas)
      (kanagawa-blue-0   "#7E9CD8")  ; Crystal blue (main blue)
      (kanagawa-blue-1   "#223249")  ; Wave blue 1 (popup background, visual selection)
      (kanagawa-blue-2   "#2D4F67")  ; Wave blue 2 (popup selection background, search)
      (kanagawa-blue-3   "#658594")  ; Dragon blue (medium wave blue)
      (kanagawa-cyan-0   "#7FB4CA")  ; Spring blue (wave foam)
      (kanagawa-cyan-1   "#6A9589")  ; Wave aqua 1
      (kanagawa-aqua-0   "#7AA89F")  ; Light aqua
      
      ;; Earth tones (from Mount Fuji and boats)
      (kanagawa-red-0    "#C34043")  ; Autumn red (Mount Fuji red)
      (kanagawa-red-1    "#E82424")  ; Samurai red (error red)
      (kanagawa-orange-0 "#FFA066")  ; Warm sunset orange
      (kanagawa-orange-1 "#FF9E3B")  ; Ronin yellow (warning)
      (kanagawa-yellow-0 "#E6C384")  ; Carp yellow (golden highlights)
      (kanagawa-yellow-1 "#DCA561")  ; Autumn yellow
      
      ;; Nature colors
      (kanagawa-green-0  "#76946A")  ; Autumn green (muted sea green)
      (kanagawa-green-1  "#98BB6C")  ; Bright green
      
      ;; Purple and violet tones
      (kanagawa-purple-0 "#957FB8")  ; Oni violet (soft purple)
      (kanagawa-purple-1 "#938AA9")  ; Spring violet 1
      (kanagawa-purple-2 "#9CABCA")  ; Spring violet 2 (light violet)
      (kanagawa-violet-0 "#B8B4D0")  ; Oni violet 2 (light purple)
      
      ;; Winter diff colors
      (kanagawa-winter-green  "#2B3328")  ; Diff add background
      (kanagawa-winter-yellow "#49443C")  ; Diff change background
      (kanagawa-winter-red    "#43242B")  ; Diff delete background
      (kanagawa-winter-blue   "#252535")  ; Diff line background
      
      ;; Special UI colors
      (kanagawa-cursor    "#7FB4CA")  ; Cursor color (spring blue)
      (kanagawa-selection "#2D4F67")  ; Text selection
      (kanagawa-search    "#E6C384")  ; Search highlight
      (kanagawa-match     "#658594")  ; Matching elements
      
      ;; Border and UI elements
      (kanagawa-border    "#54546D")  ; Borders and separators
      (kanagawa-shadow    "#727169")) ; Shadows and inactive elements

  (custom-theme-set-faces
   'kanagawa

   ;; Basic faces
   `(default ((,class (:background ,kanagawa-bg-1 :foreground ,kanagawa-fg-0))))
   `(cursor ((,class (:background ,kanagawa-cursor))))
   `(region ((,class (:background ,kanagawa-blue-1))))
   `(highlight ((,class (:background ,kanagawa-bg-3))))
   `(secondary-selection ((,class (:background ,kanagawa-blue-2))))
   `(lazy-highlight ((,class (:background ,kanagawa-blue-3 :foreground ,kanagawa-fg-0))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,kanagawa-blue-0))))
   `(font-lock-comment-face ((,class (:foreground ,kanagawa-gray-0 :style italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,kanagawa-gray-0))))
   `(font-lock-constant-face ((,class (:foreground ,kanagawa-orange-0))))
   `(font-lock-doc-face ((,class (:foreground ,kanagawa-gray-2 :style italic))))
   `(font-lock-function-name-face ((,class (:foreground ,kanagawa-blue-0 :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,kanagawa-purple-0 :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,kanagawa-red-1))))
   `(font-lock-preprocessor-face ((,class (:foreground ,kanagawa-cyan-0))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,kanagawa-yellow-0))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,kanagawa-purple-0))))
   `(font-lock-string-face ((,class (:foreground ,kanagawa-green-0))))
   `(font-lock-type-face ((,class (:foreground ,kanagawa-yellow-0))))
   `(font-lock-variable-name-face ((,class (:foreground ,kanagawa-fg-1))))
   `(font-lock-warning-face ((,class (:foreground ,kanagawa-red-1 :weight bold))))

   ;; UI elements
   `(fringe ((,class (:background ,kanagawa-bg-0))))
   `(header-line ((,class (:background ,kanagawa-bg-3 :foreground ,kanagawa-fg-0))))
   `(line-number ((,class (:background ,kanagawa-bg-0 :foreground ,kanagawa-bg-4))))
   `(line-number-current-line ((,class (:background ,kanagawa-bg-3 :foreground ,kanagawa-cursor))))
   `(minibuffer-prompt ((,class (:foreground ,kanagawa-blue-0 :weight bold))))
   `(mode-line ((,class (:background ,kanagawa-bg-3 :foreground ,kanagawa-fg-0
                         :box (:line-width 1 :color ,kanagawa-border)))))
   `(mode-line-inactive ((,class (:background ,kanagawa-bg-0 :foreground ,kanagawa-gray-1))))
   `(vertical-border ((,class (:foreground ,kanagawa-bg-3))))

   ;; Search and matching
   `(isearch ((,class (:background ,kanagawa-search :foreground ,kanagawa-bg-1 :weight bold))))
   `(isearch-fail ((,class (:background ,kanagawa-red-1 :foreground ,kanagawa-fg-0))))
   `(match ((,class (:background ,kanagawa-blue-3 :foreground ,kanagawa-fg-0))))

   ;; Links
   `(link ((,class (:foreground ,kanagawa-cyan-0 :underline t))))
   `(link-visited ((,class (:foreground ,kanagawa-purple-0 :underline t))))

   ;; Parentheses matching
   `(show-paren-match ((,class (:background ,kanagawa-blue-2 :foreground ,kanagawa-fg-0 :weight bold))))
   `(show-paren-mismatch ((,class (:background ,kanagawa-red-1 :foreground ,kanagawa-fg-0 :weight bold))))

   ;; Dired
   `(dired-directory ((,class (:foreground ,kanagawa-blue-0 :weight bold))))
   `(dired-flagged ((,class (:foreground ,kanagawa-red-1))))
   `(dired-header ((,class (:foreground ,kanagawa-cyan-0 :weight bold))))
   `(dired-ignored ((,class (:foreground ,kanagawa-gray-0))))
   `(dired-mark ((,class (:foreground ,kanagawa-yellow-0))))
   `(dired-marked ((,class (:foreground ,kanagawa-yellow-0 :weight bold))))
   `(dired-perm-write ((,class (:foreground ,kanagawa-red-1))))
   `(dired-symlink ((,class (:foreground ,kanagawa-cyan-0))))
   `(dired-warning ((,class (:foreground ,kanagawa-orange-1 :weight bold))))

   ;; Org mode
   `(org-block ((,class (:background ,kanagawa-bg-0))))
   `(org-block-begin-line ((,class (:foreground ,kanagawa-gray-0 :style italic))))
   `(org-block-end-line ((,class (:foreground ,kanagawa-gray-0 :style italic))))
   `(org-code ((,class (:foreground ,kanagawa-orange-0))))
   `(org-date ((,class (:foreground ,kanagawa-blue-0 :underline t))))
   `(org-done ((,class (:foreground ,kanagawa-green-0 :weight bold))))
   `(org-headline-done ((,class (:foreground ,kanagawa-gray-1))))
   `(org-hide ((,class (:foreground ,kanagawa-bg-1))))
   `(org-level-1 ((,class (:foreground ,kanagawa-blue-0 :weight bold :height 1.3))))
   `(org-level-2 ((,class (:foreground ,kanagawa-cyan-0 :weight bold :height 1.2))))
   `(org-level-3 ((,class (:foreground ,kanagawa-purple-0 :weight bold :height 1.1))))
   `(org-level-4 ((,class (:foreground ,kanagawa-yellow-0 :weight bold))))
   `(org-level-5 ((,class (:foreground ,kanagawa-green-0 :weight bold))))
   `(org-level-6 ((,class (:foreground ,kanagawa-orange-0 :weight bold))))
   `(org-link ((,class (:foreground ,kanagawa-cyan-0 :underline t))))
   `(org-special-keyword ((,class (:foreground ,kanagawa-gray-0))))
   `(org-table ((,class (:foreground ,kanagawa-fg-1))))
   `(org-tag ((,class (:foreground ,kanagawa-orange-0 :weight bold))))
   `(org-todo ((,class (:foreground ,kanagawa-red-1 :weight bold))))

   ;; Company (completion)
   `(company-echo-common ((,class (:foreground ,kanagawa-red-1))))
   `(company-preview ((,class (:background ,kanagawa-bg-3 :foreground ,kanagawa-fg-1))))
   `(company-preview-common ((,class (:background ,kanagawa-bg-3 :foreground ,kanagawa-blue-0))))
   `(company-preview-search ((,class (:background ,kanagawa-blue-0 :foreground ,kanagawa-fg-0))))
   `(company-scrollbar-bg ((,class (:background ,kanagawa-bg-3))))
   `(company-scrollbar-fg ((,class (:background ,kanagawa-gray-0))))
   `(company-tooltip ((,class (:background ,kanagawa-bg-3 :foreground ,kanagawa-fg-0))))
   `(company-tooltip-annotation ((,class (:foreground ,kanagawa-gray-1))))
   `(company-tooltip-common ((,class (:foreground ,kanagawa-blue-0))))
   `(company-tooltip-common-selection ((,class (:foreground ,kanagawa-fg-0))))
   `(company-tooltip-selection ((,class (:background ,kanagawa-blue-2 :foreground ,kanagawa-fg-0))))

   ;; Magit
   `(magit-section-heading ((,class (:foreground ,kanagawa-cyan-0 :weight bold))))
   `(magit-branch-local ((,class (:foreground ,kanagawa-blue-0))))
   `(magit-branch-remote ((,class (:foreground ,kanagawa-green-0))))
   `(magit-diff-added ((,class (:foreground ,kanagawa-green-0))))
   `(magit-diff-removed ((,class (:foreground ,kanagawa-red-1))))
   `(magit-diff-context ((,class (:foreground ,kanagawa-fg-1))))
   `(magit-hash ((,class (:foreground ,kanagawa-orange-0))))
   `(magit-log-author ((,class (:foreground ,kanagawa-purple-0))))
   `(magit-log-date ((,class (:foreground ,kanagawa-gray-1))))

   ;; Error and warning faces
   `(error ((,class (:foreground ,kanagawa-red-1 :weight bold))))
   `(warning ((,class (:foreground ,kanagawa-orange-1 :weight bold))))
   `(success ((,class (:foreground ,kanagawa-green-0 :weight bold))))

   ;; Flycheck
   `(flycheck-error ((,class (:underline (:style wave :color ,kanagawa-red-1)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,kanagawa-orange-1)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,kanagawa-blue-0)))))

   ;; Helm
   `(helm-header ((,class (:background ,kanagawa-bg-3 :foreground ,kanagawa-fg-0))))
   `(helm-source-header ((,class (:background ,kanagawa-blue-0 :foreground ,kanagawa-fg-0 :weight bold))))
   `(helm-selection ((,class (:background ,kanagawa-blue-2))))
   `(helm-match ((,class (:foreground ,kanagawa-yellow-0 :weight bold))))

   ;; Which-key
   `(which-key-key-face ((,class (:foreground ,kanagawa-blue-0 :weight bold))))
   `(which-key-description-face ((,class (:foreground ,kanagawa-fg-0))))
   `(which-key-group-description-face ((,class (:foreground ,kanagawa-purple-0))))
   `(which-key-command-description-face ((,class (:foreground ,kanagawa-cyan-0))))

   ;; Treemacs
   `(treemacs-directory-face ((,class (:foreground ,kanagawa-blue-0))))
   `(treemacs-file-face ((,class (:foreground ,kanagawa-fg-0))))
   `(treemacs-root-face ((,class (:foreground ,kanagawa-cyan-0 :weight bold))))

   ;; Centaur Tabs (Doom Emacs compatibility)
   `(centaur-tabs-default ((,class (:background ,kanagawa-bg-0 :foreground ,kanagawa-gray-1))))
   `(centaur-tabs-selected ((,class (:background ,kanagawa-bg-1 :foreground ,kanagawa-fg-0 :weight bold))))
   `(centaur-tabs-unselected ((,class (:background ,kanagawa-bg-2 :foreground ,kanagawa-gray-0))))
   `(centaur-tabs-selected-modified ((,class (:background ,kanagawa-bg-1 :foreground ,kanagawa-orange-0 :weight bold))))
   `(centaur-tabs-unselected-modified ((,class (:background ,kanagawa-bg-2 :foreground ,kanagawa-orange-1))))
   `(centaur-tabs-active-bar-face ((,class (:background ,kanagawa-blue-0))))
   `(centaur-tabs-modified-marker-selected ((,class (:inherit centaur-tabs-selected-modified :foreground ,kanagawa-orange-0))))
   `(centaur-tabs-modified-marker-unselected ((,class (:inherit centaur-tabs-unselected-modified :foreground ,kanagawa-orange-1))))
   
   ;; Additional centaur-tabs faces
   `(centaur-tabs-active-tab ((,class (:background ,kanagawa-bg-1 :foreground ,kanagawa-fg-0 :weight bold))))
   `(centaur-tabs-inactive-tab ((,class (:background ,kanagawa-bg-2 :foreground ,kanagawa-gray-0))))
   `(centaur-tabs-selected-tab ((,class (:background ,kanagawa-blue-0 :foreground ,kanagawa-fg-0 :weight bold))))

   ;; Doom-specific workspace tabs
   `(+workspace-tab-selected-face ((,class (:background ,kanagawa-blue-0 :foreground ,kanagawa-fg-0 :weight bold))))
   `(+workspace-tab-face ((,class (:background ,kanagawa-bg-2 :foreground ,kanagawa-gray-0))))

   ;; Tab Bar (built-in Emacs tabs)
   `(tab-bar ((,class (:background ,kanagawa-bg-0 :foreground ,kanagawa-fg-0))))
   `(tab-bar-tab ((,class (:background ,kanagawa-bg-1 :foreground ,kanagawa-fg-0 :weight bold))))
   `(tab-bar-tab-inactive ((,class (:background ,kanagawa-bg-2 :foreground ,kanagawa-gray-0))))

   ;; Tab Line (built-in Emacs tab line)
   `(tab-line ((,class (:background ,kanagawa-bg-0 :foreground ,kanagawa-fg-0))))
   `(tab-line-tab ((,class (:background ,kanagawa-bg-1 :foreground ,kanagawa-fg-0 :weight bold))))
   `(tab-line-tab-inactive ((,class (:background ,kanagawa-bg-2 :foreground ,kanagawa-gray-0))))
   `(tab-line-tab-current ((,class (:background ,kanagawa-bg-1 :foreground ,kanagawa-fg-0 :weight bold))))
   `(tab-line-close-highlight ((,class (:foreground ,kanagawa-red-1))))

   ;; Term colors (for terminal emulation)
   `(term-color-black ((,class (:background ,kanagawa-bg-1 :foreground ,kanagawa-bg-1))))
   `(term-color-red ((,class (:background ,kanagawa-red-0 :foreground ,kanagawa-red-0))))
   `(term-color-green ((,class (:background ,kanagawa-green-0 :foreground ,kanagawa-green-0))))
   `(term-color-yellow ((,class (:background ,kanagawa-yellow-0 :foreground ,kanagawa-yellow-0))))
   `(term-color-blue ((,class (:background ,kanagawa-blue-0 :foreground ,kanagawa-blue-0))))
   `(term-color-magenta ((,class (:background ,kanagawa-purple-0 :foreground ,kanagawa-purple-0))))
   `(term-color-cyan ((,class (:background ,kanagawa-cyan-0 :foreground ,kanagawa-cyan-0))))
   `(term-color-white ((,class (:background ,kanagawa-fg-0 :foreground ,kanagawa-fg-0))))

   ;; Diff mode
   `(diff-added ((,class (:background ,kanagawa-winter-green :foreground ,kanagawa-green-0))))
   `(diff-removed ((,class (:background ,kanagawa-winter-red :foreground ,kanagawa-red-0))))
   `(diff-changed ((,class (:background ,kanagawa-winter-yellow :foreground ,kanagawa-yellow-1))))
   `(diff-header ((,class (:background ,kanagawa-bg-3 :foreground ,kanagawa-fg-0))))
   `(diff-file-header ((,class (:background ,kanagawa-blue-2 :foreground ,kanagawa-fg-0 :weight bold))))
   `(diff-hunk-header ((,class (:background ,kanagawa-bg-2 :foreground ,kanagawa-cyan-0))))

   ;; Ediff
   `(ediff-current-diff-A ((,class (:background ,kanagawa-winter-red))))
   `(ediff-current-diff-B ((,class (:background ,kanagawa-winter-green))))
   `(ediff-current-diff-C ((,class (:background ,kanagawa-winter-yellow))))
   `(ediff-fine-diff-A ((,class (:background ,kanagawa-red-0 :foreground ,kanagawa-fg-0))))
   `(ediff-fine-diff-B ((,class (:background ,kanagawa-green-0 :foreground ,kanagawa-bg-1))))
   `(ediff-fine-diff-C ((,class (:background ,kanagawa-yellow-0 :foreground ,kanagawa-bg-1))))

   ;; Eshell
   `(eshell-prompt ((,class (:foreground ,kanagawa-blue-0 :weight bold))))
   `(eshell-ls-directory ((,class (:foreground ,kanagawa-blue-0 :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,kanagawa-green-0))))
   `(eshell-ls-symlink ((,class (:foreground ,kanagawa-cyan-0))))

   ;; Ivy/Counsel
   `(ivy-current-match ((,class (:background ,kanagawa-blue-2 :foreground ,kanagawa-fg-0))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,kanagawa-orange-0))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,kanagawa-blue-0 :weight bold))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,kanagawa-purple-0 :weight bold))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,kanagawa-cyan-0 :weight bold))))

   ;; Vertico/Consult (modern completion)
   `(vertico-current ((,class (:background ,kanagawa-blue-2))))
   `(consult-file ((,class (:foreground ,kanagawa-fg-0))))
   `(consult-line-number ((,class (:foreground ,kanagawa-gray-1))))

   ;; Marginalia
   `(marginalia-key ((,class (:foreground ,kanagawa-blue-0))))
   `(marginalia-documentation ((,class (:foreground ,kanagawa-gray-0 :style italic))))

   ;; Orderless
   `(orderless-match-face-0 ((,class (:foreground ,kanagawa-blue-0 :weight bold))))
   `(orderless-match-face-1 ((,class (:foreground ,kanagawa-orange-0 :weight bold))))
   `(orderless-match-face-2 ((,class (:foreground ,kanagawa-green-0 :weight bold))))
   `(orderless-match-face-3 ((,class (:foreground ,kanagawa-purple-0 :weight bold))))

   ;; Dashboard
   `(dashboard-heading ((,class (:foreground ,kanagawa-blue-0 :weight bold))))
   `(dashboard-text-banner ((,class (:foreground ,kanagawa-purple-0 :weight bold))))
   `(dashboard-banner-logo-title ((,class (:foreground ,kanagawa-blue-0 :weight bold))))

   ;; Doom modeline
   `(doom-modeline-bar ((,class (:background ,kanagawa-blue-0))))
   `(doom-modeline-project-dir ((,class (:foreground ,kanagawa-cyan-0 :weight bold))))
   `(doom-modeline-buffer-path ((,class (:foreground ,kanagawa-green-0 :weight bold))))
   `(doom-modeline-buffer-file ((,class (:foreground ,kanagawa-fg-0 :weight bold))))
   `(doom-modeline-buffer-modified ((,class (:foreground ,kanagawa-yellow-0 :weight bold))))
   `(doom-modeline-buffer-major-mode ((,class (:foreground ,kanagawa-green-0 :weight bold))))
   `(doom-modeline-info ((,class (:foreground ,kanagawa-cyan-0 :weight bold))))
   `(doom-modeline-warning ((,class (:foreground ,kanagawa-orange-1 :weight bold))))
   `(doom-modeline-urgent ((,class (:foreground ,kanagawa-red-1 :weight bold))))

   ;; LSP and Tree-sitter
   `(lsp-face-highlight-textual ((,class (:background ,kanagawa-bg-3))))
   `(lsp-face-highlight-read ((,class (:background ,kanagawa-bg-3))))
   `(lsp-face-highlight-write ((,class (:background ,kanagawa-bg-3))))

   ;; Treesitter faces
   `(tree-sitter-hl-face:function ((,class (:foreground ,kanagawa-blue-0))))
   `(tree-sitter-hl-face:function.call ((,class (:foreground ,kanagawa-blue-0))))
   `(tree-sitter-hl-face:method ((,class (:foreground ,kanagawa-blue-0))))
   `(tree-sitter-hl-face:method.call ((,class (:foreground ,kanagawa-blue-0))))
   `(tree-sitter-hl-face:type ((,class (:foreground ,kanagawa-yellow-0))))
   `(tree-sitter-hl-face:type.builtin ((,class (:foreground ,kanagawa-yellow-0))))
   `(tree-sitter-hl-face:constructor ((,class (:foreground ,kanagawa-yellow-0))))
   `(tree-sitter-hl-face:variable ((,class (:foreground ,kanagawa-fg-1))))
   `(tree-sitter-hl-face:variable.builtin ((,class (:foreground ,kanagawa-fg-1))))
   `(tree-sitter-hl-face:property ((,class (:foreground ,kanagawa-fg-1))))
   `(tree-sitter-hl-face:operator ((,class (:foreground ,kanagawa-cyan-1))))
   `(tree-sitter-hl-face:keyword ((,class (:foreground ,kanagawa-purple-0))))
   `(tree-sitter-hl-face:string ((,class (:foreground ,kanagawa-green-0))))
   `(tree-sitter-hl-face:number ((,class (:foreground ,kanagawa-orange-0))))
   `(tree-sitter-hl-face:boolean ((,class (:foreground ,kanagawa-orange-0))))
   `(tree-sitter-hl-face:comment ((,class (:foreground ,kanagawa-gray-0 :style italic))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'kanagawa)

;;; kanagawa-theme.el ends here
