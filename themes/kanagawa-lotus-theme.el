;;; kanagawa-lotus-theme.el --- Kanagawa Lotus light theme for Emacs

;; Copyright (C) 2025

;; Author: and19riv
;; Version: 2.0.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces, theme, light, kanagawa, lotus
;; URL: 

;;; Commentary:

;; A light theme inspired by the famous woodblock print "The Great Wave off Kanagawa"
;; by Katsushika Hokusai. The Lotus variant captures the serenity of lotus flowers
;; floating on calm waters, with warm earth tones and soft contrasts perfect for
;; daytime coding sessions.
;;
;; This theme provides comprehensive syntax highlighting and extensive plugin support
;; while maintaining the peaceful aesthetic of the Kanagawa Lotus color scheme.

;;; Code:

(deftheme kanagawa-lotus
  "Kanagawa Lotus - A light theme inspired by lotus flowers and serene waters.")

(let ((class '((class color) (min-colors 89)))
      ;; Lotus color palette - Light theme colors
      (lotus-bg-0        "#F2ECBC")  ; Main background (lighter, less opaque)
      (lotus-bg-1        "#E5DDD5")  ; Slightly darker background
      (lotus-bg-2        "#DDD8BB")  ; Darker background elements
      (lotus-bg-3        "#D5CEA3")  ; Subtle highlights (was main bg)
      (lotus-bg-4        "#E7DBA0")  ; Selection background
      (lotus-bg-5        "#D5CEA3")  ; Active line background (cursor line)
      
      ;; Foreground colors
      (lotus-fg-0        "#545464")  ; Main foreground (dark purple-gray)
      (lotus-fg-1        "#43436C")  ; Darker foreground (headers, emphasis)
      (lotus-gray-0      "#716E61")  ; Comments (warm gray)
      (lotus-gray-1      "#8A8980")  ; Muted foreground
      (lotus-gray-2      "#9E9B93")  ; Line numbers, subtle elements
      
      ;; Core colors
      (lotus-red-0       "#C84053")  ; Primary red
      (lotus-red-1       "#D7474B")  ; Brighter red
      (lotus-red-2       "#E61F44")  ; Error red
      
      (lotus-orange-0    "#CC6D00")  ; Primary orange
      (lotus-orange-1    "#E98A00")  ; Warning orange
      
      (lotus-yellow-0    "#836F4A")  ; Muted yellow-brown
      (lotus-yellow-1    "#CC9500")  ; Brighter yellow
      (lotus-yellow-2    "#F2BF26")  ; Bright yellow
      (lotus-yellow-3    "#E5B013")  ; Golden yellow
      
      (lotus-green-0     "#6F894E")  ; Primary green
      (lotus-green-1     "#6E915F")  ; String green
      (lotus-green-2     "#87A987")  ; Light green
      
      (lotus-teal-0      "#4C9A91")  ; Primary teal
      (lotus-teal-1      "#5A9C8B")  ; Function teal
      (lotus-teal-2      "#5E9F89")  ; Light teal
      
      (lotus-aqua-0      "#597B75")  ; Primary aqua
      (lotus-aqua-1      "#5E857A")  ; Macro aqua
      
      (lotus-blue-0      "#4D699B")  ; Primary blue
      (lotus-blue-1      "#5D57A3")  ; Keyword blue
      (lotus-blue-2      "#6693BF")  ; Cursor blue
      (lotus-blue-3      "#5A7785")  ; Muted blue
      
      (lotus-violet-0    "#8E7DC6")  ; Primary violet
      (lotus-violet-1    "#9C86BF")  ; Light violet
      (lotus-violet-2    "#A09DBC")  ; Muted violet
      (lotus-violet-3    "#766B90")  ; Dark violet
      
      (lotus-pink-0      "#B35B79")  ; Primary pink
      
      ;; Special UI colors
      (lotus-cursor      "#6693BF")  ; Cursor color
      (lotus-selection   "#C8D4E0")  ; Text selection
      (lotus-search      "#F2BF26")  ; Search highlight
      (lotus-match       "#5A7785")  ; Matching elements
      
      ;; Diff colors
      (lotus-diff-add    "#6F894E")  ; Added lines
      (lotus-diff-change "#CC9500")  ; Changed lines  
      (lotus-diff-delete "#C84053")  ; Deleted lines
      
      ;; Border and UI elements
      (lotus-border      "#8A8980")  ; Borders and separators
      (lotus-shadow      "#9E9B93")) ; Shadows and inactive elements

  (custom-theme-set-faces
   'kanagawa-lotus

   ;; Basic faces
   `(default ((,class (:background ,lotus-bg-0 :foreground ,lotus-fg-0))))
   `(cursor ((,class (:background ,lotus-cursor))))
   `(region ((,class (:background ,lotus-selection))))
   `(highlight ((,class (:background ,lotus-bg-5))))
   `(secondary-selection ((,class (:background ,lotus-bg-3))))
   `(lazy-highlight ((,class (:background ,lotus-blue-3 :foreground ,lotus-bg-0))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,lotus-blue-0))))
   `(font-lock-comment-face ((,class (:foreground ,lotus-gray-0 :style italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,lotus-gray-0))))
   `(font-lock-constant-face ((,class (:foreground ,lotus-orange-0))))
   `(font-lock-doc-face ((,class (:foreground ,lotus-gray-2 :style italic))))
   `(font-lock-function-name-face ((,class (:foreground ,lotus-teal-1 :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,lotus-violet-0 :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,lotus-red-0))))
   `(font-lock-preprocessor-face ((,class (:foreground ,lotus-aqua-1))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,lotus-orange-0))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,lotus-violet-0))))
   `(font-lock-string-face ((,class (:foreground ,lotus-green-1))))
   `(font-lock-type-face ((,class (:foreground ,lotus-yellow-0))))
   `(font-lock-variable-name-face ((,class (:foreground ,lotus-fg-1))))
   `(font-lock-warning-face ((,class (:foreground ,lotus-orange-1 :weight bold))))

   ;; UI elements
   `(fringe ((,class (:background ,lotus-bg-1))))
   `(header-line ((,class (:background ,lotus-bg-5 :foreground ,lotus-fg-0))))
   `(line-number ((,class (:background ,lotus-bg-1 :foreground ,lotus-gray-1))))
   `(line-number-current-line ((,class (:background ,lotus-bg-5 :foreground ,lotus-cursor))))
   `(minibuffer-prompt ((,class (:foreground ,lotus-blue-0 :weight bold))))
   `(mode-line ((,class (:background ,lotus-bg-2 :foreground ,lotus-fg-0 :box (:line-width 1 :color ,lotus-border)))))
   `(mode-line-inactive ((,class (:background ,lotus-bg-1 :foreground ,lotus-gray-1))))
   `(vertical-border ((,class (:foreground ,lotus-border))))

   ;; Search and matching
   `(isearch ((,class (:background ,lotus-search :foreground ,lotus-fg-0 :weight bold))))
   `(isearch-fail ((,class (:background ,lotus-red-0 :foreground ,lotus-bg-0))))
   `(match ((,class (:background ,lotus-blue-3 :foreground ,lotus-bg-0))))

   ;; Links
   `(link ((,class (:foreground ,lotus-blue-0 :underline t))))
   `(link-visited ((,class (:foreground ,lotus-violet-0 :underline t))))

   ;; Parentheses matching
   `(show-paren-match ((,class (:background ,lotus-bg-4 :foreground ,lotus-fg-1 :weight bold))))
   `(show-paren-mismatch ((,class (:background ,lotus-red-0 :foreground ,lotus-bg-0 :weight bold))))

   ;; Dired
   `(dired-directory ((,class (:foreground ,lotus-blue-0 :weight bold))))
   `(dired-flagged ((,class (:foreground ,lotus-red-0))))
   `(dired-header ((,class (:foreground ,lotus-teal-0 :weight bold))))
   `(dired-ignored ((,class (:foreground ,lotus-gray-1))))
   `(dired-mark ((,class (:foreground ,lotus-orange-0))))
   `(dired-marked ((,class (:foreground ,lotus-orange-0 :weight bold))))
   `(dired-perm-write ((,class (:foreground ,lotus-red-0))))
   `(dired-symlink ((,class (:foreground ,lotus-teal-0))))
   `(dired-warning ((,class (:foreground ,lotus-orange-1 :weight bold))))

   ;; Org mode
   `(org-block ((,class (:background ,lotus-bg-1))))
   `(org-block-begin-line ((,class (:foreground ,lotus-gray-0 :style italic))))
   `(org-block-end-line ((,class (:foreground ,lotus-gray-0 :style italic))))
   `(org-code ((,class (:foreground ,lotus-orange-0))))
   `(org-date ((,class (:foreground ,lotus-blue-0 :underline t))))
   `(org-done ((,class (:foreground ,lotus-green-0 :weight bold))))
   `(org-headline-done ((,class (:foreground ,lotus-gray-1))))
   `(org-hide ((,class (:foreground ,lotus-bg-0))))
   `(org-level-1 ((,class (:foreground ,lotus-blue-0 :weight bold :height 1.3))))
   `(org-level-2 ((,class (:foreground ,lotus-teal-0 :weight bold :height 1.2))))
   `(org-level-3 ((,class (:foreground ,lotus-violet-0 :weight bold :height 1.1))))
   `(org-level-4 ((,class (:foreground ,lotus-orange-0 :weight bold))))
   `(org-level-5 ((,class (:foreground ,lotus-green-0 :weight bold))))
   `(org-level-6 ((,class (:foreground ,lotus-yellow-0 :weight bold))))
   `(org-link ((,class (:foreground ,lotus-blue-0 :underline t))))
   `(org-special-keyword ((,class (:foreground ,lotus-gray-0))))
   `(org-table ((,class (:foreground ,lotus-fg-1))))
   `(org-tag ((,class (:foreground ,lotus-orange-0 :weight bold))))
   `(org-todo ((,class (:foreground ,lotus-red-0 :weight bold))))

   ;; Company (completion)
   `(company-echo-common ((,class (:foreground ,lotus-red-0))))
   `(company-preview ((,class (:background ,lotus-bg-5 :foreground ,lotus-fg-1))))
   `(company-preview-common ((,class (:background ,lotus-bg-5 :foreground ,lotus-blue-0))))
   `(company-preview-search ((,class (:background ,lotus-blue-0 :foreground ,lotus-bg-0))))
   `(company-scrollbar-bg ((,class (:background ,lotus-bg-5))))
   `(company-scrollbar-fg ((,class (:background ,lotus-border))))
   `(company-tooltip ((,class (:background ,lotus-bg-5 :foreground ,lotus-fg-0))))
   `(company-tooltip-annotation ((,class (:foreground ,lotus-gray-1))))
   `(company-tooltip-common ((,class (:foreground ,lotus-blue-0))))
   `(company-tooltip-common-selection ((,class (:foreground ,lotus-bg-0))))
   `(company-tooltip-selection ((,class (:background ,lotus-selection :foreground ,lotus-fg-0))))

   ;; Magit
   `(magit-section-heading ((,class (:foreground ,lotus-teal-0 :weight bold))))
   `(magit-branch-local ((,class (:foreground ,lotus-blue-0))))
   `(magit-branch-remote ((,class (:foreground ,lotus-green-0))))
   `(magit-diff-added ((,class (:foreground "#1B5E20" :background "#C8E6C9" :weight bold))))
   `(magit-diff-removed ((,class (:foreground "#B71C1C" :background "#FFCDD2" :weight bold))))
   `(magit-diff-added-highlight ((,class (:background "#E8F5E8" :foreground "#2E7D32" :weight bold))))
   `(magit-diff-removed-highlight ((,class (:background "#FFEBEE" :foreground "#C62828" :weight bold))))
   `(magit-diff-context ((,class (:foreground ,lotus-fg-1))))
   `(magit-diff-base-highlight ((,class (:background "#E8D4B8" :foreground "#7B5B16"))))
   `(magit-diff-context-highlight ((,class (:background "#E8E8D8"))))
   `(magit-hash ((,class (:foreground ,lotus-orange-0))))
   `(magit-log-author ((,class (:foreground ,lotus-violet-0))))
   `(magit-log-date ((,class (:foreground ,lotus-gray-1))))

   `(smerge-refined-added ((,class (:background "#E0F2E0" :foreground "#1B5E20" :weight bold))))
   `(smerge-refined-removed ((,class (:background "#FFE0E0" :foreground "#B71C1C" :weight bold))))

   ;; Error and warning faces
   `(error ((,class (:foreground ,lotus-red-0 :weight bold))))
   `(warning ((,class (:foreground ,lotus-orange-1 :weight bold))))
   `(success ((,class (:foreground ,lotus-green-0 :weight bold))))

   ;; Flycheck
   `(flycheck-error ((,class (:underline (:style wave :color ,lotus-red-0)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,lotus-orange-1)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,lotus-blue-0)))))

   ;; Helm
   `(helm-header ((,class (:background ,lotus-bg-5 :foreground ,lotus-fg-0))))
   `(helm-source-header ((,class (:background ,lotus-blue-0 :foreground ,lotus-bg-0 :weight bold))))
   `(helm-selection ((,class (:background ,lotus-selection))))
   `(helm-match ((,class (:foreground ,lotus-orange-0 :weight bold))))

   ;; Which-key
   `(which-key-key-face ((,class (:foreground ,lotus-blue-0 :weight bold))))
   `(which-key-description-face ((,class (:foreground ,lotus-fg-0))))
   `(which-key-group-description-face ((,class (:foreground ,lotus-violet-0))))
   `(which-key-command-description-face ((,class (:foreground ,lotus-teal-0))))

   ;; Treemacs
   `(treemacs-directory-face ((,class (:foreground ,lotus-blue-0))))
   `(treemacs-file-face ((,class (:foreground ,lotus-fg-0))))
   `(treemacs-root-face ((,class (:foreground ,lotus-teal-0 :weight bold))))

   ;; Centaur Tabs (Doom Emacs compatibility)
   `(centaur-tabs-default ((,class (:background ,lotus-bg-1 :foreground ,lotus-gray-1))))
   `(centaur-tabs-selected ((,class (:background ,lotus-bg-0 :foreground ,lotus-fg-0 :weight bold))))
   `(centaur-tabs-unselected ((,class (:background ,lotus-bg-2 :foreground ,lotus-gray-0))))
   `(centaur-tabs-selected-modified ((,class (:background ,lotus-bg-0 :foreground ,lotus-orange-0 :weight bold))))
   `(centaur-tabs-unselected-modified ((,class (:background ,lotus-bg-2 :foreground ,lotus-orange-1))))
   `(centaur-tabs-active-bar-face ((,class (:background ,lotus-blue-0))))
   `(centaur-tabs-modified-marker-selected ((,class (:inherit centaur-tabs-selected-modified :foreground ,lotus-orange-0))))
   `(centaur-tabs-modified-marker-unselected ((,class (:inherit centaur-tabs-unselected-modified :foreground ,lotus-orange-1))))
   
   ;; Additional centaur-tabs faces
   `(centaur-tabs-active-tab ((,class (:background ,lotus-bg-0 :foreground ,lotus-fg-0 :weight bold))))
   `(centaur-tabs-inactive-tab ((,class (:background ,lotus-bg-2 :foreground ,lotus-gray-0))))
   `(centaur-tabs-selected-tab ((,class (:background ,lotus-green-0 :foreground ,lotus-bg-0 :weight bold))))

   ;; Doom-specific workspace tabs
   `(+workspace-tab-selected-face ((,class (:background ,lotus-green-0 :foreground ,lotus-bg-0 :weight bold))))
   `(+workspace-tab-face ((,class (:background ,lotus-bg-2 :foreground ,lotus-gray-0))))

   ;; Tab Bar (built-in Emacs tabs)
   `(tab-bar ((,class (:background ,lotus-bg-1 :foreground ,lotus-fg-0))))
   `(tab-bar-tab ((,class (:background ,lotus-bg-0 :foreground ,lotus-fg-0 :weight bold))))
   `(tab-bar-tab-inactive ((,class (:background ,lotus-bg-2 :foreground ,lotus-gray-0))))

   ;; Tab Line (built-in Emacs tab line)
   `(tab-line ((,class (:background ,lotus-bg-1 :foreground ,lotus-fg-0))))
   `(tab-line-tab ((,class (:background ,lotus-bg-0 :foreground ,lotus-fg-0 :weight bold))))
   `(tab-line-tab-inactive ((,class (:background ,lotus-bg-2 :foreground ,lotus-gray-0))))
   `(tab-line-tab-current ((,class (:background ,lotus-bg-0 :foreground ,lotus-fg-0 :weight bold))))
   `(tab-line-close-highlight ((,class (:foreground ,lotus-red-0))))

   ;; Term colors (for terminal emulation)
   `(term-color-black ((,class (:background ,lotus-bg-2 :foreground ,lotus-bg-2))))
   `(term-color-red ((,class (:background ,lotus-red-0 :foreground ,lotus-red-0))))
   `(term-color-green ((,class (:background ,lotus-green-0 :foreground ,lotus-green-0))))
   `(term-color-yellow ((,class (:background ,lotus-yellow-1 :foreground ,lotus-yellow-1))))
   `(term-color-blue ((,class (:background ,lotus-blue-0 :foreground ,lotus-blue-0))))
   `(term-color-magenta ((,class (:background ,lotus-violet-0 :foreground ,lotus-violet-0))))
   `(term-color-cyan ((,class (:background ,lotus-teal-0 :foreground ,lotus-teal-0))))
   `(term-color-white ((,class (:background ,lotus-fg-0 :foreground ,lotus-fg-0))))

   ;; Diff mode
   `(diff-added ((,class (:background "#C8E6C9" :foreground "#1B5E20" :weight bold))))
   `(diff-removed ((,class (:background "#FFCDD2" :foreground "#B71C1C" :weight bold))))
   `(diff-changed ((,class (:background "#F0F0E8" :foreground ,lotus-diff-change))))
   `(diff-header ((,class (:background ,lotus-bg-5 :foreground ,lotus-fg-1))))
   `(diff-file-header ((,class (:background ,lotus-bg-4 :foreground ,lotus-fg-0 :weight bold))))
   `(diff-hunk-header ((,class (:background ,lotus-bg-2 :foreground ,lotus-teal-0))))

   ;; Ediff
   `(ediff-current-diff-A ((,class (:background "#F0E8E8"))))
   `(ediff-current-diff-B ((,class (:background "#E8F0E8"))))
   `(ediff-current-diff-C ((,class (:background "#F0F0E8"))))
   `(ediff-fine-diff-A ((,class (:background ,lotus-red-0 :foreground ,lotus-bg-0))))
   `(ediff-fine-diff-B ((,class (:background ,lotus-green-0 :foreground ,lotus-bg-0))))
   `(ediff-fine-diff-C ((,class (:background ,lotus-yellow-1 :foreground ,lotus-bg-0))))

   ;; Eshell
   `(eshell-prompt ((,class (:foreground ,lotus-blue-0 :weight bold))))
   `(eshell-ls-directory ((,class (:foreground ,lotus-blue-0 :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,lotus-green-0))))
   `(eshell-ls-symlink ((,class (:foreground ,lotus-teal-0))))

   ;; Ivy/Counsel
   `(ivy-current-match ((,class (:background ,lotus-selection :foreground ,lotus-fg-0))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,lotus-orange-0))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,lotus-blue-0 :weight bold))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,lotus-violet-0 :weight bold))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,lotus-teal-0 :weight bold))))

   ;; Vertico/Consult (modern completion)
   `(vertico-current ((,class (:background ,lotus-selection))))
   `(consult-file ((,class (:foreground ,lotus-fg-0))))
   `(consult-line-number ((,class (:foreground ,lotus-gray-1))))

   ;; Marginalia
   `(marginalia-key ((,class (:foreground ,lotus-blue-0))))
   `(marginalia-documentation ((,class (:foreground ,lotus-gray-0 :style italic))))

   ;; Orderless
   `(orderless-match-face-0 ((,class (:foreground ,lotus-blue-0 :weight bold))))
   `(orderless-match-face-1 ((,class (:foreground ,lotus-orange-0 :weight bold))))
   `(orderless-match-face-2 ((,class (:foreground ,lotus-green-0 :weight bold))))
   `(orderless-match-face-3 ((,class (:foreground ,lotus-violet-0 :weight bold))))

   ;; Dashboard
   `(dashboard-heading ((,class (:foreground ,lotus-blue-0 :weight bold))))
   `(dashboard-text-banner ((,class (:foreground ,lotus-violet-0 :weight bold))))
   `(dashboard-banner-logo-title ((,class (:foreground ,lotus-blue-0 :weight bold))))

   ;; Doom modeline
   `(doom-modeline-bar ((,class (:background ,lotus-blue-0))))
   `(doom-modeline-project-dir ((,class (:foreground ,lotus-blue-0 :weight bold))))
   `(doom-modeline-buffer-path ((,class (:foreground ,lotus-fg-0 :weight bold))))
   `(doom-modeline-buffer-file ((,class (:foreground ,lotus-fg-0 :weight bold))))
   `(doom-modeline-buffer-modified ((,class (:foreground ,lotus-orange-0 :weight bold))))
   `(doom-modeline-buffer-major-mode ((,class (:foreground ,lotus-blue-0 :weight bold))))
   `(doom-modeline-info ((,class (:foreground ,lotus-green-0 :weight bold))))
   `(doom-modeline-warning ((,class (:foreground ,lotus-orange-1 :weight bold))))
   `(doom-modeline-urgent ((,class (:foreground ,lotus-red-0 :weight bold))))

   ;; LSP and Tree-sitter
   `(lsp-face-highlight-textual ((,class (:background ,lotus-bg-5))))
   `(lsp-face-highlight-read ((,class (:background ,lotus-bg-5))))
   `(lsp-face-highlight-write ((,class (:background ,lotus-bg-5))))

   ;; Treesitter faces
   `(tree-sitter-hl-face:function ((,class (:foreground ,lotus-teal-1))))
   `(tree-sitter-hl-face:function.call ((,class (:foreground ,lotus-teal-1))))
   `(tree-sitter-hl-face:method ((,class (:foreground ,lotus-teal-1))))
   `(tree-sitter-hl-face:method.call ((,class (:foreground ,lotus-teal-1))))
   `(tree-sitter-hl-face:type ((,class (:foreground ,lotus-yellow-0))))
   `(tree-sitter-hl-face:type.builtin ((,class (:foreground ,lotus-yellow-0))))
   `(tree-sitter-hl-face:constructor ((,class (:foreground ,lotus-yellow-0))))
   `(tree-sitter-hl-face:variable ((,class (:foreground ,lotus-fg-1))))
   `(tree-sitter-hl-face:variable.builtin ((,class (:foreground ,lotus-fg-1))))
   `(tree-sitter-hl-face:property ((,class (:foreground ,lotus-fg-1))))
   `(tree-sitter-hl-face:operator ((,class (:foreground ,lotus-aqua-0))))
   `(tree-sitter-hl-face:keyword ((,class (:foreground ,lotus-violet-0))))
   `(tree-sitter-hl-face:string ((,class (:foreground ,lotus-green-1))))
   `(tree-sitter-hl-face:number ((,class (:foreground ,lotus-orange-0))))
   `(tree-sitter-hl-face:boolean ((,class (:foreground ,lotus-orange-0))))
   `(tree-sitter-hl-face:comment ((,class (:foreground ,lotus-gray-0 :style italic))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'kanagawa-lotus)

;;; kanagawa-lotus-theme.el ends here
