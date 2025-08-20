;;; kanagawa-theme.el --- A theme inspired by Katsushika Hokusai's "The Great Wave off Kanagawa"

;; Copyright (C) 2025

;; Author: Andres Rivero
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces, theme
;; URL: 

;;; Commentary:

;; A dark theme inspired by the famous woodblock print "The Great Wave off Kanagawa"
;; by Katsushika Hokusai. Features deep blues, soft whites, and earthy accents
;; that capture the essence of the iconic artwork.

;;; Code:

(deftheme kanagawa
  "A theme inspired by The Great Wave off Kanagawa.")

(let ((class '((class color) (min-colors 89)))
      ;; Color palette inspired by the Great Wave
      (kanagawa-bg       "#1F1F28")  ; Deep night blue-black
      (kanagawa-bg-alt   "#16161D")  ; Darker background
      (kanagawa-fg       "#DCD7BA")  ; Soft off-white (foam)
      (kanagawa-fg-alt   "#C8C093")  ; Muted foreground
      
      ;; Blues from the wave
      (kanagawa-blue     "#7E9CD8")  ; Main wave blue
      (kanagawa-blue-1   "#2D4F67")  ; Deep wave blue
      (kanagawa-blue-2   "#223249")  ; Darker wave
      (kanagawa-blue-3   "#658594")  ; Medium wave blue
      (kanagawa-cyan     "#7FB4CA")  ; Wave foam
      
      ;; Earth tones from Mount Fuji and boats
      (kanagawa-brown    "#C34043")  ; Mount Fuji red
      (kanagawa-orange   "#FFA066")  ; Warm sunset
      (kanagawa-yellow   "#E6C384")  ; Golden highlights
      (kanagawa-beige    "#D5CEA3")  ; Sand/wood tones
      
      ;; Accent colors
      (kanagawa-green    "#76946A")  ; Muted sea green
      (kanagawa-purple   "#957FB8")  ; Soft purple
      (kanagawa-red      "#E82424")  ; Alert red
      (kanagawa-gray     "#54546D")  ; Comments
      (kanagawa-gray-1   "#363646")  ; Subtle backgrounds
      (kanagawa-gray-2   "#727169")  ; Inactive elements
      
      ;; Special colors
      (kanagawa-white    "#F7F7F7")  ; Pure foam white
      (kanagawa-black    "#0D0C0C")) ; Deep black

  (custom-theme-set-faces
   'kanagawa

   ;; Basic faces
   `(default ((,class (:background ,kanagawa-bg :foreground ,kanagawa-fg))))
   `(cursor ((,class (:background ,kanagawa-cyan))))
   `(region ((,class (:background ,kanagawa-blue-2))))
   `(highlight ((,class (:background ,kanagawa-gray-1))))
   `(secondary-selection ((,class (:background ,kanagawa-blue-1))))
   `(lazy-highlight ((,class (:background ,kanagawa-blue-3 :foreground ,kanagawa-white))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,kanagawa-blue))))
   `(font-lock-comment-face ((,class (:foreground ,kanagawa-gray :style italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,kanagawa-gray))))
   `(font-lock-constant-face ((,class (:foreground ,kanagawa-orange))))
   `(font-lock-doc-face ((,class (:foreground ,kanagawa-gray-2 :style italic))))
   `(font-lock-function-name-face ((,class (:foreground ,kanagawa-blue :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,kanagawa-purple :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,kanagawa-red))))
   `(font-lock-preprocessor-face ((,class (:foreground ,kanagawa-cyan))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,kanagawa-yellow))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,kanagawa-purple))))
   `(font-lock-string-face ((,class (:foreground ,kanagawa-green))))
   `(font-lock-type-face ((,class (:foreground ,kanagawa-yellow))))
   `(font-lock-variable-name-face ((,class (:foreground ,kanagawa-fg-alt))))
   `(font-lock-warning-face ((,class (:foreground ,kanagawa-red :weight bold))))

   ;; UI elements
   `(fringe ((,class (:background ,kanagawa-bg-alt))))
   `(header-line ((,class (:background ,kanagawa-gray-1 :foreground ,kanagawa-fg))))
   `(line-number ((,class (:background ,kanagawa-bg-alt :foreground ,kanagawa-gray))))
   `(line-number-current-line ((,class (:background ,kanagawa-gray-1 :foreground ,kanagawa-cyan))))
   `(minibuffer-prompt ((,class (:foreground ,kanagawa-blue :weight bold))))
   `(mode-line ((,class (:background ,kanagawa-gray-1 :foreground ,kanagawa-fg :box (:line-width 1 :color ,kanagawa-gray)))))
   `(mode-line-inactive ((,class (:background ,kanagawa-bg-alt :foreground ,kanagawa-gray-2))))
   `(vertical-border ((,class (:foreground ,kanagawa-gray-1))))

   ;; Search and matching
   `(isearch ((,class (:background ,kanagawa-yellow :foreground ,kanagawa-black :weight bold))))
   `(isearch-fail ((,class (:background ,kanagawa-red :foreground ,kanagawa-white))))
   `(match ((,class (:background ,kanagawa-blue-3 :foreground ,kanagawa-white))))

   ;; Links
   `(link ((,class (:foreground ,kanagawa-cyan :underline t))))
   `(link-visited ((,class (:foreground ,kanagawa-purple :underline t))))

   ;; Parentheses matching
   `(show-paren-match ((,class (:background ,kanagawa-blue-1 :foreground ,kanagawa-white :weight bold))))
   `(show-paren-mismatch ((,class (:background ,kanagawa-red :foreground ,kanagawa-white :weight bold))))

   ;; Dired
   `(dired-directory ((,class (:foreground ,kanagawa-blue :weight bold))))
   `(dired-flagged ((,class (:foreground ,kanagawa-red))))
   `(dired-header ((,class (:foreground ,kanagawa-cyan :weight bold))))
   `(dired-ignored ((,class (:foreground ,kanagawa-gray))))
   `(dired-mark ((,class (:foreground ,kanagawa-yellow))))
   `(dired-marked ((,class (:foreground ,kanagawa-yellow :weight bold))))
   `(dired-perm-write ((,class (:foreground ,kanagawa-red))))
   `(dired-symlink ((,class (:foreground ,kanagawa-cyan))))
   `(dired-warning ((,class (:foreground ,kanagawa-red :weight bold))))

   ;; Org mode
   `(org-block ((,class (:background ,kanagawa-bg-alt))))
   `(org-block-begin-line ((,class (:foreground ,kanagawa-gray :style italic))))
   `(org-block-end-line ((,class (:foreground ,kanagawa-gray :style italic))))
   `(org-code ((,class (:foreground ,kanagawa-orange))))
   `(org-date ((,class (:foreground ,kanagawa-blue :underline t))))
   `(org-done ((,class (:foreground ,kanagawa-green :weight bold))))
   `(org-headline-done ((,class (:foreground ,kanagawa-gray-2))))
   `(org-hide ((,class (:foreground ,kanagawa-bg))))
   `(org-level-1 ((,class (:foreground ,kanagawa-blue :weight bold :height 1.3))))
   `(org-level-2 ((,class (:foreground ,kanagawa-cyan :weight bold :height 1.2))))
   `(org-level-3 ((,class (:foreground ,kanagawa-purple :weight bold :height 1.1))))
   `(org-level-4 ((,class (:foreground ,kanagawa-yellow :weight bold))))
   `(org-level-5 ((,class (:foreground ,kanagawa-green :weight bold))))
   `(org-level-6 ((,class (:foreground ,kanagawa-orange :weight bold))))
   `(org-link ((,class (:foreground ,kanagawa-cyan :underline t))))
   `(org-special-keyword ((,class (:foreground ,kanagawa-gray))))
   `(org-table ((,class (:foreground ,kanagawa-beige))))
   `(org-tag ((,class (:foreground ,kanagawa-orange :weight bold))))
   `(org-todo ((,class (:foreground ,kanagawa-red :weight bold))))

   ;; Company (completion)
   `(company-echo-common ((,class (:foreground ,kanagawa-red))))
   `(company-preview ((,class (:background ,kanagawa-gray-1 :foreground ,kanagawa-fg-alt))))
   `(company-preview-common ((,class (:background ,kanagawa-gray-1 :foreground ,kanagawa-blue))))
   `(company-preview-search ((,class (:background ,kanagawa-blue :foreground ,kanagawa-white))))
   `(company-scrollbar-bg ((,class (:background ,kanagawa-gray-1))))
   `(company-scrollbar-fg ((,class (:background ,kanagawa-gray))))
   `(company-tooltip ((,class (:background ,kanagawa-gray-1 :foreground ,kanagawa-fg))))
   `(company-tooltip-annotation ((,class (:foreground ,kanagawa-gray-2))))
   `(company-tooltip-common ((,class (:foreground ,kanagawa-blue))))
   `(company-tooltip-common-selection ((,class (:foreground ,kanagawa-white))))
   `(company-tooltip-selection ((,class (:background ,kanagawa-blue-2 :foreground ,kanagawa-white))))

   ;; Magit
   `(magit-section-heading ((,class (:foreground ,kanagawa-cyan :weight bold))))
   `(magit-branch-local ((,class (:foreground ,kanagawa-blue))))
   `(magit-branch-remote ((,class (:foreground ,kanagawa-green))))
   `(magit-diff-added ((,class (:foreground ,kanagawa-green))))
   `(magit-diff-removed ((,class (:foreground ,kanagawa-red))))
   `(magit-diff-context ((,class (:foreground ,kanagawa-fg-alt))))
   `(magit-hash ((,class (:foreground ,kanagawa-orange))))
   `(magit-log-author ((,class (:foreground ,kanagawa-purple))))
   `(magit-log-date ((,class (:foreground ,kanagawa-gray-2))))

   ;; Error and warning faces
   `(error ((,class (:foreground ,kanagawa-red :weight bold))))
   `(warning ((,class (:foreground ,kanagawa-yellow :weight bold))))
   `(success ((,class (:foreground ,kanagawa-green :weight bold))))

   ;; Flycheck
   `(flycheck-error ((,class (:underline (:style wave :color ,kanagawa-red)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,kanagawa-yellow)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,kanagawa-blue)))))

   ;; Helm
   `(helm-header ((,class (:background ,kanagawa-gray-1 :foreground ,kanagawa-white))))
   `(helm-source-header ((,class (:background ,kanagawa-blue :foreground ,kanagawa-white :weight bold))))
   `(helm-selection ((,class (:background ,kanagawa-blue-2))))
   `(helm-match ((,class (:foreground ,kanagawa-yellow :weight bold))))

   ;; Which-key
   `(which-key-key-face ((,class (:foreground ,kanagawa-blue :weight bold))))
   `(which-key-description-face ((,class (:foreground ,kanagawa-fg))))
   `(which-key-group-description-face ((,class (:foreground ,kanagawa-purple))))
   `(which-key-command-description-face ((,class (:foreground ,kanagawa-cyan))))

   ;; Treemacs
   `(treemacs-directory-face ((,class (:foreground ,kanagawa-blue))))
   `(treemacs-file-face ((,class (:foreground ,kanagawa-fg))))
   `(treemacs-root-face ((,class (:foreground ,kanagawa-cyan :weight bold))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'kanagawa)

;;; kanagawa-theme.el ends here
