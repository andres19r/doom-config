;;; kanso-pearl-theme.el --- A light theme inspired by Kanso Pearl -*- lexical-binding: t -*-

;; Author: Andres Rivero
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: faces, themes
;; URL: https://github.com/yourusername/kanso-pearl-theme

;;; Commentary:
;; A light theme for Emacs based on the Kanso Pearl colorscheme.
;; Ported from https://github.com/webhooked/kanso.nvim

;;; Code:

(deftheme kanso-pearl
  "A light theme inspired by Kanso Pearl colorscheme.")

(let ((class '((class color) (min-colors 89)))
      ;; Kanso Pearl palette - light theme colors
      (pearl-bg0   "#f2f1ef")  ;; pearlWhite0 - main background
      (pearl-bg1   "#e2e1df")  ;; pearlWhite1
      (pearl-bg2   "#dddddb")  ;; pearlWhite2
      (pearl-bg3   "#cacac7")  ;; pearlWhite3
      
      ;; Foreground colors
      (fg          "#22262D")  ;; pearlBlack0
      (fg-dim      "#545464")  ;; pearlBlack1
      (fg2         "#43436c")  ;; pearlBlack2
      
      ;; Grays
      (gray        "#e2e1df")  ;; pearlGray
      (gray2       "#5C6068")  ;; pearlGray2
      (gray3       "#6D6D69")  ;; pearlGray3
      (gray4       "#9F9F99")  ;; pearlGray4
      
      ;; Main colors (pearl non-saturated variants)
      (violet1     "#a09cac")  ;; pearlViolet1
      (violet2     "#766b90")  ;; pearlViolet2
      (violet3     "#c9cbd1")  ;; pearlViolet3
      (violet4     "#624c83")  ;; pearlViolet4
      
      (blue1       "#c7d7e0")  ;; pearlBlue1
      (blue2       "#b5cbd2")  ;; pearlBlue2
      (blue3       "#9fb5c9")  ;; pearlBlue3
      (blue4       "#4d699b")  ;; pearlBlue4
      (blue5       "#5d57a3")  ;; pearlBlue5
      
      (green       "#6f894e")  ;; pearlGreen
      (green2      "#6e915f")  ;; pearlGreen2
      (green3      "#b7d0ae")  ;; pearlGreen3
      
      (pink        "#b35b79")  ;; pearlPink
      (orange      "#cc6d00")  ;; pearlOrange
      (orange2     "#e98a00")  ;; pearlOrange2
      
      (yellow      "#77713f")  ;; pearlYellow
      (yellow2     "#836f4a")  ;; pearlYellow2
      (yellow3     "#de9800")  ;; pearlYellow3
      (yellow4     "#f9d791")  ;; pearlYellow4
      
      (red         "#c84053")  ;; pearlRed
      (red2        "#d7474b")  ;; pearlRed2
      (red3        "#e82424")  ;; pearlRed3
      (red4        "#d9a594")  ;; pearlRed4
      
      (aqua        "#597b75")  ;; pearlAqua
      (aqua2       "#5e857a")  ;; pearlAqua2
      
      (teal1       "#4e8ca2")  ;; pearlTeal1
      (teal2       "#6693bf")  ;; pearlTeal2
      (teal3       "#5a7785")  ;; pearlTeal3
      
      (cyan        "#d7e3d8")  ;; pearlCyan
      
      ;; Diff colors (adjusted for light theme)
      (diff-green  "#b7d0ae")
      (diff-red    "#d9a594")
      (diff-blue   "#b5cbd2")
      (diff-yellow "#f9d791")
      
      ;; Git colors
      (git-green   "#6e915f")
      (git-red     "#d7474b")
      (git-yellow  "#de9800")
      
      ;; Alt colors
      (alt-blue    "#9fb5c9"))

  (custom-theme-set-faces
   'kanso-pearl

   ;; Basic faces
   `(default ((,class (:foreground ,fg :background ,pearl-bg0))))
   `(cursor ((,class (:background ,fg2))))
   `(fringe ((,class (:background ,pearl-bg0 :foreground ,violet1))))
   `(highlight ((,class (:background ,pearl-bg2))))
   `(region ((,class (:background ,pearl-bg2))))
   `(secondary-selection ((,class (:background ,pearl-bg2))))
   `(buffer-menu-buffer ((,class (:foreground ,fg :weight bold))))
   `(minibuffer-prompt ((,class (:foreground ,violet4))))
   `(vertical-border ((,class (:foreground ,pearl-bg2))))
   `(window-divider ((,class (:foreground ,pearl-bg2))))
   `(window-divider-first-pixel ((,class (:foreground ,pearl-bg2))))
   `(window-divider-last-pixel ((,class (:foreground ,pearl-bg2))))

   ;; Font lock faces - Core syntax highlighting
   `(font-lock-builtin-face ((,class (:foreground ,blue4))))
   `(font-lock-comment-face ((,class (:foreground ,gray3))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,gray3))))
   `(font-lock-constant-face ((,class (:foreground ,orange))))
   `(font-lock-doc-face ((,class (:foreground ,gray3))))
   `(font-lock-function-name-face ((,class (:foreground ,blue4))))
   `(font-lock-keyword-face ((,class (:foreground ,violet4))))
   `(font-lock-negation-char-face ((,class (:foreground ,violet4))))
   `(font-lock-preprocessor-face ((,class (:foreground ,gray2))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-type-face ((,class (:foreground ,aqua))))
   `(font-lock-variable-name-face ((,class (:foreground ,orange))))
   `(font-lock-warning-face ((,class (:foreground ,orange2 :weight bold))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow2))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,yellow2))))

   ;; Mode line
   `(mode-line ((,class (:background ,pearl-bg1 :foreground ,fg))))
   `(mode-line-inactive ((,class (:background ,pearl-bg0 :foreground ,gray3))))
   `(mode-line-buffer-id ((,class (:weight bold))))
   `(mode-line-emphasis ((,class (:weight bold))))
   `(mode-line-highlight ((,class (:box (:line-width 1 :color ,gray3)))))

   ;; Evil mode state colors (if using evil)
   `(evil-normal-state ((,class (:foreground ,green))))  ; Using pearlGreen
   `(evil-insert-state ((,class (:foreground ,blue4))))
   `(evil-visual-state ((,class (:foreground ,yellow2))))
   `(evil-replace-state ((,class (:foreground ,red))))
   `(evil-operator-state ((,class (:foreground ,aqua))))

   ;; Doom modeline evil states (if using doom-modeline)
   `(doom-modeline-evil-normal-state ((,class (:foreground ,green))))
   `(doom-modeline-evil-insert-state ((,class (:foreground ,blue4))))
   `(doom-modeline-evil-visual-state ((,class (:foreground ,yellow2))))

   ;; Isearch
   `(isearch ((,class (:background ,blue2 :foreground ,fg :weight bold))))
   `(isearch-fail ((,class (:background ,red :foreground ,pearl-bg0))))
   `(lazy-highlight ((,class (:background ,pearl-bg3 :foreground ,fg))))

   ;; Diff mode
   `(diff-added ((,class (:background ,diff-green :foreground ,green))))
   `(diff-changed ((,class (:background ,diff-blue :foreground ,teal1))))
   `(diff-removed ((,class (:background ,diff-red :foreground ,red))))
   `(diff-header ((,class (:background ,diff-yellow :foreground ,yellow))))
   `(diff-file-header ((,class (:background ,pearl-bg2 :foreground ,fg :weight bold))))
   `(diff-hunk-header ((,class (:background ,pearl-bg2 :foreground ,violet4))))

   ;; Compilation
   `(compilation-error ((,class (:foreground ,red3))))
   `(compilation-warning ((,class (:foreground ,orange2))))
   `(compilation-info ((,class (:foreground ,green))))
   `(compilation-mode-line-exit ((,class (:foreground ,green))))
   `(compilation-mode-line-fail ((,class (:foreground ,red3))))
   `(compilation-mode-line-run ((,class (:foreground ,blue4))))

   ;; Org mode
   `(org-level-1 ((,class (:foreground ,violet4 :weight bold))))
   `(org-level-2 ((,class (:foreground ,blue4 :weight bold))))
   `(org-level-3 ((,class (:foreground ,green))))
   `(org-level-4 ((,class (:foreground ,yellow2))))
   `(org-level-5 ((,class (:foreground ,violet4))))
   `(org-level-6 ((,class (:foreground ,aqua))))
   `(org-level-7 ((,class (:foreground ,green))))
   `(org-level-8 ((,class (:foreground ,yellow2))))
   `(org-link ((,class (:foreground ,blue4 :underline t))))
   `(org-todo ((,class (:foreground ,red :weight bold))))
   `(org-done ((,class (:foreground ,green :weight bold))))
   `(org-date ((,class (:foreground ,violet4))))
   `(org-block ((,class (:background ,pearl-bg1))))
   `(org-block-begin-line ((,class (:background ,pearl-bg1 :foreground ,gray3))))
   `(org-block-end-line ((,class (:background ,pearl-bg1 :foreground ,gray3))))
   `(org-code ((,class (:foreground ,yellow2))))
   `(org-verbatim ((,class (:foreground ,orange))))

   ;; JavaScript/TypeScript modes
   `(js2-function-param ((,class (:foreground ,blue5))))
   `(js2-external-variable ((,class (:foreground ,orange))))
   `(js2-instance-member ((,class (:foreground ,fg))))
   `(js2-jsdoc-tag ((,class (:foreground ,gray3))))
   `(js2-jsdoc-type ((,class (:foreground ,gray3))))
   `(js2-jsdoc-value ((,class (:foreground ,gray3))))
   `(js2-object-property ((,class (:foreground ,fg))))
   `(js2-private-function-call ((,class (:foreground ,blue4))))
   `(js2-warning ((,class (:underline (:color ,orange2 :style wave)))))
   `(js2-error ((,class (:underline (:color ,red3 :style wave)))))
   `(js2-function-call ((,class (:foreground ,blue4))))
   `(js2-keywords ((,class (:foreground ,violet4))))
   `(js2-this-face ((,class (:foreground ,violet4))))

   ;; TypeScript specific
   `(typescript-jsdoc-tag ((,class (:foreground ,gray3))))
   `(typescript-jsdoc-type ((,class (:foreground ,gray3))))
   `(typescript-jsdoc-value ((,class (:foreground ,gray3))))
   `(typescript-access-modifier-face ((,class (:foreground ,violet4))))
   `(typescript-primitive-face ((,class (:foreground ,aqua))))
   `(typescript-type-face ((,class (:foreground ,aqua))))
   `(typescript-class-name-face ((,class (:foreground ,aqua))))
   `(typescript-property-face ((,class (:foreground ,orange))))

   ;; Web mode
   `(web-mode-doctype-face ((,class (:foreground ,gray3))))
   `(web-mode-html-tag-face ((,class (:foreground ,violet4))))
   `(web-mode-html-tag-bracket-face ((,class (:foreground ,gray3))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,aqua))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,green))))
   `(web-mode-html-attr-equal-face ((,class (:foreground ,gray3))))
   `(web-mode-builtin-face ((,class (:foreground ,aqua))))
   `(web-mode-keyword-face ((,class (:foreground ,violet4))))
   `(web-mode-string-face ((,class (:foreground ,green))))
   `(web-mode-comment-face ((,class (:foreground ,gray3))))
   `(web-mode-block-control-face ((,class (:foreground ,violet4))))
   `(web-mode-block-delimiter-face ((,class (:foreground ,gray3))))
   `(web-mode-css-selector-face ((,class (:foreground ,yellow2))))
   `(web-mode-css-property-name-face ((,class (:foreground ,aqua))))
   `(web-mode-css-at-rule-face ((,class (:foreground ,violet4))))
   `(web-mode-function-call-face ((,class (:foreground ,blue4))))
   `(web-mode-variable-name-face ((,class (:foreground ,fg))))

   ;; CSS/SCSS mode
   `(css-selector ((,class (:foreground ,yellow2))))
   `(css-property ((,class (:foreground ,aqua))))
   `(css-proprietary-property ((,class (:foreground ,aqua))))
   `(scss-variable-color ((,class (:foreground ,violet4))))
   `(scss-at-rule ((,class (:foreground ,violet4))))
   `(scss-selector ((,class (:foreground ,yellow2))))
   `(scss-property ((,class (:foreground ,aqua))))

   ;; Markdown mode
   `(markdown-header-face-1 ((,class (:foreground ,violet4 :weight bold))))
   `(markdown-header-face-2 ((,class (:foreground ,blue4 :weight bold))))
   `(markdown-header-face-3 ((,class (:foreground ,green :weight bold))))
   `(markdown-header-face-4 ((,class (:foreground ,yellow2 :weight bold))))
   `(markdown-bold-face ((,class (:weight bold))))
   `(markdown-italic-face ((,class (:slant italic))))
   `(markdown-link-face ((,class (:foreground ,blue4 :underline t))))
   `(markdown-url-face ((,class (:foreground ,blue4))))
   `(markdown-code-face ((,class (:background ,pearl-bg1 :foreground ,yellow2))))
   `(markdown-inline-code-face ((,class (:background ,pearl-bg1 :foreground ,yellow2))))
   `(markdown-pre-face ((,class (:foreground ,yellow2))))

   ;; Company mode
   `(company-tooltip ((,class (:background ,pearl-bg1 :foreground ,fg))))
   `(company-tooltip-selection ((,class (:background ,pearl-bg2 :foreground ,fg))))
   `(company-tooltip-common ((,class (:foreground ,blue4))))
   `(company-tooltip-annotation ((,class (:foreground ,gray3))))
   `(company-scrollbar-bg ((,class (:background ,pearl-bg1))))
   `(company-scrollbar-fg ((,class (:background ,pearl-bg2))))
   `(company-preview ((,class (:foreground ,blue4))))
   `(company-preview-common ((,class (:foreground ,blue4 :background ,pearl-bg2))))

   ;; LSP mode
   `(lsp-face-highlight-textual ((,class (:background ,blue2))))
   `(lsp-face-highlight-read ((,class (:background ,blue2))))
   `(lsp-face-highlight-write ((,class (:background ,blue2))))
   `(lsp-face-semhl-constant ((,class (:foreground ,orange))))
   `(lsp-face-semhl-variable ((,class (:foreground ,fg))))
   `(lsp-face-semhl-function ((,class (:foreground ,blue4))))
   `(lsp-face-semhl-method ((,class (:foreground ,blue4))))
   `(lsp-face-semhl-class ((,class (:foreground ,aqua))))
   `(lsp-face-semhl-keyword ((,class (:foreground ,violet4))))
   `(lsp-face-semhl-comment ((,class (:foreground ,gray3))))
   `(lsp-face-semhl-string ((,class (:foreground ,green))))
   `(lsp-face-semhl-type ((,class (:foreground ,aqua))))
   `(lsp-face-semhl-struct ((,class (:foreground ,aqua))))
   `(lsp-face-semhl-interface ((,class (:foreground ,aqua))))
   `(lsp-face-semhl-enum ((,class (:foreground ,aqua))))
   `(lsp-face-semhl-property ((,class (:foreground ,fg))))

   ;; Tree-sitter faces
   `(tree-sitter-hl-face:attribute ((,class (:foreground ,yellow2))))
   `(tree-sitter-hl-face:comment ((,class (:foreground ,gray3))))
   `(tree-sitter-hl-face:constant ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:constant.builtin ((,class (:foreground ,orange :weight bold))))
   `(tree-sitter-hl-face:constructor ((,class (:foreground ,yellow2))))
   `(tree-sitter-hl-face:escape ((,class (:foreground ,red2))))
   `(tree-sitter-hl-face:function ((,class (:foreground ,blue4))))
   `(tree-sitter-hl-face:function.builtin ((,class (:foreground ,blue4))))
   `(tree-sitter-hl-face:function.call ((,class (:foreground ,blue4))))
   `(tree-sitter-hl-face:function.macro ((,class (:foreground ,violet4))))
   `(tree-sitter-hl-face:function.special ((,class (:foreground ,yellow2))))
   `(tree-sitter-hl-face:keyword ((,class (:foreground ,violet4))))
   `(tree-sitter-hl-face:label ((,class (:foreground ,violet4))))
   `(tree-sitter-hl-face:method ((,class (:foreground ,blue4))))
   `(tree-sitter-hl-face:method.call ((,class (:foreground ,blue4))))
   `(tree-sitter-hl-face:number ((,class (:foreground ,pink))))
   `(tree-sitter-hl-face:operator ((,class (:foreground ,gray3))))
   `(tree-sitter-hl-face:property ((,class (:foreground ,fg))))
   `(tree-sitter-hl-face:property.definition ((,class (:foreground ,fg))))
   `(tree-sitter-hl-face:punctuation ((,class (:foreground ,gray3))))
   `(tree-sitter-hl-face:punctuation.bracket ((,class (:foreground ,gray3))))
   `(tree-sitter-hl-face:punctuation.delimiter ((,class (:foreground ,gray3))))
   `(tree-sitter-hl-face:punctuation.special ((,class (:foreground ,yellow2))))
   `(tree-sitter-hl-face:string ((,class (:foreground ,green))))
   `(tree-sitter-hl-face:string.special ((,class (:foreground ,green))))
   `(tree-sitter-hl-face:tag ((,class (:foreground ,violet4))))
   `(tree-sitter-hl-face:type ((,class (:foreground ,aqua))))
   `(tree-sitter-hl-face:type.builtin ((,class (:foreground ,aqua))))
   `(tree-sitter-hl-face:type.parameter ((,class (:foreground ,aqua))))
   `(tree-sitter-hl-face:type.super ((,class (:foreground ,aqua))))
   `(tree-sitter-hl-face:variable ((,class (:foreground ,fg))))
   `(tree-sitter-hl-face:variable.builtin ((,class (:foreground ,violet4))))
   `(tree-sitter-hl-face:variable.parameter ((,class (:foreground ,blue5))))
   `(tree-sitter-hl-face:variable.special ((,class (:foreground ,violet4))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,violet4))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,yellow2))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,blue4))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,pink))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,orange))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,violet4))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,yellow2))))

   ;; Which-key
   `(which-key-key-face ((,class (:foreground ,violet4))))
   `(which-key-separator-face ((,class (:foreground ,gray3))))
   `(which-key-command-description-face ((,class (:foreground ,fg))))
   `(which-key-group-description-face ((,class (:foreground ,blue4))))
   `(which-key-local-map-description-face ((,class (:foreground ,yellow2))))

   ;; Flycheck
   `(flycheck-error ((,class (:underline (:color ,red3 :style wave)))))
   `(flycheck-warning ((,class (:underline (:color ,orange2 :style wave)))))
   `(flycheck-info ((,class (:underline (:color ,green :style wave)))))
   `(flycheck-fringe-error ((,class (:foreground ,red3))))
   `(flycheck-fringe-warning ((,class (:foreground ,orange2))))
   `(flycheck-fringe-info ((,class (:foreground ,green))))

   ;; Git gutter
   `(git-gutter:added ((,class (:foreground ,git-green :weight bold))))
   `(git-gutter:deleted ((,class (:foreground ,git-red :weight bold))))
   `(git-gutter:modified ((,class (:foreground ,git-yellow :weight bold))))

   ;; Magit
   `(magit-branch-current ((,class (:foreground ,blue4 :weight bold))))
   `(magit-branch-local ((,class (:foreground ,teal2))))
   `(magit-branch-remote ((,class (:foreground ,green2))))
   `(magit-diff-added ((,class (:background ,green3 :foreground ,green))))
   `(magit-diff-added-highlight ((,class (:background ,green3 :foreground "#003300"))))
   `(magit-diff-removed ((,class (:background ,red4 :foreground ,red))))
   `(magit-diff-removed-highlight ((,class (:background ,red4 :foreground ,red2))))
   `(magit-diff-context ((,class (:foreground ,gray3))))
   `(magit-diff-context-highlight ((,class (:background ,pearl-bg1 :foreground ,fg))))
   `(magit-section-heading ((,class (:foreground ,violet4 :weight bold))))
   `(magit-section-highlight ((,class (:background ,pearl-bg1))))
   `(magit-hash ((,class (:foreground ,gray3))))

   ;; Helm
   `(helm-header ((,class (:background ,pearl-bg0 :foreground ,fg :weight bold))))
   `(helm-source-header ((,class (:background ,pearl-bg2 :foreground ,fg :weight bold))))
   `(helm-selection ((,class (:background ,pearl-bg2))))
   `(helm-selection-line ((,class (:background ,pearl-bg2))))
   `(helm-visible-mark ((,class (:background ,pearl-bg2 :foreground ,yellow2))))
   `(helm-candidate-number ((,class (:foreground ,blue4))))
   `(helm-separator ((,class (:foreground ,violet4))))
   `(helm-match ((,class (:foreground ,yellow2 :weight bold))))

   ;; Ivy
   `(ivy-current-match ((,class (:background ,pearl-bg2 :foreground ,fg))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,yellow2))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,yellow2 :weight bold))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,green))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,green :weight bold))))

  ;; Centaur Tabs
  `(centaur-tabs-default ((,class (:background ,pearl-bg1 :foreground ,gray3))))
  `(centaur-tabs-selected ((,class (:background ,pearl-bg0 :foreground ,fg :weight bold))))
  `(centaur-tabs-unselected ((,class (:background ,pearl-bg2 :foreground ,gray2))))
  `(centaur-tabs-selected-modified ((,class (:background ,pearl-bg0 :foreground ,orange :weight bold))))
  `(centaur-tabs-unselected-modified ((,class (:background ,pearl-bg2 :foreground ,orange2))))
  `(centaur-tabs-active-bar-face ((,class (:background ,blue4))))
  `(centaur-tabs-modified-marker-selected ((,class (:inherit centaur-tabs-selected-modified :foreground ,orange))))
  `(centaur-tabs-modified-marker-unselected ((,class (:inherit centaur-tabs-unselected-modified :foreground ,orange2))))
  `(centaur-tabs-active-tab ((,class (:background ,pearl-bg0 :foreground ,fg :weight bold))))
  `(centaur-tabs-inactive-tab ((,class (:background ,pearl-bg2 :foreground ,gray2))))
  `(centaur-tabs-selected-tab ((,class (:background ,green :foreground ,pearl-bg0 :weight bold))))
  `(+workspace-tab-selected-face ((,class (:background ,green :foreground ,pearl-bg0 :weight bold))))
  `(+workspace-tab-face ((,class (:background ,pearl-bg2 :foreground ,gray2))))
  `(tab-bar ((,class (:background ,pearl-bg1 :foreground ,fg))))
  `(tab-bar-tab ((,class (:background ,pearl-bg0 :foreground ,fg :weight bold))))
  `(tab-bar-tab-inactive ((,class (:background ,pearl-bg2 :foreground ,gray2))))
  `(tab-line ((,class (:background ,pearl-bg1 :foreground ,fg))))
  `(tab-line-tab ((,class (:background ,pearl-bg0 :foreground ,fg :weight bold))))
  `(tab-line-tab-inactive ((,class (:background ,pearl-bg2 :foreground ,gray2))))
  `(tab-line-tab-current ((,class (:background ,pearl-bg0 :foreground ,fg :weight bold))))
  `(tab-line-close-highlight ((,class (:foreground ,red3))))

  ;; Term colors (for terminal emulation) - Better contrast for Pearl
  `(term-color-black ((,class (:background ,fg :foreground ,fg))))
  `(term-color-red ((,class (:background ,red2 :foreground ,red2))))
  `(term-color-green ((,class (:background ,green :foreground ,green))))
  `(term-color-yellow ((,class (:background ,yellow :foreground ,yellow))))
  `(term-color-blue ((,class (:background ,blue4 :foreground ,blue4))))
  `(term-color-magenta ((,class (:background ,violet4 :foreground ,violet4))))
  `(term-color-cyan ((,class (:background ,teal3 :foreground ,teal3))))
  `(term-color-white ((,class (:background ,pearl-bg1 :foreground ,pearl-bg1))))

  ;; Eshell - These look good but could use minor adjustments
  `(eshell-prompt ((,class (:foreground ,violet4 :weight bold))))
  `(eshell-ls-archive ((,class (:foreground ,red2))))
  `(eshell-ls-backup ((,class (:foreground ,gray3))))
  `(eshell-ls-clutter ((,class (:foreground ,gray2))))
  `(eshell-ls-directory ((,class (:foreground ,blue4 :weight bold))))
  `(eshell-ls-executable ((,class (:foreground ,green :weight bold))))
  `(eshell-ls-missing ((,class (:foreground ,red3))))
  `(eshell-ls-product ((,class (:foreground ,yellow))))
  `(eshell-ls-readonly ((,class (:foreground ,orange))))
  `(eshell-ls-special ((,class (:foreground ,violet4))))
  `(eshell-ls-symlink ((,class (:foreground ,teal1))))
  `(eshell-ls-unreadable ((,class (:foreground ,red2))))

  `(vterm-color-default ((,class (:foreground ,fg :background ,pearl-bg0))))
  `(vterm-color-black ((,class (:foreground ,gray3 :background ,gray3))))
  `(vterm-color-red ((,class (:foreground ,red :background ,red4))))
  `(vterm-color-green ((,class (:foreground ,green2 :background ,green3))))
  `(vterm-color-yellow ((,class (:foreground ,orange2 :background ,yellow4))))
  `(vterm-color-blue ((,class (:foreground ,teal2 :background ,blue2))))
  `(vterm-color-magenta ((,class (:foreground ,violet2 :background ,violet3))))
  `(vterm-color-cyan ((,class (:foreground ,aqua2 :background ,cyan))))
  `(vterm-color-white ((,class (:foreground ,pearl-bg0 :background ,pearl-bg1))))

  ;; Bright variants with better contrast
  `(vterm-color-bright-black ((,class (:foreground ,gray2))))
  `(vterm-color-bright-red ((,class (:foreground ,red))))
  `(vterm-color-bright-green ((,class (:foreground ,green))))
  `(vterm-color-bright-yellow ((,class (:foreground ,orange))))
  `(vterm-color-bright-blue ((,class (:foreground ,blue4))))
  `(vterm-color-bright-magenta ((,class (:foreground ,violet4))))
  `(vterm-color-bright-cyan ((,class (:foreground ,teal1))))
  `(vterm-color-bright-white ((,class (:foreground ,pearl-bg2)))))

  (custom-theme-set-variables
   'kanso-pearl
   `(ansi-color-names-vector
     [,fg2 ,red ,green ,yellow ,blue4 ,pink ,aqua ,pearl-bg0])))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'kanso-pearl)

;;; kanso-pearl-theme.el ends here
