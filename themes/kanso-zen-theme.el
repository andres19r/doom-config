;;; kanso-zen-theme.el --- A dark theme inspired by Kanso Zen -*- lexical-binding: t -*-

;; Author: Andres Rivero
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: faces, themes
;; URL: https://github.com/yourusername/kanso-zen-theme

;;; Commentary:
;; A dark theme for Emacs based on the Kanso Zen colorscheme.
;; Ported from https://github.com/webhooked/kanso.nvim

;;; Code:

(deftheme kanso-zen
  "A dark theme inspired by Kanso Zen colorscheme.")

(let ((class '((class color) (min-colors 89)))
      ;; Kanso Zen palette - non-saturated variants
      (zen-bg0     "#090E13")
      (zen-bg1     "#1C1E25")
      (zen-bg2     "#22262D")
      (zen-bg3     "#393B44")
      (ink-bg3     "#393B44")
      
      (fg          "#C5C9C7")
      (fg2         "#f2f1ef")
      (gray        "#717C7C")
      (gray2       "#A4A7A4")
      (gray3       "#909398")
      (gray4       "#75797f")
      (gray5       "#5C6066")
      
      ;; Main colors (non-saturated)
      (red         "#C34043")
      (red2        "#E46876")
      (red3        "#c4746e")
      (orange      "#b6927b")
      (orange2     "#b98d7b")
      (yellow      "#DCA561")
      (yellow2     "#E6C384")
      (yellow3     "#c4b28a")
      (green       "#98BB6C")
      (green2      "#87a987")
      (green3      "#8a9a7b")
      (green4      "#6A9589")
      (green5      "#7AA89F")
      (blue        "#7FB4CA")
      (blue2       "#658594")
      (blue3       "#8ba4b0")
      (aqua        "#8ea4a2")
      (violet      "#938AA9")
      (violet2     "#8992a7")
      (pink        "#a292a3")
      
      ;; Diff colors
      (diff-green  "#2B3328")
      (diff-red    "#43242B")
      (diff-blue   "#252535")
      (diff-yellow "#49443C")
      
      ;; Git colors
      (git-green   "#76946A")
      (git-red     "#C34043")
      (git-yellow  "#DCA561")
      
      ;; Alt colors
      (alt-blue2   "#2D4F67"))

  (custom-theme-set-faces
   'kanso-zen

   ;; Basic faces
   `(default ((,class (:foreground ,fg :background ,zen-bg0))))
   `(cursor ((,class (:background ,fg2))))
   `(fringe ((,class (:background ,zen-bg0 :foreground ,gray5))))
   `(highlight ((,class (:background ,zen-bg2))))
   `(region ((,class (:background ,ink-bg3))))
   `(secondary-selection ((,class (:background ,zen-bg2))))
   `(buffer-menu-buffer ((,class (:foreground ,fg :weight bold))))
   `(minibuffer-prompt ((,class (:foreground ,violet2))))
   `(vertical-border ((,class (:foreground ,zen-bg2))))
   `(window-divider ((,class (:foreground ,zen-bg2))))
   `(window-divider-first-pixel ((,class (:foreground ,zen-bg2))))
   `(window-divider-last-pixel ((,class (:foreground ,zen-bg2))))

   ;; Font lock faces - Core syntax highlighting
   `(font-lock-builtin-face ((,class (:foreground ,aqua))))
   `(font-lock-comment-face ((,class (:foreground ,gray4))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,gray4))))
   `(font-lock-constant-face ((,class (:foreground ,yellow3))))
   `(font-lock-doc-face ((,class (:foreground ,gray4))))
   `(font-lock-function-name-face ((,class (:foreground ,aqua))))
   `(font-lock-keyword-face ((,class (:foreground ,violet2))))
   `(font-lock-negation-char-face ((,class (:foreground ,violet2))))
   `(font-lock-preprocessor-face ((,class (:foreground ,violet2))))
   `(font-lock-string-face ((,class (:foreground ,green3))))
   `(font-lock-type-face ((,class (:foreground ,aqua))))
   `(font-lock-variable-name-face ((,class (:foreground ,yellow3))))
   `(font-lock-warning-face ((,class (:foreground ,yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow3))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,yellow3))))

   ;; Mode line
   `(mode-line ((,class (:background ,zen-bg1 :foreground ,fg))))
   `(mode-line-inactive ((,class (:background ,zen-bg0 :foreground ,gray3))))
   `(mode-line-buffer-id ((,class (:weight bold))))
   `(mode-line-emphasis ((,class (:weight bold))))
   `(mode-line-highlight ((,class (:box (:line-width 1 :color ,gray3)))))
   `(mode-line ((,class (:background ,zen-bg1 :foreground ,fg))))
   `(mode-line-inactive ((,class (:background ,zen-bg0 :foreground ,gray3))))
   `(mode-line-buffer-id ((,class (:weight bold))))
   `(mode-line-emphasis ((,class (:weight bold :foreground ,green3)))) ; Use muted green
   `(mode-line-highlight ((,class (:box (:line-width 1 :color ,gray3)))))
   `(mode-line-buffer-id-inactive ((,class (:foreground ,gray3))))
   `(vc-git-state-up-to-date ((,class (:foreground ,green3))))

   ;; Evil mode state colors (if using evil)
   `(evil-normal-state ((,class (:foreground ,green3))))  ; Using muted green3 instead of bright green
   `(evil-insert-state ((,class (:foreground ,blue3))))
   `(evil-visual-state ((,class (:foreground ,yellow3))))
   `(evil-replace-state ((,class (:foreground ,red3))))
   `(evil-operator-state ((,class (:foreground ,aqua))))

   ;; Doom modeline evil states (if using doom-modeline)
   `(doom-modeline-evil-normal-state ((,class (:foreground ,green3))))
   `(doom-modeline-evil-insert-state ((,class (:foreground ,blue3))))
   `(doom-modeline-evil-visual-state ((,class (:foreground ,yellow3))))
   `(doom-modeline-info ((,class (:foreground ,green3))))
   `(doom-modeline-warning ((,class (:foreground ,yellow3))))
   `(doom-modeline-error ((,class (:foreground ,red3))))
   `(doom-modeline-buffer-modified ((,class (:foreground ,orange))))
   `(doom-modeline-project-dir ((,class (:foreground ,violet2))))
   `(doom-modeline-buffer-file ((,class (:foreground ,fg))))

   ;; Version control and git faces
   `(vc-state-base ((,class (:foreground ,gray3))))
   `(vc-up-to-date ((,class (:foreground ,green3))))  ; Use muted green
   `(vc-edited ((,class (:foreground ,yellow3))))     ; Use muted yellow
   `(vc-missing ((,class (:foreground ,red3))))


   ;; Isearch
   `(isearch ((,class (:background ,alt-blue2 :foreground ,fg :weight bold))))
   `(isearch-fail ((,class (:background ,red :foreground ,zen-bg0))))
   `(lazy-highlight ((,class (:background ,zen-bg3 :foreground ,fg2))))

   ;; Diff mode
   `(diff-added ((,class (:background ,diff-green :foreground ,green3))))
   `(diff-changed ((,class (:background ,diff-blue :foreground ,blue3))))
   `(diff-removed ((,class (:background ,diff-red :foreground ,red3))))
   `(diff-header ((,class (:background ,diff-yellow :foreground ,yellow3))))
   `(diff-file-header ((,class (:background ,zen-bg2 :foreground ,fg :weight bold))))
   `(diff-hunk-header ((,class (:background ,zen-bg2 :foreground ,violet2))))

   ;; Compilation
   `(compilation-error ((,class (:foreground ,red))))
   `(compilation-warning ((,class (:foreground ,yellow))))
   `(compilation-info ((,class (:foreground ,green3))))
   `(compilation-mode-line-exit ((,class (:foreground ,green3))))
   `(compilation-mode-line-fail ((,class (:foreground ,red))))
   `(compilation-mode-line-run ((,class (:foreground ,blue3))))

   ;; Org mode
   `(org-level-1 ((,class (:foreground ,violet2 :weight bold))))
   `(org-level-2 ((,class (:foreground ,aqua :weight bold))))
   `(org-level-3 ((,class (:foreground ,green3))))
   `(org-level-4 ((,class (:foreground ,yellow3))))
   `(org-level-5 ((,class (:foreground ,violet2))))
   `(org-level-6 ((,class (:foreground ,aqua))))
   `(org-level-7 ((,class (:foreground ,green3))))
   `(org-level-8 ((,class (:foreground ,yellow3))))
   `(org-link ((,class (:foreground ,blue3 :underline t))))
   `(org-todo ((,class (:foreground ,red :weight bold))))
   `(org-done ((,class (:foreground ,green3 :weight bold))))
   `(org-date ((,class (:foreground ,violet2))))
   `(org-block ((,class (:background ,zen-bg1))))
   `(org-block-begin-line ((,class (:background ,zen-bg1 :foreground ,gray4))))
   `(org-block-end-line ((,class (:background ,zen-bg1 :foreground ,gray4))))
   `(org-code ((,class (:foreground ,yellow3))))
   `(org-verbatim ((,class (:foreground ,orange))))

   ;; JavaScript/TypeScript modes
   `(js2-function-param ((,class (:foreground ,fg))))
   `(js2-external-variable ((,class (:foreground ,yellow3))))
   `(js2-instance-member ((,class (:foreground ,fg))))
   `(js2-jsdoc-tag ((,class (:foreground ,gray4))))
   `(js2-jsdoc-type ((,class (:foreground ,gray4))))
   `(js2-jsdoc-value ((,class (:foreground ,gray4))))
   `(js2-object-property ((,class (:foreground ,fg))))
   `(js2-private-function-call ((,class (:foreground ,aqua))))
   `(js2-warning ((,class (:underline (:color ,yellow :style wave)))))
   `(js2-error ((,class (:underline (:color ,red :style wave)))))
   `(js2-function-call ((,class (:foreground ,aqua))))
   `(js2-keywords ((,class (:foreground ,violet2))))

   ;; TypeScript specific
   `(typescript-jsdoc-tag ((,class (:foreground ,gray4))))
   `(typescript-jsdoc-type ((,class (:foreground ,gray4))))
   `(typescript-jsdoc-value ((,class (:foreground ,gray4))))
   `(typescript-access-modifier-face ((,class (:foreground ,violet2))))
   `(typescript-primitive-face ((,class (:foreground ,aqua))))
   `(typescript-type-face ((,class (:foreground ,aqua))))
   `(typescript-class-name-face ((,class (:foreground ,aqua))))
   `(typescript-property-face ((,class (:foreground ,orange))))
   `(js2-object-property ((,class (:foreground ,orange))))
   `(typescript-ts-mode-property-face ((,class (:foreground ,orange))))
   `(typescript-ts-mode-property-identifier-face ((,class (:foreground ,orange))))

   ;; Web mode
   `(web-mode-doctype-face ((,class (:foreground ,gray4))))
   `(web-mode-html-tag-face ((,class (:foreground ,yellow3))))
   `(web-mode-html-tag-bracket-face ((,class (:foreground ,gray3))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,aqua))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,green3))))
   `(web-mode-html-attr-equal-face ((,class (:foreground ,gray3))))
   `(web-mode-builtin-face ((,class (:foreground ,aqua))))
   `(web-mode-keyword-face ((,class (:foreground ,violet2))))
   `(web-mode-string-face ((,class (:foreground ,green3))))
   `(web-mode-comment-face ((,class (:foreground ,gray4))))
   `(web-mode-block-control-face ((,class (:foreground ,violet2))))
   `(web-mode-block-delimiter-face ((,class (:foreground ,gray3))))
   `(web-mode-css-selector-face ((,class (:foreground ,yellow3))))
   `(web-mode-css-property-name-face ((,class (:foreground ,aqua))))
   `(web-mode-css-at-rule-face ((,class (:foreground ,violet2))))
   `(web-mode-function-call-face ((,class (:foreground ,aqua))))
   `(web-mode-variable-name-face ((,class (:foreground ,fg))))
   `(web-mode-javascript-string-face ((,class (:foreground ,green3))))

   ;; CSS/SCSS mode
   `(css-selector ((,class (:foreground ,violet2))))
   `(css-property ((,class (:foreground ,aqua))))
   `(css-proprietary-property ((,class (:foreground ,aqua))))
   `(scss-variable-color ((,class (:foreground ,violet2))))
   `(scss-at-rule ((,class (:foreground ,violet2))))
   `(scss-selector ((,class (:foreground ,yellow3))))
   `(scss-property ((,class (:foreground ,aqua))))

   ;; Markdown mode
   `(markdown-header-face-1 ((,class (:foreground ,violet2 :weight bold))))
   `(markdown-header-face-2 ((,class (:foreground ,aqua :weight bold))))
   `(markdown-header-face-3 ((,class (:foreground ,green3 :weight bold))))
   `(markdown-header-face-4 ((,class (:foreground ,yellow3 :weight bold))))
   `(markdown-bold-face ((,class (:weight bold))))
   `(markdown-italic-face ((,class (:slant italic))))
   `(markdown-link-face ((,class (:foreground ,blue3 :underline t))))
   `(markdown-url-face ((,class (:foreground ,blue3))))
   `(markdown-code-face ((,class (:background ,zen-bg1 :foreground ,yellow3))))
   `(markdown-inline-code-face ((,class (:background ,zen-bg1 :foreground ,yellow3))))
   `(markdown-pre-face ((,class (:foreground ,yellow3))))

   ;; Company mode
   `(company-tooltip ((,class (:background ,zen-bg1 :foreground ,fg))))
   `(company-tooltip-selection ((,class (:background ,zen-bg3 :foreground ,fg))))
   `(company-tooltip-common ((,class (:foreground ,blue3))))
   `(company-tooltip-annotation ((,class (:foreground ,gray3))))
   `(company-scrollbar-bg ((,class (:background ,zen-bg1))))
   `(company-scrollbar-fg ((,class (:background ,gray5))))
   `(company-preview ((,class (:foreground ,blue3))))
   `(company-preview-common ((,class (:foreground ,blue3 :background ,zen-bg2))))

   ;; LSP mode
   `(lsp-face-highlight-textual ((,class (:background ,alt-blue2))))
   `(lsp-face-highlight-read ((,class (:background ,alt-blue2))))
   `(lsp-face-highlight-write ((,class (:background ,alt-blue2))))
   `(lsp-face-semhl-constant ((,class (:foreground ,yellow3))))
   `(lsp-face-semhl-variable ((,class (:foreground , orange))))
   `(lsp-face-semhl-function ((,class (:foreground ,aqua))))
   `(lsp-face-semhl-method ((,class (:foreground ,aqua))))
   `(lsp-face-semhl-class ((,class (:foreground ,aqua))))
   `(lsp-face-semhl-keyword ((,class (:foreground ,violet2))))
   `(lsp-face-semhl-comment ((,class (:foreground ,gray4))))
   `(lsp-face-semhl-string ((,class (:foreground ,green3))))
   `(lsp-face-semhl-type ((,class (:foreground ,aqua))))
   `(lsp-face-semhl-struct ((,class (:foreground ,aqua))))
   `(lsp-face-semhl-interface ((,class (:foreground ,aqua))))
   `(lsp-face-semhl-enum ((,class (:foreground ,aqua))))
   `(lsp-face-semhl-property ((,class (:foreground ,orange))))
   `(lsp-face-semhl-member ((,class (:foreground ,orange))))
   `(lsp-face-semhl-parameter ((,class (:foreground ,orange))))

   ;; Tree-sitter faces
   `(tree-sitter-hl-face:attribute ((,class (:foreground ,yellow3))))
   `(tree-sitter-hl-face:comment ((,class (:foreground ,gray4))))
   `(tree-sitter-hl-face:constant ((,class (:foreground ,yellow3))))
   `(tree-sitter-hl-face:constant.builtin ((,class (:foreground ,yellow3 :weight bold))))
   `(tree-sitter-hl-face:constructor ((,class (:foreground ,aqua))))
   `(tree-sitter-hl-face:escape ((,class (:foreground ,red3))))
   `(tree-sitter-hl-face:function.builtin ((,class (:foreground ,aqua))))
   `(tree-sitter-hl-face:function.macro ((,class (:foreground ,violet2))))
   `(tree-sitter-hl-face:function.special ((,class (:foreground ,aqua))))
   `(tree-sitter-hl-face:keyword ((,class (:foreground ,violet2))))
   `(tree-sitter-hl-face:label ((,class (:foreground ,violet2))))
   `(tree-sitter-hl-face:number ((,class (:foreground ,pink))))
   `(tree-sitter-hl-face:operator ((,class (:foreground ,gray3))))
   `(tree-sitter-hl-face:property.definition ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:punctuation ((,class (:foreground ,gray3))))
   `(tree-sitter-hl-face:punctuation.bracket ((,class (:foreground ,gray3))))
   `(tree-sitter-hl-face:punctuation.delimiter ((,class (:foreground ,gray3))))
   `(tree-sitter-hl-face:punctuation.special ((,class (:foreground ,yellow3))))
   `(tree-sitter-hl-face:string ((,class (:foreground ,green3))))
   `(tree-sitter-hl-face:string.special ((,class (:foreground ,green3))))
   `(tree-sitter-hl-face:tag ((,class (:foreground ,violet2))))
   `(tree-sitter-hl-face:type ((,class (:foreground ,aqua))))
   `(tree-sitter-hl-face:type.builtin ((,class (:foreground ,aqua))))
   `(tree-sitter-hl-face:type.parameter ((,class (:foreground ,aqua))))
   `(tree-sitter-hl-face:type.super ((,class (:foreground ,aqua))))
   `(tree-sitter-hl-face:variable ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:variable.builtin ((,class (:foreground ,violet2))))
   `(tree-sitter-hl-face:variable.parameter ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:variable.member ((,class (:foreground ,aqua))))
   `(tree-sitter-hl-face:variable.field ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:variable.special ((,class (:foreground ,violet2))))
   `(tree-sitter-hl-face:property.decorator ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:parameter ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:identifier ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:property_identifier.name ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:field ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:field.name ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:member_expression ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:call_expression ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:member_expression.object ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:identifier.object ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:identifier.property ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:type_arguments ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:predefined_type ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:type.argument ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:type.predefined ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:builtin_type ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:function ((,class (:foreground ,yellow2))))
   `(tree-sitter-hl-face:function.call ((,class (:foreground ,yellow2))))
   `(tree-sitter-hl-face:method ((,class (:foreground ,aqua))))
   `(tree-sitter-hl-face:method.call ((,class (:foreground ,orange))))
   `(tree-sitter-hl-face:keyword.this ((,class (:foreground ,violet2))))
   `(tree-sitter-hl-face:property_identifier ((,class (:foreground ,fg))))
   `(tree-sitter-hl-face:property ((,class (:foreground ,fg))))
   `(tree-sitter-hl-face:member ((,class (:foreground ,fg))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,violet2))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,green3))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,yellow3))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,aqua))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,pink))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,orange))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,violet2))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,green3))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,yellow3))))

   ;; Which-key
   `(which-key-key-face ((,class (:foreground ,violet2))))
   `(which-key-separator-face ((,class (:foreground ,gray3))))
   `(which-key-command-description-face ((,class (:foreground ,fg))))
   `(which-key-group-description-face ((,class (:foreground ,aqua))))
   `(which-key-local-map-description-face ((,class (:foreground ,yellow3))))

   ;; Flycheck
   `(flycheck-error ((,class (:underline (:color ,red :style wave)))))
   `(flycheck-warning ((,class (:underline (:color ,yellow :style wave)))))
   `(flycheck-info ((,class (:underline (:color ,green3 :style wave)))))
   `(flycheck-fringe-error ((,class (:foreground ,red))))
   `(flycheck-fringe-warning ((,class (:foreground ,yellow))))
   `(flycheck-fringe-info ((,class (:foreground ,green3))))

   ;; Git gutter
   `(git-gutter:added ((,class (:foreground ,git-green :weight bold))))
   `(git-gutter:deleted ((,class (:foreground ,git-red :weight bold))))
   `(git-gutter:modified ((,class (:foreground ,git-yellow :weight bold))))

   ;; Magit
   `(magit-branch-current ((,class (:foreground ,green3))))
   `(magit-branch-local ((,class (:foreground ,blue3))))
   `(magit-branch-remote ((,class (:foreground ,green3))))
   `(magit-diff-added ((,class (:background ,diff-green :foreground ,green3))))
   `(magit-diff-added-highlight ((,class (:background, green :foreground, diff-green))))
   `(magit-diff-removed ((,class (:background ,diff-red :foreground ,red3))))
   `(magit-diff-removed-highlight ((,class (:background ,diff-red :foreground ,red2))))
   `(magit-diff-context ((,class (:foreground ,gray3))))
   `(magit-diff-context-highlight ((,class (:background ,zen-bg1 :foreground ,fg))))
   `(magit-section-heading ((,class (:foreground ,violet2 :weight bold))))
   `(magit-section-highlight ((,class (:background ,zen-bg1))))
   `(magit-hash ((,class (:foreground ,gray3))))

   ;; Helm
   `(helm-header ((,class (:background ,zen-bg0 :foreground ,fg :weight bold))))
   `(helm-source-header ((,class (:background ,zen-bg2 :foreground ,fg :weight bold))))
   `(helm-selection ((,class (:background ,zen-bg3))))
   `(helm-selection-line ((,class (:background ,zen-bg3))))
   `(helm-visible-mark ((,class (:background ,zen-bg3 :foreground ,yellow3))))
   `(helm-candidate-number ((,class (:foreground ,aqua))))
   `(helm-separator ((,class (:foreground ,violet2))))
   `(helm-match ((,class (:foreground ,yellow3 :weight bold))))

   ;; Ivy
   `(ivy-current-match ((,class (:background ,zen-bg3 :foreground ,fg))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,yellow3))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,yellow3 :weight bold))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,green3))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,green3 :weight bold))))

  ;; Centaur Tabs (Doom Emacs compatibility)
  `(centaur-tabs-default ((,class (:background ,zen-bg1 :foreground ,gray3))))
  `(centaur-tabs-selected ((,class (:background ,zen-bg0 :foreground ,fg :weight bold))))
  `(centaur-tabs-unselected ((,class (:background ,zen-bg2 :foreground ,gray2))))
  `(centaur-tabs-selected-modified ((,class (:background ,zen-bg0 :foreground ,orange :weight bold))))
  `(centaur-tabs-unselected-modified ((,class (:background ,zen-bg2 :foreground ,orange2))))
  `(centaur-tabs-active-bar-face ((,class (:background ,blue3))))
  `(centaur-tabs-modified-marker-selected ((,class (:inherit centaur-tabs-selected-modified :foreground ,orange))))
  `(centaur-tabs-modified-marker-unselected ((,class (:inherit centaur-tabs-unselected-modified :foreground ,orange2))))
  ;; Additional centaur-tabs faces
  `(centaur-tabs-active-tab ((,class (:background ,zen-bg0 :foreground ,fg :weight bold))))
  `(centaur-tabs-inactive-tab ((,class (:background ,zen-bg2 :foreground ,gray2))))
  `(centaur-tabs-selected-tab ((,class (:background ,green3 :foreground ,zen-bg0 :weight bold))))
  ;; Doom-specific workspace tabs
  `(+workspace-tab-selected-face ((,class (:background ,green3 :foreground ,zen-bg0 :weight bold))))
  `(+workspace-tab-face ((,class (:background ,zen-bg2 :foreground ,gray2))))
  ;; Tab Bar (built-in Emacs tabs)
  `(tab-bar ((,class (:background ,zen-bg1 :foreground ,fg))))
  `(tab-bar-tab ((,class (:background ,zen-bg0 :foreground ,fg :weight bold))))
  `(tab-bar-tab-inactive ((,class (:background ,zen-bg2 :foreground ,gray2))))
  ;; Tab Line (built-in Emacs tab line)
  `(tab-line ((,class (:background ,zen-bg1 :foreground ,fg))))
  `(tab-line-tab ((,class (:background ,zen-bg0 :foreground ,fg :weight bold))))
  `(tab-line-tab-inactive ((,class (:background ,zen-bg2 :foreground ,gray2))))
  `(tab-line-tab-current ((,class (:background ,zen-bg0 :foreground ,fg :weight bold))))
  `(tab-line-close-highlight ((,class (:foreground ,red))))
  ;; Term colors (for terminal emulation)
  `(term-color-black ((,class (:background ,zen-bg0 :foreground ,zen-bg0))))
  `(term-color-red ((,class (:background ,red3 :foreground ,red3))))
  `(term-color-green ((,class (:background ,green3 :foreground ,green3))))
  `(term-color-yellow ((,class (:background ,yellow3 :foreground ,yellow3))))
  `(term-color-blue ((,class (:background ,blue3 :foreground ,blue3))))
  `(term-color-magenta ((,class (:background ,pink :foreground ,pink))))
  `(term-color-cyan ((,class (:background ,aqua :foreground ,aqua))))
  `(term-color-white ((,class (:background ,fg :foreground ,fg))))

   ;; Eshell
   `(eshell-prompt ((,class (:foreground ,violet2 :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,red))))
   `(eshell-ls-backup ((,class (:foreground ,gray3))))
   `(eshell-ls-clutter ((,class (:foreground ,gray4))))
   `(eshell-ls-directory ((,class (:foreground ,blue3 :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,green3))))
   `(eshell-ls-missing ((,class (:foreground ,red))))
   `(eshell-ls-product ((,class (:foreground ,yellow3))))
   `(eshell-ls-readonly ((,class (:foreground ,orange))))
   `(eshell-ls-special ((,class (:foreground ,violet2))))
   `(eshell-ls-symlink ((,class (:foreground ,aqua))))
   `(eshell-ls-unreadable ((,class (:foreground ,red)))))

  (custom-theme-set-variables
   'kanso-zen
   `(ansi-color-names-vector
     [,zen-bg0 ,red3 ,green3 ,yellow3 ,blue3 ,pink ,aqua ,fg])))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'kanso-zen)

;;; kanso-zen-theme.el ends here
