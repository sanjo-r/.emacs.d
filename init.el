;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))
;; 引数のディレクトとそのサブディレクトリをload-pathに追加
;; (add-to-load-path "elisp" "conf" "public_repos")

;; package.elを有効化
;; (require 'package)
;; パッケージリポジトリにMarmaladeとMELPAを追加
;; (add-to-list
 ;; 'package-archives
 ;; '("marmalade" . "https://marmalade-repo.org/packages/"))
;; (add-to-list
 ;; 'package-archives
 ;; '("melpa" . "https://melpa.org/packages/") t)
;; インストール済みのElispを読み込む
;; (package-initialize)

;; 以下、leafの記述
;; leafとは https://github.com/conao3/leaf.el#install
;; 以下のコードはここから https://emacs-jp.github.io/tips/emacs-in-2020

;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; ここにいっぱい設定を書く
(provide 'init)

;; 文字コード指定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; カラム番号を表示
(column-number-mode t)

;; ファイルサイズを表示
(size-indication-mode t)

;; タイトルバーにファイルのパスを表示
(setq frame-title-format "%f")

;; 行番号を常に表示する
(global-linum-mode t)
;; (setq linum-format "%d ")    ; 行番号後ろにスペース

;; TABの設定
(setq-default tab-width 2)

;; 背景色を変更
;; (set-face-background 'region "darkgreen")

;; paren-mode:対応するカッコを強調して表示する
;; (setq show-paren-delay 0) ;表示までの秒数
;; (show-paren-mode t)
;; parenのスタイル:expressionはカッコ内も強調
;; (setq show-paren-style 'expression)
;; フェイスを変更
;; (set-face-background 'show-paren-match-face nil)

;; テーマ deep-blue を適用
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/"))
(load-theme 'deep-blue t t)
 (enable-theme 'deep-blue)

;; Terminal emulator multi-term
(require 'multi-term)
(when (require 'multi-term nil t)
  ;; シェルの指定
  ;; (setq multi-term-program "/usr/local/bin/zsh"))
  (setq multi-term-program "/usr/local/bin/bash"))

;; helm
(require 'helm)
(require 'helm-config)

;; 文法チェックツール Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Auto-Complete
;; (add-to-list 'load-path
;;            (file-name-as-directory "~/.emacs.d/")
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (global-auto-complete-mode t)

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

(require 'minimap)

;; 全角スペースの可視化
(require 'whitespace)
(global-whitespace-mode t)
(set-face-background 'whitespace-space nil) ; 半角スペースの視覚化を無効化
(set-face-background 'whitespace-indentation nil) ; 行頭の8つ以上のスペースの視覚化を無効化
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
			'(space-mark ?\x3000 [?\□]))

;; バックスラッシュの入力設定
(define-key global-map [?\M-¥] [?\\])
;; インクリメンタルサーチの設定
(defun isearch-add-backslash()
  (interactive)
  (isearch-printing-char ?\\ 1))
(define-key isearch-mode-map [?\M-¥] 'isearch-add-backslash)

;; (require 'dashboard)
;; (dashboard-setup-startup-hook)
;; Or if you use use-package
;; (use-package dashboard
  ;; :ensure t
  ;; :config
;; (dashboard-setup-startup-hook))

;; 閉じカッコ自動挿入
;; (electric-pair-mode 1)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; 以下、Ruby用設定
;; タブ文字を使用する
(setq ruby-indent-tabs-mode t)
;; (require 'ruby-end)
;; (add-hook 'ruby-mode-hook
  ;; '(lambda ()
    ;; (abbrev-mode 1)
    ;; (electric-pair-mode t)
    ;; (electric-indent-mode t)
    ;; (electric-layout-mode t)))

;; (require 'ruby-block)
;; (ruby-block-mode t)
;; (setq ruby-block-highlight-toggle t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(electric-pair-mode t)
 '(package-archives
	 '(("gnu" . "https://elpa.gnu.org/packages/")
		 ("melpa" . "https://melpa.org/packages/")
		 ("org" . "https://orgmode.org/elpa/")))
 '(package-selected-packages
	 '(markdown-mode helm web-mode ruby-end ruby-electric rspec-mode rainbow-delimiters python-mode multi-term minimap leaf-keywords hydra helm-descbinds gnu-elpa flycheck elscreen el-get blackout ac-emoji)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
