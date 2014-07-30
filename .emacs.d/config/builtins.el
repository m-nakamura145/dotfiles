;;スペルチェック
(setq-default flyspell-mode t)
(setq ispell-dictionary "american")

;; text-modeでバッファーを開いたときに行う設定
(add-hook
 'text-mode-hook
 (lambda ()
   ;; 自動で長過ぎる行を分割する
   (auto-fill-mode 1)))

;; c-modeやc++-modeなどcc-modeベースのモード共通の設定
(add-hook
 'c-mode-common-hook
 (lambda ()
   ;; BSDスタイルをベースにする
   (c-set-style "bsd")

   ;; スペースでインデントをする
   (setq indent-tabs-mode nil)

   ;; インデント幅を2にする
   (setq c-basic-offset 2)

   ;; 自動改行（auto-new-line）と
   ;; 連続する空白の一括削除（hungry-delete）を
   ;; 有効にする
   (c-toggle-auto-hungry-state 1)

   ;; CamelCaseの語でも単語単位に分解して編集する
   ;; GtkWindow         => Gtk Window
   ;; EmacsFrameClass   => Emacs Frame Class
   ;; NSGraphicsContext => NS Graphics Context
   (subword-mode 1)))

;; c++-modeだけの設定
(add-hook
 'c++-mode-hook
 (lambda ()
   ;; 定義開始時の開き波かっこ「{」の前で改行しない
   ;; デフォルトでは前後に改行が入る
   ;; afterを指定すると後だけに改行がはいる
   ;; (defun-open before after)にすると前後に改行が入る
   (setq c-hanging-braces-alist
         `((defun-open after)   ; トップレベルの関数定義
           (class-open after)   ; クラス定義
           (inline-open after)  ; クラス定義内のインラインメソッド定義
           ,@c-hanging-braces-alist))))


;; emacs-lisp-modeでバッファーを開いたときに行う設定
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   ;; スペースでインデントをする
   (setq indent-tabs-mode nil)))
