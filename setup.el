;; xquery mode
(require 'xquery-mode)
(autoload 'xquery-mode "xquery-mode" "XQuery mode" t )
(setq auto-mode-alist
      (append '(("\\.xqy$" . xquery-mode)) auto-mode-alist))
