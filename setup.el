;; xquery mode
(require 'xquery-mode)
(autoload 'xquery-mode "xquery-mode" "XQuery mode" t )
(setq auto-mode-alist
      (append '(("\\.xqy$" . xquery-mode)) auto-mode-alist))
(add-hook 'xquery-mode-hook
  '(lambda ()
     (make-local-variable 'write-contents-hooks)
     (add-hook 'write-contents-hooks 'buffer-replace-all-tabs)))
