;;; cheatsheets.el --- A cheatsheet manager for Emacs.

;;; Commentary:

;;; Code:

(defun get-cheatsheet (cheatsheet)
  "Gets CHEATSHEET."
  (interactive "sOpen cheatsheet: ")
  (let ((buffer-name (format "*cheatsheet<%s>*" cheatsheet)))
    (with-temp-buffer-window buffer-name
	'(nil (mode . plantuml-mode))
	nil
      (princ "@startuml\n  test\n@enduml"))))

(provide 'cheatsheets)

;;; cheatsheets.el ends here
