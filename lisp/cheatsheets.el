;;; cheatsheets.el --- A cheatsheet manager for Emacs.

;;; Commentary:

;;; Code:

(defun get-cheatsheet (cheatsheet)
  "Gets CHEATSHEET."
  (interactive "sOpen cheatsheet: ")
  (let ((old-buffer (current-buffer))
	(buffer-name (format "*cheatsheet<%s>*" cheatsheet)))
    (with-current-buffer-window buffer-name
	nil
	nil
	(lambda ()
	  (insert "@startuml\n")
	  (insert "  This is a sample\n")
	  (insert "@enduml"))
	)
    (set-buffer old-buffer)))

(provide 'cheatsheets)

;;; cheatsheets.el ends here
