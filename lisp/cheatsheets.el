;;; cheatsheets.el --- A cheatsheet manager for Emacs.

;;; Commentary:

;;; Code:

(defun get-cheatsheet (cheatsheet)
  "CHEATSHEET being one of the provided cheatsheets.
This function will display it read-only and the indicated mode."
  (interactive "sOpen cheatsheet: ")
  (let ((buffer-name (format "*cheatsheet<%s>*" cheatsheet)))
    (with-temp-buffer-window buffer-name
	'(display-buffer-at-bottom (mode . '(plantuml-mode read-only-mode)))
	nil
      (princ "@startuml\n  test\n@enduml"))))

(provide 'cheatsheets)

;;; cheatsheets.el ends here
