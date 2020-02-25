;;; cheatsheets.el --- A cheatsheet manager for Emacs.

;;; Commentary:

;;; Code:

(defun get-cheatsheet (cheatsheet)
  "Gets CHEATSHEET."
  (interactive "sOpen cheatsheet: ")
  (let ((buffer-name (format "*cheatsheet<%s>*" cheatsheet)))
    (with-output-to-temp-buffer buffer-name
      (princ "@startuml\n  test\n@enduml"))))

(provide 'cheatsheets)

;;; cheatsheets.el ends here
