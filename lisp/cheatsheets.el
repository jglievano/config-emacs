;;; cheatsheets.el --- A cheatsheet manager for Emacs.

;;; Commentary:

;;; Code:

(defun get-cheatsheet (cheatsheet)
  "Gets CHEATSHEET."
  (interactive "sOpen cheatsheet: ")
  (let* ((buffer-name (format "*cheatsheet<%s>*" cheatsheet))
	(buffer (generate-new-buffer buffer-name)))
    (message "the buffer was generated.")))


(provide 'cheatsheets)

;;; cheatsheets.el ends here
