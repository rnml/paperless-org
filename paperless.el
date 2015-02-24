(defvar paperless-file (concat temporary-file-directory "paperless.org")
  "org file mirror of Paperless lists")

(defvar paperless-repo (expand-file-name "~/Dropbox/Paperless")
  "dropbox folder for Paperless lists")

(defun paperless-open ()
  "open paperless.org generated from XML files"
  (interactive)
  (shell-command (format "paperless pull %s -src %s" paperless-file paperless-repo))
  (find-file paperless-file)
  (org-align-all-tags)
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook 'paperless-save))

(defun paperless-save ()
  "save paperless.org over XML files"
  (interactive)
  (shell-command (format "paperless push %s -dst %s" paperless-file paperless-repo)))
