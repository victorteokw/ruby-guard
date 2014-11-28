(defvar ruby-guard-buffer-name "*guard*")

(defun ruby-guard-root (&optional last-directory)
  "Return the directory name where guard file is located."
  (ignore-errors
    (let ((current-directory
           (or last-directory (file-truename default-directory))))
      (if (string-equal "/" current-directory)
          nil
        (if (file-exists-p (expand-file-name "Guardfile" current-directory))
            current-directory
          (setq current-directory (expand-file-name ".." current-directory))
          (ruby-guard-root current-directory))))))

(defun ruby-guard ()
  (interactive)
  (if (ruby-guard-available-p)
      (progn
        (if (member projectile-rails-guard-buffer-name
                    (mapcar 'buffer-name (buffer-list)))
            (switch-to-buffer projectile-rails-guard-buffer-name)
          (projectile-rails-with-root
           (async-shell-command (projectile-rails-with-preloader
                                 :spring "spring guard"
                                 :zeus "zeus guard"
                                 :vanilla "bundle exec guard")
                                (get-buffer-create
                                 projectile-rails-guard-buffer-name)))))
    (message "Guardfile not found.")))
