(defvar ruby-guard-buffer-name "*guard*")



(defun ruby-guard-available-p ()
  (file-exists-p (projectile-expand-root "Guardfile")))

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
