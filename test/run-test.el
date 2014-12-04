(defvar ruby-guard-test-dir (file-name-directory load-file-name))
(defvar ruby-guard-root-dir (append ruby-guard-test-dir ".."))

(mapc (lambda (p)
        (add-to-list 'load-path p))
      (list ruby-guard-test-dir ruby-guard-root-dir))

(load "ruby-guard-test")
