;;; -*- lexical-binding: t; -*-

;; mirrors a package.el repository into specified subdirectory
;; you can run a web-server in said subdirectory and use it as a package archive

(defun mirror-repositories (repos out-dir)
  (unless (file-exists-p out-dir)
    (make-directory out-dir))

  (dolist (repo repos)
    (mirror-repository repo out-dir)))

(defun mirror-repository (repo out-base-dir)
  (let* ((repo-name (car repo))
         (repo-url (cdr repo))
         (out-dir (concat (file-name-as-directory out-base-dir) repo-name)))
    (unless (file-exists-p out-dir)
      (make-directory out-dir))
    (dolist (package (read-archive repo-url out-dir))
      (save-package repo-url
                    (get-full-package-name package)
                    out-dir))))

(defun save-package (base-url package-name out-dir)
  (let ((file-name (concat (file-name-as-directory out-dir) package-name)))
    (if (file-exists-p file-name)
        (message (concat "Already downloaded " package-name))
      (message (concat "Downloading " package-name))
      (download-package (concat base-url package-name) file-name))))

(defun download-package (package-url out-path)
  (url-copy-file package-url out-path))

(defun read-archive (url out-dir)
  (with-temp-file (expand-file-name "archive-contents" out-dir)
    (url-insert-file-contents (concat url "archive-contents"))
    (cdr (read (current-buffer)))))

(defun get-full-package-name (package)
  ;; name-version.extension
  (concat
   (get-package-name package)
   "-"
   (get-package-version-string package)
   "."
   (get-package-extension package)))

(defun get-package-name (package)
  ;; (package-name ...)
  (symbol-name (car package)))

(defun get-package-version-string (package)
  ;; (package-name [(date version) ...])"
  (let ((version-tuple (elt (cdr package) 0)))
    (mapconcat #'number-to-string version-tuple ".")))

(defun get-package-extension (package)
  ;; (package-name . [(date version) something docstring package-type])
  ;; (car          . [    0              1       2           3       ])
  (let ((package-type (elt (cdr package) 3)))
    (cond ((eq package-type 'single) "el")
          ((eq package-type 'tar) "tar"))))
