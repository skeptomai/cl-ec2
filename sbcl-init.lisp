(require 'asdf)
(require 'asdf-install)

(load 
 (merge-pathnames 
  (make-pathname :name "asdf" :type "lisp" :directory '(:relative "lisp")) (user-homedir-pathname)))
(setf asdf:*central-registry*
 (list (merge-pathnames 
  (make-pathname :directory '(:relative "lisp" "asdf-install-dir" "systems")) (user-homedir-pathname)) ))

(push '(#P"/Users/cbrown/lisp/asdf-install-dir/site/" 
        #P"/Users/cbrown/lisp/asdf-install-dir/systems/" "Real Personal Installation") 
      asdf-install:*locations*)

(in-package :asdf-install)
(defun where () (elt asdf-install:*locations* 0))
(defun uninstall (system &optional (prompt t))
  (declare (ignore prompt))
  (ignore-errors
    (let* ((asd (asdf:system-definition-pathname system))
           (dir (asdf::pathname-sans-name+type
                 (asdf::resolve-symlinks asd))))
      (delete-file asd)
      (asdf:run-shell-command "rm -r ~A" (namestring dir)))))
(in-package :common-lisp-user)

;;; If the first user-processable command-line argument is a filename,
;;; disable the debugger, load the file handling shebang-line and quit.
(let ((script (and (second *posix-argv*)
                   (probe-file (second *posix-argv*)))))
  (when script
    ;; Handle shebang-line
    (set-dispatch-macro-character #\# #\!
                                  (lambda (stream char arg)
                                    (declare (ignore char arg))
                                    (read-line stream)))
    ;; Disable debugger
;;     (setf *invoke-debugger-hook*
;;           (lambda (condition hook)
;;             (declare (ignore hook))
;;             ;; Uncomment to get backtraces on errors
;;             ;; (sb-debug:backtrace 20)
;;             (format *error-output* "Error: ~A~%" condition)
;;             (quit)))
    (load script)))
(quit)