(defpackage :opt-commands
  (:use #:cl #:alexandria #:metabang-bind #:serapeum)
  (:export #:define-command
           #:remove-command
           #:extra-free-args
           #:missing-free-args
           #:general-args-error
           #:describe-commands
           #:handle-command
           #:commands))

(in-package :opt-commands)

(defstruct free-arg name count required)

(defparameter *commands* (dict)
  "Hash table of sub-commands added with the `define-command' macro.")

(defun commands ()
  "Returns a list of the names of all commands."
  (hash-table-keys *commands*))

(defmacro define-command ((command-name function &optional description) &body body)
  "Define a sub-command with its own command line arguments.

This macro takes a `command-name' as a string, a `function' that should take 2
arguments - the options and free arguments returned by `opts:get-opts' - and an optional
`description' of the command to be shown on the help screen.  The body of this macro
should be plists that `opts:define-opts' would accept, or strings naming the
free arguments this command accepts."
  (destructuring-bind (&key free-args options) body
    `(setf (@ *commands* ,command-name)
           (list :function ,function
                 :description ,description
                 :free-args (mapcar (op (apply #'make-free-arg :name _)) ',free-args)
                 :make-opts (lambda ()
                              (opts:define-opts
                                ,@options
                                (:name :help
                                 :description "Print this help"
                                 :short #\h
                                 :long "help")))))))

(defun remove-command (command-name)
  (remhash command-name *commands*))

(define-condition general-args-error (opts::troublesome-option)
  ((error-string :initarg :error-string :reader error-string))
  (:report (lambda (condition stream)
             (format stream (error-string condition)))))

(define-condition missing-free-args (opts::troublesome-option)
  ((args :initarg :args :accessor args))
  (:report (lambda (condition stream)
             (format stream "missing arguments: ~{~s~^, ~}" (mapcar #'free-arg-name (args condition))))))

(define-condition extra-free-args (opts::troublesome-option)
  ((args :initarg :args :accessor args))
  (:report (lambda (condition stream)
             (format stream "extra arguments provided: ~{~s~^, ~}" (args condition)))))

(defun describe-commands (&key prefix suffix usage-of)
  "Print the help screen showing which commands are available.

`prefix' will be printed before the list of available commands.
`suffix' will be printed after the list of available commands.
`usage-of' take the name of the application and uses it to show
how the commands are used."
  (opts:describe :usage-of usage-of
                 :args "COMMAND"
                 :prefix prefix
                 :suffix (~>> *commands*
                              hash-table-alist
                              (sort _ #'string-lessp :key #'car)
                              (mapcar (op (list (car _1) (getf (cdr _1) :description))))
                              (format nil "Available commands:~%~:{~2t~16a~a~%~}~%~@[~a~]"
                                      _ suffix))))

(defun set-opts (command)
  "Set the accepted command line arguments to those relevant to `command'.
Returns T if command exists, NIL otherwise."
  (when-let ((opts-function (getf (@ *commands* command) :make-opts)))
    (funcall opts-function)
    t))

(defun get-command-function (command)
  (getf (@ *commands* command) :function))

(defun get-command-free-args (command)
  (getf (@ *commands* command) :free-args))

(defun help-flag-p (args)
  (or (position "-h" args :test 'string=)
      (position "--help" args :test 'string=)))

(defun get-free-arg-checks (free-args)
  (loop with min-count = 0 and max-count = 0
        for free-arg in free-args
        when (free-arg-required free-arg)
          do (incf min-count)
        when (eql (or (free-arg-count free-arg) :one) :one)
          do (incf max-count)
        when (eql (free-arg-count free-arg) :many)
          do (setf max-count most-positive-fixnum)
        finally (return (values min-count max-count))))

(defun handle-command (command args &optional application-name)
  (if (set-opts command)
      (let ((free-arg-names (string-join (mapcar #'free-arg-name (get-command-free-args command))
                                         " ")))
        (if (help-flag-p args)
            (opts:describe :usage-of (when application-name
                                       (format nil "~a ~a" application-name command))
                           :args free-arg-names)
            (handler-case
                (bind ((command-function (get-command-function command))
                       ((:values min-count max-count)
                        (get-free-arg-checks (get-command-free-args command)))
                       ((:values options free-args) (when args (opts:get-opts args))))
                  (cond ((< (length free-args) min-count)
                         (error 'missing-free-args :args (get-command-free-args command)))
                        ((not (<= min-count (length free-args) max-count))
                         (error 'extra-free-args :args (drop max-count free-args)))
                        (t
                         (funcall command-function options free-args))))
              (opts::troublesome-option (condition)
                (opts:describe :usage-of (when application-name
                                           (format nil "~a ~a" application-name command))
                               :args free-arg-names
                               :prefix (format nil "Error: ~a" condition))))))
      (describe-commands :usage-of application-name)))
