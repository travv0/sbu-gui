(defpackage :opt-commands
  (:use #:cl #:alexandria #:metabang-bind #:serapeum)
  (:export #:define-command
           #:remove-command
           #:extra-free-args
           #:missing-free-args
           #:general-args-error
           #:describe-commands
           #:handle-command
           #:commands
           #:unknown-command
           #:unknown-command-command
           #:similar-opts
           #:build-opt-choices))

(in-package :opt-commands)

(defstruct free-arg name count required)

(defvar *commands* (dict)
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
                 :free-args (mapcar (lambda (arg)
                                      (when (eq (free-arg-count arg) :many)
                                        (setf (free-arg-name arg) (concat (free-arg-name arg) "...")))
                                      arg)
                                    (mapcar (op (apply #'make-free-arg :name _)) ',free-args))
                 :make-opts (lambda ()
                              (opts:define-opts
                                ,@options
                                (:name :help
                                 :description "Print this help"
                                 :short #\h
                                 :long "help")))))))

(defun remove-command (command-name)
  (remhash command-name *commands*))

(define-condition general-args-error (opts:troublesome-option)
  ((error-string :initarg :error-string :reader error-string))
  (:report (lambda (condition stream)
             (format stream (error-string condition)))))

(define-condition missing-free-args (opts:troublesome-option)
  ((args :initarg :args :accessor args))
  (:report (lambda (condition stream)
             (format stream "missing arguments: ~{~s~^, ~}" (mapcar #'free-arg-name (args condition))))))

(define-condition extra-free-args (opts:troublesome-option)
  ((args :initarg :args :accessor args))
  (:report (lambda (condition stream)
             (format stream "extra arguments provided: ~{~s~^, ~}" (args condition)))))

(defparameter *argument-block-width* 25)
(defparameter *max-width* 80)

(defun describe-commands (&key prefix suffix usage-of brief (usage-of-label "Usage"))
  "Print the help screen showing which commands are available.

`prefix' will be printed before the list of available commands.
`suffix' will be printed after the list of available commands.
`usage-of' take the name of the application and uses it to show
how the commands are used."
  (when prefix
    (format *error-output* "~a~&~%" prefix))
  (when usage-of
    (format *error-output* "~a: ~a~a~%~%"
            usage-of-label
            usage-of
            (print-usage (+ (length usage-of-label)
                            (length usage-of)
                            2) ; colon and space
                         *max-width*
                         opts::*options*
                         '("COMMAND"))))
  (opts:describe :stream *error-output*
                 :argument-block-width *argument-block-width*
                 :max-width *max-width*
                 :brief brief
                 :defined-options (if brief '() opts::*options*))
  (when (not brief)
    (format *error-output* "~a~&"
            (~>> *commands*
                 hash-table-alist
                 (sort _ #'string-lessp :key #'car)
                 (mapcar (op (list (1+ *argument-block-width*)
                                   (car _1)
                                   (getf (cdr _1) :description))))
                 (format nil "Available commands:~%~:{~2t~va~a~%~}~%~@[~a~]"
                         _ suffix)))))

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

(define-condition unknown-command (opts:troublesome-option)
  ((command :initarg :command :accessor unknown-command-command))
  (:report (lambda (c s)
             (format s "unknown command: ~s" (unknown-command-command c)))))

(defun handle-command (command args &optional application-name)
  (if (set-opts command)
      (let ((free-arg-names (string-join (mapcar #'free-arg-name (get-command-free-args command))
                                         " ")))
        (handler-case
            (bind ((command-function (get-command-function command))
                   ((:values min-count max-count)
                    (get-free-arg-checks (get-command-free-args command)))
                   ((:values options free-args) (when args (opts:get-opts args))))
              (if (getf options :help)
                  (opts:describe :argument-block-width *argument-block-width*
                                 :stream *error-output*
                                 :max-width *max-width*
                                 :usage-of (when application-name
                                             (format nil "~a ~a" application-name command))
                                 :args free-arg-names)
                  (cond ((< (length free-args) min-count)
                         (error 'missing-free-args :args (get-command-free-args command)))
                        ((not (<= min-count (length free-args) max-count))
                         (error 'extra-free-args :args (drop max-count free-args)))
                        (t
                         (funcall command-function options free-args)))))
          (opts:troublesome-option (condition)
            (let ((similar-output (when (slot-boundp condition 'opts:option)
                                    (similar-opts (opts:option condition)
                                                  (build-opt-choices)))))
              (opts:describe :argument-block-width *argument-block-width*
                             :stream *error-output*
                             :brief t
                             :max-width *max-width*
                             :usage-of (when application-name
                                         (format nil "~a ~a" application-name command))
                             :args free-arg-names
                             :prefix (format nil "Error: ~a~@[~%~%~a~]" condition similar-output))))))
      (error 'unknown-command :command command)))

(defun build-opt-choices ()
  (loop for opt in opts::*options*
        for long-opt = (when (opts::long opt) (concat "--" (opts::long opt)))
        for short-opt = (when (opts::short opt) (format nil "-~c" (opts::short opt)))
        when long-opt
          collect long-opt
        when short-opt
          collect short-opt))

(defun similar-opts (bad-option choices)
  "Takes an option that didn't match, `bad-option', and the `choices'
that it had tried to match, and returns a formatted string asking the
user if they meant the choices that were similar to the bad option."
  (let* ((similar (loop for opt in choices
                        when (< (mk-string-metrics:damerau-levenshtein bad-option opt) 3)
                          collect opt)))
    (cond ((= (length similar) 1)
           (format nil "Did you mean this?~%~t~a" (first similar)))
          ((> (length similar) 1)
           (format nil "Did you mean one of these?~{~%~t~a~}" similar)))))

(defun print-usage (margin max-width defined-options free-args)
  "Return a string containing info about defined options. All options are
displayed on one line, although this function tries to print it elegantly if
it gets too long. MARGIN specifies margin."
  (let ((fill-col (- max-width margin))
        (i 0)
        (last-newline 0))
    (with-output-to-string (s)
      (dolist (opt defined-options)
        (with-slots (opts::short opts::long opts::required opts::arg-parser opts::meta-var) opt
          (let* ((str
                   (format nil " [~a]"
                           (concatenate
                            'string
                            (if opts::short (format nil "-~c" opts::short) "")
                            (if (and opts::short opts::long) "|" "")
                            (if opts::long  (format nil "--~a" opts::long) "")
                            (if opts::arg-parser (format nil " ~a" opts::meta-var) "")
                            (if opts::required (format nil " (Required)") ""))))
                 (length (length str)))
            (when (> (- (+ i length) last-newline) fill-col)
              (terpri s)
              (dotimes (x margin)
                (princ #\space s))
              (setf last-newline i))
            (incf i length)
            (princ str s))))
      (dolist (arg free-args)
        (let* ((str (format nil " ~a" arg))
               (length (length str)))
          (when (> (- (+ i length) last-newline) fill-col)
            (terpri s)
            (dotimes (x margin)
              (princ #\space s))
            (setf last-newline i))
          (incf i length)
          (princ str s))))))
