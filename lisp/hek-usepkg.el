;;; hek-usepkg.el --- Package configuration tool. -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides a macro to load and setup packages like `use-package'.
;; This package assumes that all configurations are reasonable and no extra
;; verification will be done.

;;; Code:

(defvar hek-usepkg-debug nil
  "If non-nil, try to detect possible problems.")

(defvar hek-usepkg-keywords nil
  "An alist of keywords and their handlers. The order in the list matters.
A handler is a function that takes arguments (PKG-NAME VALUES FLAGS) where
PKG-NAME is the name of current package, VALUES is a list of given arguments,
and FLAGS is a list of flags to determine the behaviors; and that returns nil or
(CODE . FLAGS) where CODE is the generated code that will be used by the macro,
and FLAGS is a list of flags for other handlers.")

(defvar hek-usepkg-sources nil
  "An alist of package sources and their handlers.
A handler is a function that takes arguments (PKG-NAME ARGS ENSURE); and that
returns the code which will be used by the macro. The handler can be nil.")

(defvar hek-usepkg-ensure nil
  "If non-nil, ensure the packages are installed.")

(defun hek-usepkg--error (pkg-name section problem detail)
  (error "%s/%s: %s: %S" pkg-name section problem detail))

(defun hek-usepkg--group-opts (pkg-name options)
  ;; (:kw1 val1 val2 :kw2 val3 val4) => ((:kw1 val1 val2) (:kw2 val3 val4))
  (unless (keywordp (car options))
    (hek-usepkg--error pkg-name "top" "keyword is not given" (car options)))
  (let (groups)
    (while options
      (let ((pos options))
        (catch 'found
          (while t
            (when (let ((next (cdr pos)))
                    (or (null next)
                        (keywordp (car next))))
              (if-let ((entry (assq (car options) groups)))
                  (nconc entry (cdr options))
                (push options groups))
              (setq options (cdr pos))
              (setcdr pos nil)
              (throw 'found nil))
            (setq pos (cdr pos))))))
    groups))

(defmacro hek-usepkg (pkg-name &rest options)
  "Declare package and the configurations.

PKG-NAME is the name (a symbol) of the package to use. OPTIONS is a list that
contains option names (keywords) and the values (others) like ':opt val1 val2'.
Available keywords are listed in `hek-usepkg-keywords'.

If `hek-usepkg-ensure' is non-nil, flag 'ensure' will be used.
If flag 'no-require' never appears, package will be required at the end.
If flag 'disable' is given, the macro will generate nothing."
  (if (not options)
      `(require ',pkg-name)
    (let* ((flags (if hek-usepkg-ensure (list 'ensure) nil))
           (code-list (list 'progn))
           (code-list-last code-list))
      (setq options (hek-usepkg--group-opts pkg-name options))
      (when hek-usepkg-debug
        (dolist (opt options)
          (unless (assq (car opt) hek-usepkg-keywords)
            (hek-usepkg--error pkg-name "top" "unrecognized keyword" (car opt)))))
      (dolist (kw&h hek-usepkg-keywords)
        (when-let ((opt&val (assq (car kw&h) options)))
          (let ((code&flags (funcall (cdr kw&h) pkg-name (cdr opt&val) flags)))
            (when-let ((new-code (car code&flags)))
              (setq new-code (if (eq (car new-code) 'progn)
                               (cdr new-code)
                               (cons new-code nil)))
              (unless (null new-code)
                (nconc code-list-last new-code)
                (setq code-list-last new-code)))
            (dolist (new-flag (cdr code&flags))
              (unless (memq new-flag flags)
                (push new-flag flags))))))
      (unless (memq 'no-require flags)
        (nconc code-list-last (cons `(require ',pkg-name) nil)))
      (unless (memq 'disable flags)
        code-list))))

(put 'hek-usepkg 'lisp-indent-function 'defun)

(defun hek-usepkg-doc (&optional buffer switch-buffer)
  "Get documentations of `hek-usepkg', `hek-usepkg-keywords', and
`hek-usepkg-sources'. Optional argument BUFFER can be nil (return doc string),
t (use a new buffer) or a buffer object or name (use this buffer)."
  (interactive '(t t))
  (let ((buf-obj (if (or (null buffer) (eq buffer t))
                   (generate-new-buffer "*hek-usepkg-doc*")
                   buffer))
        (ret-val))
    (with-current-buffer buf-obj
      (insert (propertize "* hek-usepkg" 'face '(bold underline))
              "\n\n"
              (documentation 'hek-usepkg)
              "\n\n")
      (insert (propertize "* hek-usepkg-keywords" 'face '(bold underline))
              "\n\n"
              (documentation-property 'hek-usepkg-keywords
                                      'variable-documentation)
              "\n\n")
      (dolist (item hek-usepkg-keywords)
        (insert (propertize (symbol-name (car item)) 'face '(bold italic))
                "\n"
                (documentation (cdr item))
                "\n\n"))
      (insert (propertize "* hek-usepkg-sources" 'face '(bold underline))
              "\n\n"
              (documentation-property 'hek-usepkg-sources
                                      'variable-documentation)
              "\n\n")
      (dolist (item hek-usepkg-sources)
        (insert (propertize (symbol-name (car item)) 'face '(bold italic))
                "\n"
                (if (cdr item) (documentation (cdr item)) "Ignored.")
                "\n\n"))
      (help-mode)
      (setq ret-val (if (null buffer) (buffer-string) buf-obj)))
    (if (null buffer)
        (kill-buffer buf-obj)
      (when switch-buffer
        (switch-to-buffer buf-obj)))
    ret-val))

;; ===== Keywords ======

(defun hek-usepkg--flag (_pkg-name args _flags)
  "Use flags. Syntax: ':flags FLAG1 FLAG2 ...'."
  (cons nil args))

(defun hek-usepkg--preface (_pkg-name args _flags)
  "Code before `:when'."
  (cons (cons 'progn args) nil))

(defun hek-usepkg--when (pkg-name args _flags)
  "Conditionally enable the package. Syntax: ':when SPEC'."
  (when hek-usepkg-debug
    (unless (and (consp args) (null (cdr args)))
      (hek-usepkg--error pkg-name :when "bad syntax" args)))
  (unless (eval (car args) t)
    (cons nil '(disable))))

(defun hek-usepkg--from (pkg-name args flags)
  "Specify package source. Syntax: ':from SRC [ARGS...]'.
Available sources are listed in `hek-usepkg-sources'."
  (when hek-usepkg-debug
    (unless (and (consp args) (symbolp (car args)))
      (hek-usepkg--error pkg-name :from "bad syntax" args)))
  (let ((src-name (car args))
        (src-args (cdr args)))
    (if-let ((src&h (assq src-name hek-usepkg-sources)))
        (cons (when-let ((handler (cdr src&h)))
                (funcall handler pkg-name src-args (memq 'ensure flags)))
              nil)
      (hek-usepkg--error pkg-name :from "unrecognized package source" src-name))))

(defun hek-usepkg--defer (pkg-name args _flags)
  "Defer package loading. Syntax: ':defer t' or ':defer TIME'.
Argument t means do not load package here. This is unnecessary when some other
keywords like ':after', ':hook', ':bind', etc. are specified.
Argument TIME is a non negative integer which force loading the package after
TIME seconds of idle time."
  (when hek-usepkg-debug
    (unless (null (cdr args))
      (hek-usepkg--error pkg-name :defer "bad syntax" args)))
  (cons (let ((x (car args)))
          (if (integerp x)
              (if (>= x 0)
                  `(run-with-idle-timer ,x nil #'require ',pkg-name)
                (hek-usepkg--error pkg-name :defer "illegal defer time" x))
            (if (eq x t)
                nil
              (hek-usepkg--error pkg-name :after "bad syntax" args))))
        '(no-require)))

(defun hek-usepkg--after (pkg-name args flags)
  "Load package after another. Syntax: ':after PKG'."
  (when hek-usepkg-debug
    (unless (and (consp args) (symbolp (car args)) (null (cdr args)))
      (hek-usepkg--error pkg-name :after "bad syntax" args)))
  (unless (memq 'no-require flags)
    (cons `(eval-after-load ',(car args) '(require ',pkg-name))
          '(no-require))))

(defun hek-usepkg--init (_pkg-name args _flags)
  "Code before package loaded."
  (cons (cons 'progn args) nil))

(defun hek-usepkg--hook (_pkg-name args _flags)
  "Add hooks. Syntax: ':hook (HOOK . FUNC)' or ':hook ((HOOK1 HOOK2 ...) . FUNC)'."
  (let ((add-hooks))
    (dolist (x args)
      (let ((hook (car x))
            (func (cdr x)))
        (if (listp hook)
            (dolist (hook1 hook)
              (push `(add-hook ',hook1 ',func) add-hooks))
          (push `(add-hook ',hook ',func) add-hooks))))
    (cons (cons 'progn add-hooks) '(no-require))))

(defun hek-usepkg--bind (_pkg-name args _flags)
  "Add key bindings. Syntax: ':bind ([MAP] (KEY1 . CMD1) (KEY2 . CMD2) ...)'."
  (let ((def-keys))
    (dolist (x args)
      (let ((map))
        (if (symbolp (car x))
            (setq map (car x)
                  x (cdr x))
          (setq map '(current-global-map)))
        (push `(let ((__map ,map))
                 ,@(mapcar (lambda (k&c)
                             (let ((key (car k&c))
                                   (cmd (cdr k&c)))
                               (unless (vectorp key)
                                 (setq key (kbd key)))
                               `(define-key __map ,key #',cmd)))
                           x))
              def-keys)))
    (cons (cons 'progn def-keys) '(no-require))))

(defun hek-usepkg--bind~ (pkg-name args flags)
  "Add key bindings like `:bind', but the bindings always happens after
the package is loaded."
  (let ((binding-code (car (hek-usepkg--bind pkg-name args flags))))
    (cons (if (memq 'no-require flags)
              `(eval-after-load ',pkg-name ',binding-code)
            binding-code)
          nil)))

(defun hek-usepkg--config (pkg-name args flags)
  "Code after package loaded.
If flag 'no-require' is not given, package will be loaded; otherwise, the code
will be evaluated after package loaded."
  (if (memq 'no-require flags)
      (cons `(eval-after-load ',pkg-name '(progn ,@args))
            nil)
    (cons `(progn (require ',pkg-name) ,@args)
          '(no-require))))

;; List keywords.
(setq hek-usepkg-keywords
      '((:flag . hek-usepkg--flag)
        (:preface . hek-usepkg--preface)
        (:when . hek-usepkg--when)
        (:from . hek-usepkg--from)
        (:defer . hek-usepkg--defer)
        (:after . hek-usepkg--after)
        (:init . hek-usepkg--init)
        (:hook . hek-usepkg--hook)
        (:bind . hek-usepkg--bind)
        (:bind~ . hek-usepkg--bind~)
        (:config . hek-usepkg--config)))

;; ====== Sources ======

(defun hek-usepkg--from-package (pkg-name _args ensure)
  "From `package.el'. ARGS are unused."
  (when ensure
    `(unless (package-installed-p ',pkg-name)
       (package-install ',pkg-name))))

(defun hek-usepkg--from-local (_pkg-name args _ensure)
  "Local package. Optional ARGS is the load path."
  (when args
    `(eval-and-compile (add-to-list 'load-path ,(car args)))))

;; List package sources.
(setq hek-usepkg-sources
      '((package . hek-usepkg--from-package)
        (local . hek-usepkg--from-local)
        (builtin . nil)))

(provide 'hek-usepkg)
;;; hek-usepkg.el ends here
