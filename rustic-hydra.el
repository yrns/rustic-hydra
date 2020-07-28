(require 'dash)
(require 'ivy)
(require 'counsel)
(require 'hydra)
(require 'rustic-cargo)

(defun rustic-hydra--nth (n list)
  "Return the nth value, or the first if nil."
  (or (nth n list) (car list)))

(defmacro rustic-hydra--cycle (var list)
  "Cycle var with the next value from list."
  `(let* ((l ,list)
          (n (or (-elem-index ,var l) -1)))
     (setq ,var (rustic-hydra--nth (+ n 1) l))))

(defvar rustic-hydra-last nil)

(defun rustic-hydra-run (a)
  (setq rustic-hydra-last a)
  (rustic-run-cargo-command a))

(defun rustic-hydra-rerun-last ()
  (interactive)
  (rustic-run-cargo-command rustic-hydra-last))

(defun rustic-hydra--manifest ()
  (let ((json-array-type 'list))
    (json-read-from-string
     (shell-command-to-string "cargo read-manifest"))))

(defun rustic-hydra--deps ()
  (-map (lambda (a) (cdr (assoc 'name a)))
        (cdr (assoc 'dependencies (rustic-hydra--manifest)))))

(defun rustic-hydra--targets ()
  (cdr (assoc 'targets (rustic-hydra--manifest))))

(defun rustic-hydra--examples ()
  (-map (lambda (a) (cdr (assoc 'name a)))
        (-filter (lambda (a) (string= "example" (car (cdr (car a)))))
                 (rustic-hydra--targets))))

(defun rustic-hydra--bins ()
  (-map (lambda (a) (cdr (assoc 'name a)))
        (-filter (lambda (a) (string= "bin" (car (cdr (car a)))))
                 (rustic-hydra--targets))))

(defun rustic-hydra-run-example ()
  "Run example."
  (interactive)
  (ivy-read "Run example: "
            (rustic-hydra--examples)
            :history 'rustic-hydra-run-example-history
            :require-match t
            :action (lambda (x)
                      (rustic-hydra-run
                       (format "cargo run --example %s" (intern x))))
            :caller 'rustic-hydra-run-example))

(defun rustic-hydra-run-bin ()
  "Run bin."
  (interactive)
  (ivy-read "Run bin: "
            (rustic-hydra--bins)
            :history 'rustic-hydra-run-bin-history
            :require-match t
            :action (lambda (x)
                      (rustic-hydra-run
                       (format "cargo run --bin %s" (intern x))))
            :caller 'rustic-hydra-run-bin))

(defun rustic-hydra-add-fn (input)
  "Search crates."
  (or
   (ivy-more-chars)
   (progn
     (counsel--async-command
      (format "cargo search %s" input))
     '("" "working..."))))

(defun rustic-hydra-add (&optional initial-input)
  "Add a dependency."
  (interactive)
  (ivy-read "Add: " #'rustic-hydra-add-fn
            :initial-input initial-input
            :dynamic-collection t
            :history 'rustic-hydra-add-history
            :action (lambda (a)
                      (rustic-run-cargo-command (format "cargo add %s" (car (split-string a)))))
            :caller 'rustic-hydra-add))

(defun rustic-hydra-rm ()
  "Remove a dependency."
  (interactive)
  (ivy-read "Remove: "
            (rustic-hydra--deps)
            :history 'rustic-hydra-rm-history
            :require-match t
            :action (lambda (x)
                      (rustic-run-cargo-command
                       (format "cargo rm %s" (intern x))))
            :caller 'rustic-hydra-rm))

(defhydra rustic-hydra (:color blue :hint nil :pre rustic-hydra--pre)
  "
_a_: backtrace: %`rustic-compile-backtrace
_l_: run last: %`rustic-hydra-last

_r_: run            _f_: fmt         _k_: check    _+_: add   _p_: rustic popup
_n_: run bin        _c_: clippy      _t_: test     _-_: rm    _q_: cancel
_x_: run example    _o_: outdated    _d_: doc
_b_: build          _e_: clean
"
  ("p" rustic-popup)
  ("b" rustic-cargo-build)
  ("f" rustic-cargo-fmt)
  ("a" (rustic-hydra--cycle rustic-compile-backtrace '("0" "1" "full")) :color pink)
  ("r" (rustic-hydra-run "cargo run"))
  ("x" rustic-hydra-run-example)
  ("n" rustic-hydra-run-bin)
  ("l" rustic-hydra-rerun-last)
  ("c" rustic-cargo-clippy)
  ("o" rustic-cargo-outdated)
  ("e" rustic-cargo-clean)
  ("k" rustic-cargo-check)
  ("t" rustic-cargo-test)
  ("d" rustic-cargo-doc)
  ("+" rustic-hydra-add)
  ("-" rustic-hydra-rm)
  ("q" nil))

(defun rustic-hydra--pre ()
  "Define whatever key is used for the hydra body to rerun last."
  (define-key rustic-hydra/keymap
    (where-is-internal 'rustic-hydra/body rustic-mode-map t) 'rustic-hydra-rerun-last))

(provide 'rustic-hydra)
