(defvar nix-shell-env-alist)

(setq nix-shell-env-alist nil)
;; Save user environment
(setq nix-shell-env-alist (acons "user-env"
			 (acons "PATH" (getenv "PATH") nil)
			 nix-shell-env-alist))

(defun nix-shell/stack-then-activate-env (out-buff env-name)
	(with-current-buffer out-buff
		(goto-char 0)
		(search-forward-regexp "^PATH='")
		(set-mark (point))
		(search-forward "'")
		(left-char)
		(let ((path-value (buffer-substring
											 (region-beginning)
											 (region-end))))
			(setq nix-shell-env-alist
						(acons env-name
									 (acons "PATH" path-value nil)
									 nix-shell-env-alist))
			(setenv "PATH" path-value)
			(setq exec-path (s-split ":" path-value)))
		;; (switch-to-buffer out-buff)

		))

(defun nix-shell-sentinel (out-buf env-name process event)
	""(message (concat "event" event))
	(unless (process-live-p process)
		(nix-shell/stack-then-activate-env out-buf env-name)))

(defun realize-nix-env (expr-file env-name)
	"Realize a nix environment inside Emacs from a given nix expression file"
	(interactive)
	(let ((err-buff (generate-new-buffer "nix-shell-buffer-err"))
				(out-buff (generate-new-buffer "nix-shell-buffer")))
		(make-process
	 :name "nix-shell"
	 :buffer out-buff
	 :command (list "nix-shell"
									"--command" "set"
									(file-relative-name "~/projects/volumetric-render-frontend/default.nix"))
	 :sentinel (apply-partially 'nix-shell-sentinel
															out-buff
															env-name
															)
	 :noquery t
	 :stderr err-buff)))

(defun nix-shell ()
	"nix-shell shell command inside Emacs to work in nix enironment defined by the closest `default.nix' upward in the file hyearhchy"
	(interactive)
	(let* ((cwd (or (file-name-directory (buffer-file-name))
									default-directory))
				 (expr-dir (locate-dominating-file cwd "default.nix"))
				 (expr-file (f-join expr-dir "default.nix"))
				 (env-name (f-base expr-dir)))
		(realize-nix-env expr-file env-name)))

(defun nix-shell-choose-env ()
	(interactive)
	(helm :sources 'nix-shell-env-alist
				:buffer "*helm my command*"))
