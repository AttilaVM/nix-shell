(defvar nix-shell-env-alist nil
	"Storage for variables from loaded nix environments.")
(defvar nix-shell-env-name-list nil
	"List of loaded nix environment names.")

;; Save user environment
(setq nix-shell-env-alist (acons "user-env"
			 (acons "PATH" (getenv "PATH") nil)
			 nix-shell-env-alist))
(setq nix-shell-env-name-list '("user-env"))

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
			;; Save env
			(setq nix-shell-env-alist
						(acons env-name
									 (acons "PATH" path-value nil)
									 nix-shell-env-alist))
			(setq nix-shell-env-name-list
						(add-to-list 'nix-shell-env-name-list env-name))
			;; activate env TODO separate
			(setenv "PATH" path-value)
			(setq exec-path (s-split ":" path-value)))

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

(defun nix-shell-activate-env (candidate)
	"Activate nix-environment, identitified by CANDIDATE."
	(let* ((env-definition (assoc candidate nix-shell-env-alist))
				 (path (a-get "PATH" env-definition)))
		(setenv "PATH" path)
		(setq exec-path (s-split ":" path))))

(defun nix-shell-helm-source ()
	""
	(helm-build-sync-source "Nix Envs"
		:candidates
		(lambda () (copy-sequence nix-shell-env-name-list))
		:candidate-number-limit 999
		:nomark t
		:action '(("Nix Envs" . nix-shell-activate-env))))

(defun nix-shell-choose-env ()
	(interactive)
	(helm :sources (nix-shell-helm-source)
				:buffer "*helm my command*"))
