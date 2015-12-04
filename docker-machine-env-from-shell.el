(defun docker-machine-env-from-shell ()
  "Loads the docker environment from the shell"
  (interactive)
  (mapcar (lambda (env) (apply 'setenv env))
          ;; This should be taken as an argument but works on my current
          ;; configurations
          (docker-machine-env "docker-vm")))

(defun docker-machine-env (machine)
  "Reads the docker environment from the shell returns it as a list of pairs"
  (defun docker-machine-env-impl (machine)
    (let ((cmd (format (concat "docker-machine env %s |"
                               "grep export | ggrep -Po 'export \\K(.+)' |"
                               "sed 's/=/ /' | sed 's/\"//g'")
                       machine)))
      (shell-command-to-string cmd)))
  (mapcar (lambda (s) (split-string s " "))
          (butlast (split-string (docker-machine-env-impl machine)
                                 "\n"))))
