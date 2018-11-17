(setq load-path (append load-path (list ".")))

(require 'weather)

(command-execute 'weather)
