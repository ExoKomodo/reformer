(if (current-filename)
    (add-to-load-path (dirname (current-filename)))
    (add-to-load-path (string-append (getcwd) "/src")))

(use-modules (reformer))

(cfrr)

