;; The file passed on the command line does not exist. The diagnostic is
;; emitted from the IO layer, so it has no code prefix.
  $ ./main.exe missing-file.spectec 2>&1
  error: missing-file.spectec: No such file or directory
    --> missing-file.spectec
    |
    | source: io
  [1]
