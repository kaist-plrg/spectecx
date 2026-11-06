Stage commands: elab prints the IL of a spec, and algo prints its AL.

  $ cat > flip.spectec <<'EOF'
  > dec $flip(bool) : bool
  > def $flip(true) = false
  > def $flip(false) = true
  > EOF

  $ spectec elab flip.spectec
  ;; flip.spectec:1:1-1:23:
  def $flip(bool) : bool =
  
     ;; flip.spectec:2:1-2:24:
     clause 0(true) = false
  
     ;; flip.spectec:3:1-3:24:
     clause 1(false) = true

  $ spectec algo flip.spectec
  ;; flip.spectec:1:1-1:23:
  def $flip(bool) : bool =
  
     ;; flip.spectec:2:1-2:24:
     clause 0(bool) = false
        -- if (bool = true)
  
     ;; flip.spectec:3:1-3:24:
     clause 1(bool) = true
        -- if (bool = false)
