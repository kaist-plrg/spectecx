elab prints the IL of a spec, algo prints the AL of a spec, and annotate prints the PL of a spec.

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

  $ cat > lookup.spectec <<'EOF'
  > syntax pair<K, V> = K -> V
  > syntax map<K, V> = pair<K, V>*
  > dec $lookup<K, V>(map<K, V>, K) : V?
  > def $lookup<K, V>(eps, K_query) = eps
  > def $lookup<K, V>((K_h -> V_h) :: (K_t -> V_t)*, K_query) = V_h
  >   -- if K_h = K_query
  > def $lookup<K, V>((K_h -> V_h) :: (K_t -> V_t)*, K_query)
  >   = $lookup<K, V>((K_t -> V_t)*, K_query)
  >   -- otherwise
  > EOF

  $ spectec annotate lookup.spectec
  syntax pair<K, V> = 
     | K -> V (from pair<K, V>)
  
  syntax map<K, V> = pair<K, V>*
  
  def $lookup<K, V>(pair<K, V>*{pair<K, V> <- pair<K, V>*}, K_query)
  
  1. Case analysis on pair<K, V>*{pair<K, V> <- pair<K, V>*}
  
    1. Case (% matches [])
  
      1. Return ?()
  
    2. Case (let K_h -> V_h :: K_t -> V_t*{K_t <- K_t*, V_t <- V_t*} = % matches _ :: _)
  
      1. If ((K_h = K_query)), then
  
        1. Return ?(V_h)
  
  2. Otherwise,
  
    1. Otherwise
  
      1. If (pair<K, V>*{pair<K, V> <- pair<K, V>*} matches _ :: _), then
  
        1. (Let K_h -> V_h :: K_t -> V_t*{K_t <- K_t*, V_t <- V_t*} be pair<K, V>*{pair<K, V> <- pair<K, V>*})
  
        2. Return $lookup<K, V>(K_t -> V_t*{K_t <- K_t*, V_t <- V_t*}, K_query)
