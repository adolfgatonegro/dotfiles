

  
  if &background == 'dark'
    
  let s:shade0 = "#0a0a15"
  let s:shade1 = "#202029"
  let s:shade2 = "#35353d"
  let s:shade3 = "#4b4b51"
  let s:shade4 = "#606065"
  let s:shade5 = "#767679"
  let s:shade6 = "#8b8b8d"
  let s:shade7 = "#a1a1a1"
  let s:accent0 = "#ff00aa"
  let s:accent1 = "#ff74d2"
  let s:accent2 = "#ff8700"
  let s:accent3 = "#aaee00"
  let s:accent4 = "#00ffed"
  let s:accent5 = "#0077ff"
  let s:accent6 = "#00ffed"
  let s:accent7 = "#bb00ff"
  
  endif
  

  
  if &background == 'light'
    
  let s:shade0 = "#a1a1a1"
  let s:shade1 = "#8b8b8e"
  let s:shade2 = "#76767a"
  let s:shade3 = "#606067"
  let s:shade4 = "#4b4b54"
  let s:shade5 = "#353541"
  let s:shade6 = "#20202d"
  let s:shade7 = "#0a0a1a"
  let s:accent0 = "#f03e4d"
  let s:accent1 = "#f37735"
  let s:accent2 = "#eeba21"
  let s:accent3 = "#97bd2d"
  let s:accent4 = "#1fc598"
  let s:accent5 = "#53a6e1"
  let s:accent6 = "#bf65f0"
  let s:accent7 = "#ee4eb8"
  
  endif
  

  let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
  let s:p.normal.left = [ [ s:shade1, s:accent5 ], [ s:shade7, s:shade2 ] ]
  let s:p.normal.right = [ [ s:shade1, s:shade4 ], [ s:shade5, s:shade2 ] ]
  let s:p.inactive.right = [ [ s:shade1, s:shade3 ], [ s:shade3, s:shade1 ] ]
  let s:p.inactive.left =  [ [ s:shade4, s:shade1 ], [ s:shade3, s:shade0 ] ]
  let s:p.insert.left = [ [ s:shade1, s:accent3 ], [ s:shade7, s:shade2 ] ]
  let s:p.replace.left = [ [ s:shade1, s:accent1 ], [ s:shade7, s:shade2 ] ]
  let s:p.visual.left = [ [ s:shade1, s:accent6 ], [ s:shade7, s:shade2 ] ]
  let s:p.normal.middle = [ [ s:shade5, s:shade1 ] ]
  let s:p.inactive.middle = [ [ s:shade4, s:shade1 ] ]
  let s:p.tabline.left = [ [ s:shade6, s:shade2 ] ]
  let s:p.tabline.tabsel = [ [ s:shade6, s:shade0 ] ]
  let s:p.tabline.middle = [ [ s:shade2, s:shade4 ] ]
  let s:p.tabline.right = copy(s:p.normal.right)
  let s:p.normal.error = [ [ s:accent0, s:shade0 ] ]
  let s:p.normal.warning = [ [ s:accent2, s:shade1 ] ]

  let g:lightline#colorscheme#NeonGatoLightline#palette = lightline#colorscheme#fill(s:p)

  
