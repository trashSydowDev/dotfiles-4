function! CabalInsertDependencyVersion(str)
  let l:version = system('cabal-version ' . a:str)
  execute 'normal! i' . l:version
endfunction
