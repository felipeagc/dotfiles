" Vim filetype plugin

if exists('b:did_fsharp_ftplugin')
    finish
endif
let b:did_fsharp_ftplugin = 1

let s:cpo_save = &cpo
set cpo&vim

" enable syntax based folding
setlocal fdm=syntax

" make ftplugin undo-able
let b:undo_ftplugin = 'setlocal fo< cms< com< fdm<'

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=4 et sts=4
