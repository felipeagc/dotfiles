if exists("b:current_syntax")
  finish
endif

let b:current_syntax = "peanuts"

" Match language specific keywords
syn keyword peanutsKeyword
            \ to from note category id
			\ month importer account balance
            \ import payee

syn match peanutsComment "\v;.*$"

syn match peanutsNumber display '\v[+-]?\d+(\.\d+)?([eE][+-]?\d+)?'

syn match peanutsIdentifier /[a-zA-Z][a-zA-Z0-9-_\/]*/

syn match peanutsDate /\<\d\d\d\d-\d\d\>/
syn match peanutsDate /\<\d\d\d\d-\d\d-\d\d\>/

syn match peanutsEscapeError display contained /\\./
syn match peanutsEscape      display contained /\\\([nrt\\'"]\|x\x\{2}\)/

syn region peanutsString start=+c\?"+ skip=+\\\\\|\\"+ end=+"+ oneline contains=peanutsEscape,peanutsEscapeError,@Spell

syn region peanutsBlock start="{" end="}" transparent fold

hi def link peanutsComment Comment
hi def link peanutsString String
hi def link peanutsKeyword Keyword
hi def link peanutsNumber Number
hi def link peanutsEscape Special
hi def link peanutsEscapeError Error
hi def link peanutsIdentifier Identifier
hi def link peanutsDate Function
