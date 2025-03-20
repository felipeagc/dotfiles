if exists("b:current_syntax")
  finish
endif

let b:current_syntax = "budget"

" Match language specific keywords
syn keyword budgetKeyword
            \ to from note category id
			\ month importer account

syn match budgetComment "\v;.*$"

syn match budgetNumber display "\<[0-9]\+\(\.[0-9]\+\)\=\>"

syn match budgetIdentifier /[a-zA-Z][a-zA-Z0-9-_\/]*/

syn match budgetDate /\<\d\d\d\d-\d\d\>/
syn match budgetDate /\<\d\d\d\d-\d\d-\d\d\>/

syn match budgetEscapeError display contained /\\./
syn match budgetEscape      display contained /\\\([nrt\\'"]\|x\x\{2}\)/

syn region budgetString start=+c\?"+ skip=+\\\\\|\\"+ end=+"+ oneline contains=budgetEscape,budgetEscapeError,@Spell

syn region budgetBlock start="{" end="}" transparent fold

hi def link budgetComment Comment
hi def link budgetString String
hi def link budgetKeyword Keyword
hi def link budgetNumber Number
hi def link budgetEscape Special
hi def link budgetEscapeError Error
hi def link budgetIdentifier Identifier
hi def link budgetDate Function
