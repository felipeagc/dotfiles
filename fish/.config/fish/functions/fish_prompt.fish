function fish_prompt
	set_color cyan
	echo -sn (prompt_pwd)
	set_color magenta
	echo -sn (__fish_git_prompt)
	set_color green
	echo -sn ' $ '
	set_color normal
end
