function fish_prompt
	set_color cyan
	echo -sn \[
	set_color --bold white
	echo -sn $USER@(hostname) ' '
	set_color normal
	echo -sn (prompt_pwd)
	set_color cyan
	echo -sn \]
	set_color green
	echo -sn '$ '
	set_color normal
end
