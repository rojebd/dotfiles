# Hook so that way the Shift-d keybind only for hare files
hook global WinSetOption filetype=hare %{
    # map global normal <s-d> "<a-i>c(^|[^\w:]),[^\w:]<ret>"	
    map global normal <s-d> "<a-i>c(^|[^\w:]),[^\w:]<ret>:haredoc-get<ret>"	
    define-command haredoc-get -docstring "Call haredoc with current selection" %{
    	evaluate-commands %sh{
    		line=$kak_cursor_line
    		char=$kak_cursor_char_column
    		selected=$kak_selection
    		haredoc_info=$(export NO_COLOR="N"; haredoc $selected)
    		# printf "info -title '$selected' -style modal -anchor $line.$char '$haredoc_info'"
    		printf "info -title '$selected' -style below -anchor $line.$char '$haredoc_info'"
    	}
    }
}

