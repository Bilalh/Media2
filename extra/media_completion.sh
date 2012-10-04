function _histalist(){
    sqlite3 "/Users/bilalh/Library/Application Support/Media/Media.db" 'Select Series from OngoingSeries'
}

function _hista_comp(){
	local curw
	COMPREPLY=()
	curw=${COMP_WORDS[COMP_CWORD]}

	case $COMP_CWORD in

	1)	IFS=$'\x0a';
		COMPREPLY=($(compgen -W '`_histalist`' -- $curw))
		unset IFS;
		;;
	2)	IFS=$'\x0a';
		COMPREPLY=($(compgen -W '`media_get_num.rb ${COMP_WORDS[*]}`' -- $curw))
		unset IFS
		;;
	esac

	return 0
}

shopt -s progcomp
complete -F _hista_comp histb