# gruber-dark theme

evaluate-commands %sh{
	fg0="rgb:e4e4ef"
    fg1="rgb:f4f4ff"
    fg2="rgb:f5f5f5"
    fg3="rgb:a89984"
    bg0="rgb:181818"
    bg1="rgb:282828"
    bg2="rgb:453d41"
    bg4="rgb:52494e"
    bg5="rgb:404040"
    bg6="rgb:232323"
    bg7="rgb:3f3f3f"
    bg8="rgb:2c2c2c"
    red0="rgb:f43841"
    red1="rgb:ff4f58"
    red2="rgb:2B0A0B"
    red3="rgb:fb4934"
    green0="rgb:73c936"
    green1="rgb:b8bb26"
    yellow0="rgb:ffdd33"
    yellow1="rgb:655814"
    blue0="rgb:5292c8"
    orange0="rgb:d65d0e"
    orange1="rgb:fe8019"
    brown0="rgb:cc8c3c"
    quartz="rgb:95a99f"
    niagara0="rgb:96a6c8"
    niagara1="rgb:303540"
    wisteria="rgb:9e95c7"
    aqua1="rgb:8ec07c"

    echo "
        # Code highlighting
        face global value         ${fg0}
        face global type          ${quartz}
        face global variable      ${fg0}
        face global module        ${aqua1}
        face global function      ${niagara0}
        face global string        ${green0}

        face global keyword       ${yellow0}+b

        face global operator      ${fg0}
        face global attribute     ${fg0}
        face global comment       ${brown0}
        face global documentation comment

        face global meta          ${yellow0}+b
        face global builtin       ${yellow0}+b

        # Markdown highlighting
        face global title     ${aqua1}+b  
        face global header    ${aqua1}    
        face global mono      ${fg0}      
        face global block     ${fg0}      
        face global link      ${green1}+u 
        face global bullet    ${fg0}      
        face global list      ${fg0}      

        face global Default            ${fg0},${bg0} 
        face global PrimarySelection   ${fg0},${bg5}+g 
        face global SecondarySelection ${bg2},${fg0}+g 
        face global PrimaryCursor      ${bg4},${yellow0}+fg 
        face global SecondaryCursor    ${yellow1},${fg0}+fg 
        face global PrimaryCursorEol   ${niagara1},${yellow0}+fg 
        face global SecondaryCursorEol ${yellow1},${fg0}+fg 
        face global LineNumbers        ${bg4} 
        face global LineNumberCursor   ${yellow0},${bg1} 
        face global LineNumbersWrapped ${bg4} 
        face global MenuForeground     ${bg0},${fg0} 
        face global MenuBackground     ${fg0},${bg2} 
        face global MenuInfo           ${bg0} 
        face global Information        ${fg0},${bg1} 
        face global Error              ${bg1},${red0} 
        face global DiagnosticError    ${red0} 
        face global DiagnosticWarning  ${yellow0} 
        face global StatusLine         ${fg0},${bg1} 
        # face global StatusLineMode     ${yellow0}+b 
        face global StatusLineMode     ${yellow0}
        face global StatusLineInfo     ${yellow0} 
        face global StatusLineValue    ${yellow0} 
        face global StatusCursor       ${bg0},${fg0} 
        face global Prompt             ${fg0} 
        face global MatchingChar       ${fg0},${bg1}+b 
        face global BufferPadding      ${bg2},${bg0}
        face global Whitespace         ${bg1}+f 
    "
}
