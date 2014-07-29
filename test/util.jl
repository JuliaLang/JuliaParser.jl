RED     = "\x1b[31m"
GREEN   = "\x1b[32m"
BOLD    = "\x1b[1m"
DEFAULT = "\x1b[0m"

colored(s::String, color) = string(color, s, DEFAULT)

red(s::String)   = colored(s, RED)
green(s::String) = colored(s, GREEN)
bold(s::String)  = colored(s, BOLD) 
