const RED     = "\x1b[31m"
const GREEN   = "\x1b[32m"
const BOLD    = "\x1b[1m"
const DEFAULT = "\x1b[0m"

colored(s::String, color) = string(color, s, DEFAULT)

red(s::String)   = colored(s, RED)
green(s::String) = colored(s, GREEN)
bold(s::String)  = colored(s, BOLD)
