const RED     = "\x1b[31m"
const GREEN   = "\x1b[32m"
const BOLD    = "\x1b[1m"
const DEFAULT = "\x1b[0m"

colored(s::AbstractString, color) = string(color, s, DEFAULT)

red(s::AbstractString)   = colored(s, RED)
green(s::AbstractString) = colored(s, GREEN)
bold(s::AbstractString)  = colored(s, BOLD)
