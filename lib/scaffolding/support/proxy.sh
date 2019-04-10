# appendToStringIfMissing takes
#
#  arg1: a string representing some list
#  arg2: a string you want to add to the list
#  arg3: the separator for the list
#
# and prints a new string with arg2 appended to arg1 if it wasn't
# already contained in arg1. If the string you want to append contains
# the separator, reconsidering whether shell is the right language for
# this task.
appendToStringIfMissing() {
    string="$1"
    new_item="$2"
    sep="$3"

    case "$string" in
        "")
            echo "$new_item"
            ;;
        "$new_item$sep"*|*"$sep$new_item"|*"$sep$new_item$sep"*)
            echo "$string"
            ;;
        *)
            echo "$string$sep$new_item"
    esac
}

# addNoProxy adds the given argument to
# the no_proxy environment variable.
addNoProxy() {
    export no_proxy="$(appendToStringIfMissing "$no_proxy" "$1" ",")"
}
