#!/bin/bash
set -o pipefail

# This runs spell checking on all projects in A2.
# For the front-end projects it checks these file types: .ts, .tsx, .html, .md.
# For all other projects it checks these file types: .go, .rb, .proto, .md.
# It also checks the docs and dev-docs directories for .md files.
#
# This script uses the cspell engine (https://github.com/Jason3S/cspell).
# Besides the standard dictionaries used by cspell,
# customized A2 configuration and word list are in ./cspell.json.
# The word list in cspell.json should be updated as necessary with valid but unrecognized words.

# Typical usage:
# ./scripts/spellcheck > a2_spelling_issues.txt
# spellcheck has two modes:
# To see exact files/line numbers where errors are found, run with no arguments.
# To get a histogram of misspellings throughout A2, add the "--wordsOnly" argument.

# Besides the command line version of cspell, you can install a VSCode plugin
# that uses the very same configuration file ("Code Spell Checker" from
# https://marketplace.visualstudio.com/items?itemName=streetsidesoftware.code-spell-checker)
# With the plugin, spelling errors appear in the Problems pane for all open files.

# Note that one must exercise care when checking the front-end projects (or any portion of the file tree
# with a huge number of files to be ignored). The 2 front-end projects contain node_modules subdirectories;
# of the 235,000 files (!) in an A2 working directory, node_modules accounts for fully 230,000 of those!
# Even though the node_modules path is ignored by the config file, the path is still exhaustively traversed
# rather than just pruning the path in the bud, so cspell would take a verrrryyyy long time if it sees them.
# The workaround is not to include them at all in the path spec given to cspell.


command -v cspell >/dev/null 2>&1 || { echo >&2 "Cspell required; install with 'npm install -g cspell'.  Aborting."; exit 2; }
test -d components && test -d scripts || { echo >&2 "Must start in a2 root directory.  Aborting."; exit 2; }
test -f cspell.json || { echo >&2 "Oh dear, oh dear: config file (cspell.json) is missing.  Aborting."; exit 2; }

# OPTS are options that will be passed to cspell
OPTS=""
# CHECK_GLOBAL_DOCS controls whether we check the dev-docs and
# automate-chef-io content folders. We set this to true unless the
# user specifies directories.
CHECK_GLOBAL_DOCS="false"
# Directories to scan for spelling errors. If the user does not
# provide this, a default set is chosen below.
DIRS=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --wordsOnly)
            OPTS="${CSPELL_OPTS} --wordsOnly"
            shift
            ;;
        *)
            DIRS+=("$1")
            shift
            ;;
    esac
done

if [[ ${#DIRS[@]} -eq 0 ]]; then
    DIRS=(components/* lib/ api/)
    CHECK_GLOBAL_DOCS="true"
fi

function checkFiles {
    RETVAL=0
    for dir in "${DIRS[@]}"
    do
        if [[ -d $dir/node_modules ]]; then
            >&2 echo -n "$dir [ts and tsx]... "
            cspell $OPTS "$dir/src/**/*.ts*" "$dir/e2e/**/*.ts*" || RETVAL=1
            >&2 echo -n "$dir [sh]... "
            cspell $OPTS "$dir/src/**/*.sh" || RETVAL=1
            >&2 echo -n "$dir [md]... "
            cspell $OPTS "$dir/src/**/*.md" "$dir/docs/**/*.md" README.md || RETVAL=1

            # automate-ui is not ignoring src/assets/dist.html like it should
            # when specifying src/**/*.html so do this instead:
            if [[ "$dir" == "components/automate-ui" ]]; then
                >&2 echo -n "$dir [html]... "
                cspell $OPTS "$dir/src/app/**/*.html" || RETVAL=1
            else
                >&2 echo -n "$dir [html]... "
                cspell $OPTS "$dir/src/**/*.html" || RETVAL=1
            fi
        else
            >&2 echo -n "$dir [go]... "
            cspell $OPTS "$dir/**/*.go" || RETVAL=1
            >&2 echo -n "$dir [rb]... "
            cspell $OPTS "$dir/**/*.rb" || RETVAL=1
            >&2 echo -n "$dir [md]... "
            cspell $OPTS "$dir/**/*.md" || RETVAL=1
            >&2 echo -n "$dir [proto]... "
            cspell $OPTS "$dir/**/*.proto" || RETVAL=1
            >&2 echo -n "$dir [sh]... "
            cspell $OPTS "$dir/**/*.sh" || RETVAL=1
        fi
    done

    if [[ "$CHECK_GLOBAL_DOCS" = "true" ]]; then
        >&2 echo -n "global docs [md]... "
        cspell $OPTS "dev-docs/**/*.md" "components/automate-chef-io/content/docs/**/*.md" || RETVAL=1
    fi
    [[ $RETVAL -eq 0 ]] || exit $RETVAL
}

checkFiles |
    sort --ignore-case |
    uniq -ci |
    sort --numeric-sort
# return result of checkFiles due to pipefail above
exit $?
