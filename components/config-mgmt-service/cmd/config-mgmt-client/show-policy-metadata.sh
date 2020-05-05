#!/bin/bash

## Policyfile Metadata Collector (Prototype)
# Gets the metadata about a policyfile we want to send to automate whenever we
# push policy code to the chef server. We collect the following kinds of things:
# * policy name, group and the chef server: this set of three things identifies
#   nodes that get the same code at the same time. This can extend to
#   effortless if we use depot and depot channel subscription instead of chef
#   server and policy group
# * user-provided description: can be whatever.
# * git commit/commit message: provides conceptual link to git repo so the user
#   can look in git for answers to questions about the code. May be used to add
#   hyperlinks (e.g., to user's github) in Automate UI
# * Ci metadata: if policy is being tested/pushed by an automation pipeline,
#   link to that.

# TODO STILL: chef server url, chef server username, policy group


function verify_args() {
  local OPTIND opt 
  
  while getopts 'hd:c:u:' opt; do
    case $opt in
      h) print_usage; exit 1
      ;;
      d) description="$OPTARG"
      ;;
      c) ci_job="$OPTARG"
      ;;
      u) ci_job_url="$OPTARG"
      ;;
      \?) echo "Invalid option -$OPTARG" >&2; exit 1
      ;;
      : )
      echo "Invalid option: $OPTARG requires an argument" 1>&2; exit 1
      ;;
    esac

  done

  shift $((OPTIND-1))

  # Args Capturing:
  filename_to_check=$1

  if [ "x$filename_to_check" == "x" ]; then
    print_usage
    exit 1
  fi

}

function print_usage() {
  cat<<EOH
USAGE: show-policy-metadata.sh [opts] POLICYFILE_LOCK

OPTIONS:
  -h               print help
  -d DESCRIPTION   Add a custom description (defaults to last commit message)
  -c CI_JOB_NAME   If using a Ci pipeline to push policy updates,
                   you may include the job name/ID
  -u CI_JOB_URL    If using a Ci pipeline to push policy updates,
                   you may include the job URL

EOH

}

function print_header() {
  echo "== Policy Rollout Metadata (Prototype) =="
  echo "Collecting metadata for $filename_to_check"
}

function get_policy_name() {
  jq -Mc .policy_name "$filename_to_check"
}

function get_revision_id() {
  jq -Mc .revision_id "$filename_to_check"
}

function pushd_git_repo() {
  local dir; dir=$(dirname "$filename_to_check")
  pushd "$dir" > /dev/null || exit 1
}

# Get the last commit that changed the policyfile lock.json file.
# `git rev-list` is "plumbing" so it should be stable for scripting
function get_last_commit_to_lockfile() {
  pushd_git_repo || exit 1
  local file; file=$(basename "$filename_to_check")
  git rev-list -1 HEAD "$file"
  popd > /dev/null || exit 1
}

function get_commit_message() {
  local commit; commit=$(get_last_commit_to_lockfile | tr -d ' ')
  pushd_git_repo || exit 1
  echo -n '"'
  local msg; msg=$(git show -s --format=%B "$commit" | tr -d "\n")
  echo -n "$msg"
  echo -n '"'
  popd > /dev/null || exit 1
}

function get_description() {
  if [ "x$description" == "x" ]; then
    echo -n "(no description given, falling back to last commit message) "
    get_commit_message
  else
    echo "$description"
  fi
}

function get_ci_job() {
  if [ "x$ci_job" == "x" ]; then
    echo "<none>"
  else
    echo "$ci_job"
  fi
}

function get_ci_job_url() {
  if [ "x$ci_job_url" == "x" ]; then
    echo "<none>"
  else
    echo "$ci_job_url"
  fi
}


verify_args "$@" || exit 1
print_header

echo -n "policy_name: "
get_policy_name

echo -n "policy_revision_id: "
get_revision_id

echo -n "last_lockfile_commit: "
echo "\"$(get_last_commit_to_lockfile | tr -d "\n")\""

echo -n "last_lockfile_commit_message: "
get_commit_message
echo ""

echo -n "description: "
echo "\"$(get_description | tr -d "\n")\""

echo -n "ci_job: "
echo "\"$(get_ci_job | tr -d "\n")\""

echo -n "ci_job_url: "
echo "\"$(get_ci_job | tr -d "\n")\""

echo ""
