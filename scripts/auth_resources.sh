#!/bin/bash

###
### auth_resources.sh -- adds auth utilities to your shell
###
### usage: source auth_resources.sh
###
### Commands added:
###
###   a2policies <local_user_name>
###     Enumerates all policies for <local_user_name>, including direct mention,
###     or indirect through local team membership or wildcard mention.
###
###     EXAMPLE: a2policies bob
###     If user:local:bob is a member of team:local:foo, then policies with
###     any of these members will be returned:
###       user:local:bob
###       user:local:*
###       user:*
###       team:local:foo
###       team:local:*
###       team:*
###       *
###
###   a2teams <local_user_name>
###     Enumerates all local teams for <local_user_name>.
###
###   a2userid <local_user_name>
###     Returns the user id for <local_user_name>.
###     (The user id is required rather than the user name for querying the team API.)
###
###   a2rules
###     Enumerates all rules for all projects. Projects with no rules are skipped.
###
### Reference:
###   https://blog.chef.io/now-what-were-those-permissions-for-this-user-again/
###
### Prerequisites:
###  * `jq` must be installed.
###  * env var TOK must be defined and specify a valid A2 admin token.
###  * env var TARGET_HOST must be defined in the form https://your.automate.host.


# source for this help infrastructure: https://samizdat.dev/help-message-for-shell-scripts/
help() {
  awk -F'### ' '/^###/{print $2}' "$0"
}

# Sourced scripts cannot include the no-arg check here!
if [[ "$1" == "-h" ]]; then
  help
  exit 1
fi

# Commands

a2policies() {
  local user_and_teams=("$1")
  local user_id
  user_id=$(curl -sSkH "api-token: $TOK" "$TARGET_HOST/apis/iam/v2/users/$1" | jq -cr '.user.membership_id')
  if [[ $user_id != "null" ]]; then
    teams=$(curl -sSkH "api-token: $TOK" "$TARGET_HOST/apis/iam/v2/users/$user_id/teams" | jq -r '.teams[] | .id')
    # shellcheck disable=SC2206
    user_and_teams=("$1" ${teams[@]}) 
  fi
  local resourceType="user"
  for resource in "${user_and_teams[@]}"
  do
    printf "\nChecking %s:local:%s...\n" "$resourceType" "$resource"
    curl -sSkH "api-token: $TOK" -H "" "$TARGET_HOST/apis/iam/v2/policies" | \
      jq -cr --arg name "$resourceType:local:$resource" \
             --arg allLocal "$resourceType:local:*" \
             --arg allType "$resourceType:*" \
        '.policies[] |
           select (.members | contains([$name]) or contains([$allLocal]) or contains([$allType]) or (index("*") != null)) |
           .id, .members'
    resourceType="team"
  done
}

# Works for local users only
a2userid() {
  curl -sSkH "api-token: $TOK" "$TARGET_HOST/apis/iam/v2/users/$1" | jq -cr '.user.membership_id'
}

# Works for local users only
a2teams() {
  local user_id
  user_id=$(a2userid "$1")
  curl -sSkH "api-token: $TOK" "$TARGET_HOST/apis/iam/v2/users/$user_id/teams" | jq -r '.teams[] | .id'
}

a2rules() {
  local projects
  projects=$(curl -sSkH "api-token: $TOK" "$TARGET_HOST/apis/iam/v2/projects" | \
    jq -r '.projects[] | select(.status != "NO_RULES") | .id')
  for proj in $projects
  do
    printf "\nProject %s...\n" "$proj"
    curl -sSkH "api-token: $TOK" "$TARGET_HOST/apis/iam/v2/projects/$proj/rules" | jq
  done
}
