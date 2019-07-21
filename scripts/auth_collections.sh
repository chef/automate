#!/bin/bash

# Prerequisites:
#    jq post-processor
#       https://stedolan.github.io/jq/
#    jo pre-processor (for authGen function)
#       https://github.com/jpmens/jo
#    variable $TOK
#       An admin token obtained from `get_admin_token` or `chef-automate admin-token`
#    variable $TARGET_HOST
#       A host running A2, either `https://a2-dev.test` or a remote machine.
#
# These works either inside or outside Habitat Studio.


#############
# authQuery
#############
# Purpose:
#     Provide a very concise way to fetch collections of a2 auth resources at the API level.
#
# Usage:
#     a2 [ <options> ] <collection> [ <name> <value> ]
#
#     <collection> may be "users", "teams", "policies", "tokens", "projects", or "roles".
#     Optionally specify a property <name> and <value> if you wish to emit just a single entity.
#
# Options:
#    -o or --one-line     output each entity on one-line
#    -s or --show         show the executed curl command
#    -2 or --v2           use IAM V2 endpoints
#    -p or --projects     project filter; a comma-separate list as a single argument
#
# Examples:
#     a2 users
#     a2 --one users
#     a2 -s users name bob
#     a2 -o --show users name bob
#     a2 --v2 -o --show -p proj1,proj2 roles
#
# To run on a different environment you can adjust the env vars for a single invocation. Example:
# TARGET_HOST=https://fresh-install-dev.cd.chef.co TOK=stVpD9I23OFn_U2OME= a2 -s policies

authQuery () {
  # prerequisites
  local token=${TOK:?Need this env variable to contain ChefAutomate admin token}
  local host=${TARGET_HOST:?Need this env variable to point to your ChefAutomate host}
  if ! type jq &> /dev/null
  then
    echo "Need jq installed"; return;
  fi

  # process options
  jq_option=
  show_cmd=
  auth_path="api/v0/auth"
  projects=""
  while [[ "$1" =~ ^- ]]; do
    case "$1" in
      -o|--one-line) jq_option="-c"; shift;;
      -s|--show) show_cmd=1; shift;;
      -2|--v2) auth_path="apis/iam/v2beta"; shift;;
      -p|--projects) projects="projects: $2"; shift 2;;
      -*) echo "'$1' unknown"; return 1;;
    esac
  done

  # generate jq script
  if [[ "$3" != "" ]]; then
    jq_script=".$1[] | select (.$2 == \"$3\")"
  elif [[ "$jq_option" == "-c" ]]; then 
    jq_script=".$1[]"
  else
    jq_script="."
  fi

  # execute
  if [[ "$show_cmd" == 1 ]]; then 
    echo "curl -sSkH \"api-token: \$TOK\" -H \"$projects\" \"$host/$auth_path/$1\" | jq $jq_option '$jq_script'"
  fi
  curl -sSkH "api-token: $token" -H "$projects" "$host/$auth_path/$1" | jq $jq_option "$jq_script"
}


#############
# authLoad
#############
# Purpose:
#     Load a list of a2 auth resources.
#     You can copy resources from one machine to another (e.g. for debugging or backup)
#     by storing the output of authQuery into a file and feeding that file to authLoad.
#
# Options:
#    -s or --show         show the executed curl commands
#    -d or --dry-run      show the curl commands but do not execute them
#    -2 or --v2           use IAM V2 endpoints
#
# Example:
#    authLoad teams captured/teams-from-acceptance.txt
#
authLoad() {
  # process options
  dry_run=
  show_cmd=
  auth_path="api/v0/auth"
  while [[ "$1" =~ ^- ]]; do
    case "$1" in
      -s|--show) show_cmd=1; shift;;
      -d|--dry-run) dry_run=1; shift;;
      -2|--v2) auth_path="apis/iam/v2beta"; shift;;
      -*) echo "'$1' unknown"; return 1;;
    esac
  done

  # prerequisites
  local token=${TOK:?Need this env variable to contain ChefAutomate admin token}
  local host=${TARGET_HOST:?Need this env variable to point to your ChefAutomate host}
  [[ -z "$2" ]] && { echo "usage: authLoad [ <options> ] <resource> <filename>"; return; }
  if ! type jq &> /dev/null
  then
    echo "Need jq installed"; return;
  fi
  RESOURCE=$1
  FILE=$2

  readarray -t lines <"$FILE"
  for line in "${lines[@]}"
  do
    if [[ -n "$line" ]]; then
      # v1 resources work by just re-using output of authQuery except for users, which need a password added
      # TODO: check that all v2 resources work
      if [[ "$RESOURCE" == "users" ]]; then
        line=$(jq -c '. += {password: "chefautomate"}' <<< "$line")
      fi
      if [[ "$show_cmd" == 1 ]] || [[ "$dry_run" == 1 ]]; then 
        echo "curl -sSkH \"api-token: \$token\" \"$host/$auth_path/$RESOURCE\" -d '$line'"
      fi
      if [[ -z "$dry_run" ]]; then 
        curl -sSkH "api-token: $token" "$host/$auth_path/$RESOURCE" -d "$line"
        echo
      fi
    fi
  done
}

#############
# authGen
#############
# Purpose:
#     Generate a list of a2 auth resources.
#     This generates dummy data that is not particularly realistic
#     but can be used to observe general performance characteristics.
#     NB: When generating policies, it builds one policy per token,
#     so you should also generate a set of tokens with the same parameters.
#     This function also allows you to delete your generated resources just as easily.
# 
# Example:
#     Create 500 policies starting with a seed of 101:
#         $ authGen policies create 500 101
#     Per above note, those policies need these members:
#         $ authGen tokens create 500 101
#     Now, just for illustration, delete the last 100 of those 500 policies:
#         $ authGen policies delete 100 501
#
# Options:
#    -2 or --v2           use IAM V2 endpoints
#
# TODO: Except for common things (e.g. teams) this supports only IAM v1 so far.
#
authGen() {
  # process options
  auth_path="api/v0/auth"
  while [[ "$1" =~ ^- ]]; do
    case "$1" in
      -2|--v2) auth_path="apis/iam/v2beta"; shift;;
      -*) echo "'$1' unknown"; return 1;;
    esac
  done

  # prerequisites
  local token=${TOK:?Need this env variable to contain ChefAutomate admin token}
  local host=${TARGET_HOST:?Need this env variable to point to your ChefAutomate host}
  if [[ -z "$4" ]]; then echo "usage: authGen [ <options> ] <resource> <mode> <count> <seed>"; return; fi
  if ! type jo &> /dev/null
  then
    echo "Need jo installed"; return;
  fi
  RESOURCE=$1
  MODE=$2
  COUNT=$3
  SEED=$4
  ID_PREFIX=test-$RESOURCE
  if [[ ! $MODE =~ ^(create|delete)$ ]]; then echo "mode must be 'create' or 'delete'"; return; fi
  if [[ ! $RESOURCE =~ ^(tokens|users|teams|team-members|policies|rules)$ ]]; then
    echo "resource must be in: tokens, users, teams, team-members, policies, rules"
    return
  fi

  operation="${MODE}_resource"
  echo "$MODE $COUNT $RESOURCE..."
  for (( i = 0; i < COUNT; i++ )) 
  do
    $operation "$token" "$host" $i
  done
}

function create_resource {
  local token=$1
  local host=$2
  local id_index=$(($3 + SEED))
  local id=$ID_PREFIX-$id_index
  local name="$ID_PREFIX $id_index"
  local json
  local resource
  case "$RESOURCE" in
    tokens)
      json=$(jo -p id="$id" name="$name" description="test token for $name" active=true)
      resource=$RESOURCE ;;
    users)
      json=$(jo -p id="$id" name="$name" username="$id" password=chefautomate active=true)
      resource=$RESOURCE ;;
    teams)
      # v1 ignores id, v2 ignores description
      json=$(jo id="$id" name="$name" description="test team for $name")
      resource=$RESOURCE ;;
    team-members)
      # just add 100 members to each team
      json=$(jq -n --argjson n 100 'reduce range(1; $n) as $i (.; .user_ids += ["test-user-\($i)"])')
      resource="teams/test-teams-$id_index/users:add" ;;
    policies)
      json=$(jo subjects="$(jo -a token:test-tokens-$id_index)" action=read resource="compliance:*")
      resource=$RESOURCE ;;
    rules)
      # note: requires adding "foo-project" first
      json=$(jo id="$id" name="$name" type=NODE project_id=foo-project conditions='[{"operator":"MEMBER_OF","attribute":"CHEF_SERVERS","values":["prod","staging"]}]')
      resource=$RESOURCE ;;
  esac
  curl -sSkH "api-token: $token" "$host/$auth_path/$resource" -X POST --data "$json"
  echo
}
 
function delete_resource {
  local token=$1
  local host=$2
  local id_index=$(($3 + SEED))
  local id=$ID_PREFIX-$id_index
  local name="$ID_PREFIX $id_index"
  case "$RESOURCE" in
    tokens)  ;;
    users)  ;;
    teams) 
      local teams
      teams=$(curl -sSkH "api-token: $token" "$host/$auth_path/$RESOURCE")
      id=$(echo "$teams" | jq -r --arg name "$name" '.teams[] | select (.name==$name).id') ;;
    policies) 
      local policies
      policies=$(curl -sSkH "api-token: $token" "$host/$auth_path/$RESOURCE")
      id=$(echo "$policies" | jq -r --arg name "token:test-tokens-$id_index" '.policies[] | select (.subjects==[$name]).id') ;;
  esac
  curl -sSkH "api-token: $token" "$host/$auth_path/$RESOURCE/$id" -X DELETE
  echo
}
