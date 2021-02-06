#!/usr/bin/env bash

# Pre-requisites:
# gq from https://github.com/hasura/graphqurl
# jq from https://stedolan.github.io/jq/
# env var $BUILDKITE_TOKEN populated from https://buildkite.com/user/api-access-tokens

# Usage:
# pipelines --blocked
# pipelines --stale
# pipelines

pipelines() {
  mode=$1
  gql='query {
    organization(slug: "chef") {
      pipelines(last: 100, search: "[chef/automate:master]") {
        edges {
          node {
	    name
            builds(first: 1) {
              edges {
                node {
                  branch state message createdAt
                } } } } } } } }'
  if [[ $mode == "--blocked" ]]; then
    # See https://buildkite.com/docs/apis/rest-api/builds for buildkite API 
    curl -sH "Authorization: Bearer $BUILDKITE_TOKEN" https://api.buildkite.com/v2/builds?state=blocked | \
      jq -c '.[] | select(.pipeline.provider.settings.repository == "chef/automate") | { branch, created_at, state, pr:.pull_request.id, pipeline:.pipeline.name, message }'
  elif [[ $mode == "--stale" ]]; then
    # See https://buildkite.com/user/graphql/documentation for buildkite GraphQL API
    echo "Note: any reported pipelines are NOT necessarily problems"
    gq https://graphql.buildkite.com/v1 -H "Authorization: Bearer $BUILDKITE_TOKEN" --query="$gql" | \
      jq -c --arg today "$(date +"%Y-%m-%d")" \
        '.data.organization.pipelines.edges[].node | .builds.edges[].node.createdAt as $createdAt | select($createdAt < $today) | { name:.name, createdAt:$createdAt }'
  else
    gq https://graphql.buildkite.com/v1 -H "Authorization: Bearer $BUILDKITE_TOKEN" --query="$gql" | \
      jq -c '.data.organization.pipelines.edges[].node | .builds.edges[].node.createdAt as $createdAt | { name:.name, createdAt:$createdAt }'
  fi
}
