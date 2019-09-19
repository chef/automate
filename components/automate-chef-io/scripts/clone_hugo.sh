#!/bin/bash

clone_url="https://github.com/chef/chef-hugo-theme.git"
if [[ -n "$GITHUB_TOKEN" ]];then
    clone_url="https://${GITHUB_TOKEN}@github.com/chef/chef-hugo-theme.git"
elif [[ -n "$GITHUB_APP_TOKEN" ]];then
    clone_url="https://x-access-token:${GITHUB_APP_TOKEN}@github.com/chef/chef-hugo-theme.git"
fi

git clone "$clone_url" themes/chef
