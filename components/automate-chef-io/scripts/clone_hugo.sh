#!/bin/bash

clone_url="https://github.com/chef/chef-hugo-theme.git"
if [[ -n "$GITHUB_TOKEN" ]];then
    clone_url="https://x-access-token:${GITHUB_TOKEN}@github.com/chef/chef-hugo-theme.git"
fi

git clone -b old_hugo_theme "$clone_url" themes/chef
