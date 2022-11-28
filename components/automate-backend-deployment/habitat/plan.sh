#stable channel

pkg_name=automate-ha-deployment
pkg_description="A wrapper package for setting up a deployment workstation for the A2 HA Backend."
pkg_origin=chef
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_deps=(
  core/ruby30
  core/aws-cli
  core/bash
  core/coreutils
  core/cacerts
  core/findutils
  core/gawk
  core/gzip
  core/jq-static
  core/openssh
  core/openssl
  core/make
  core/curl
  core/rsync
  core/terraform/0.14.8/20210826165930
  core/busybox-static
  chef/automate-ha-cluster-ctl
)
 
pkg_build_deps=(
  core/gcc
)

# workaround for https://github.com/habitat-sh/habitat/issues/6341
pkg_svc_user=root
pkg_svc_group=root
pkg_svc_run="return 0"

do_before() {
  update_pkg_version
}

do_setup_environment() {
  set_buildtime_env SSL_CERT_FILE "$(pkg_path_for cacerts)/ssl/cert.pem"
}

do_build() {
  # bundle install the required gems for smoke and integration tests
  cd $PLAN_CONTEXT/../../../test
  if [[ $HAB_DEV != "true" ]]; then
    rm -rf vendor/bundle
  fi
  bundle install --path vendor/bundle
}

do_install() {
  $(pkg_path_for core/bash)/bin/bash $PLAN_CONTEXT/../../../scripts/cert.sh
  mkdir -p $pkg_prefix/workspace
  mkdir -p $pkg_prefix/workspace/inspec/
  mkdir -p $pkg_prefix/workspace/scripts/
  mkdir -p $pkg_prefix/workspace/terraform/

  build_line "Copying tests"
  cp -r $PLAN_CONTEXT/../../../test $pkg_prefix/workspace/

  build_line "Copying inspec"
  cp -r $PLAN_CONTEXT/../../../inspec/automate-backend-opensearch-smoke $pkg_prefix/workspace/inspec/
  cp -r $PLAN_CONTEXT/../../../inspec/automate-backend-postgresql-smoke $pkg_prefix/workspace/inspec/
  cp -r $PLAN_CONTEXT/../../../inspec/automate-backend-resources $pkg_prefix/workspace/inspec/
  cp -r $PLAN_CONTEXT/../../../inspec/automate-frontend-chef-server-smoke $pkg_prefix/workspace/inspec/
  cp -r $PLAN_CONTEXT/../../../inspec/automate-frontend-smoke $pkg_prefix/workspace/inspec/
  
  build_line "Copying terraform"
  rsync -a --exclude=test-environments --exclude=test-license-usage --exclude=.editorconfig --exclude=\*.hart --exclude=\*.aib --exclude=.terraform --exclude=.git --exclude=backups --exclude=vendor --exclude=terraform.tfstate --exclude=terraform.tfstate.backup --exclude=*.swp $PLAN_CONTEXT/../../../terraform/a2ha-terraform/ $pkg_prefix/workspace/terraform

  # make sure no pre-built aibs get bundled into the package
  rm -f $pkg_prefix/workspace/terraform/transfer_files/*.aib
  cp -r $PLAN_CONTEXT/../../../scripts/bundle_creation.sh $pkg_prefix/workspace/scripts/
  cp -r $PLAN_CONTEXT/../../../scripts/bumpaib.sh $pkg_prefix/workspace/scripts/
  cp -r $PLAN_CONTEXT/../../../scripts/credentials $pkg_prefix/workspace/scripts/
  cp -r $PLAN_CONTEXT/../../../scripts/gather-logs $pkg_prefix/workspace/scripts/
  cp -r $PLAN_CONTEXT/../../../scripts/generate-chef-load-user-toml.sh $pkg_prefix/workspace/scripts/
  cp -r $PLAN_CONTEXT/../../../scripts/smoke-test $pkg_prefix/workspace/scripts/
  cp -r $PLAN_CONTEXT/../../../scripts/sup-keys.sh $pkg_prefix/workspace/scripts/
  cp -r $PLAN_CONTEXT/../../../scripts/tf_var_sort.py $pkg_prefix/workspace/scripts/
  cp -r $PLAN_CONTEXT/../../../scripts/airgap_bundle.sh $pkg_prefix/workspace/scripts/
  cp -r $PLAN_CONTEXT/../../../scripts/cert.sh $pkg_prefix/workspace/scripts/
  cp -r $PLAN_CONTEXT/../../../certs $pkg_prefix/workspace/ 
  cp -r $PLAN_CONTEXT/../../../terraform/a2ha-terraform/deployment-makefile/Makefile $pkg_prefix/workspace/
  cp -r $PLAN_CONTEXT/../../../terraform/a2ha-terraform/How-to-destroy-infra.md $pkg_prefix/workspace/terraform/

  # terraform dependecies
  build_line "building dependencies"
  cp -r $PLAN_CONTEXT/../../../terraform/a2ha-terraform/dependencies.tf $pkg_prefix/workspace/terraform/
  pushd $pkg_prefix/workspace/terraform/
  terraform init
  rm -f dependencies.tf
  popd

  # make sure no state is copied over
  rm -f $pkg_prefix/workspace/terraform/*.tfstate
  rm -f $pkg_prefix/workspace/terraform/*.tfstate.*
  rm -f $pkg_prefix/workspace/terraform/terraform.tfvars
  rm -f $pkg_prefix/workspace/terraform/a2ha*.auto.tfvars
  rm -f $pkg_prefix/workspace/terraform/*.abb
  rm -f $pkg_prefix/workspace/terraform/.tf_arch
  rm -f $pkg_prefix/workspace/terraform/main.tf
  rm -f $pkg_prefix/workspace/terraform/variables.tf
  rm -f $pkg_prefix/workspace/terraform/outputs.tf
  rm -f $pkg_prefix/workspace/terraform/versions.tf
  rm -f $pkg_prefix/workspace/backups
}

do_strip() {
  return 0
}
