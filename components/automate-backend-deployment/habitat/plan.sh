#stable channel

pkg_name=automate-ha-deployment
pkg_description="A wrapper package for setting up a deployment workstation for the A2 HA Backend."
pkg_origin=chef
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")

pkg_deps=(
  core/ruby3_4
  core/aws-cli/1.31.4/20240106034739
  core/bash/5.1/20240105214248
  core/coreutils/8.32/20240105213308
  core/cacerts/2021.10.26/20240105224256
  core/findutils/4.9.0/20240105220908
  core/gawk/5.1.0/20240105214723
  core/gzip/1.13/20240105221940
  core/jq-static/1.6/20240107004905
  core/openssh/7.9p1/20240106022237
  core/openssl/1.0.2zi/20240105224424
  core/make/4.3/20240105222044
  core/curl/8.7.1/20240614090648
  core/rsync/3.2.3/20240107034222
  core/terraform1/1.5.7/20240106055300
  core/busybox-static/1.34.1/20240105230035
  chef/automate-ha-cluster-ctl
)

#core/aws-cli ( core/aws-cli/1.21.11/20231020110846 core/aws-cli/1.21.11/20220817123642 )
   # core/openssl11 ( core/openssl11/1.1.1w/20231020105352 core/openssl11/1.1.1k/20220311131131 )
   # core/python ( core/python/3.10.0/20231020105702 core/python/3.10.0/20220817121853 )
   #
pkg_build_deps=(
  core/gcc/9.5.0/20240105175314
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

  build_line "Adding Certificates"
  $(pkg_path_for core/bash)/bin/bash $PLAN_CONTEXT/cert.sh "$PLAN_CONTEXT"

  build_line "Copying Certificates toml to workspace"
  cp $PLAN_CONTEXT/default_backend_certificates.toml $pkg_prefix/workspace/

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






