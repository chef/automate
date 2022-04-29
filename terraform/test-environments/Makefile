# Existing environments: dev, acceptance, union, rehearsal, delivered.
TF_ENVIRONMENT ?= userdev
TF_VARS ?= userdev
TF_STATE ?= $(TF_ENVIRONMENT).tfstate

ifeq ($(TF_ENVIRONMENT), userdev)
	# We have 4 isolated dev environments for individual users
	export TF_STATE=dev-$(TF_ENVIRONMENT_INDEX).tfstate
	export TF_VAR_environment=dev-$(TF_ENVIRONMENT_INDEX)
	export TF_VAR_dns_suffix ?= dev-$(TF_ENVIRONMENT_INDEX)
	export TF_VAR_chef_environment ?= $(TF_ENVIRONMENT)
	export TF_VAR_aws_tag_contact ?= $(USER)
	export DNS_SUFFIX ?= dev-$(TF_ENVIRONMENT_INDEX)
else ifeq ($(TF_ENVIRONMENT), acceptance)
	TF_VARS=unstable
	export DNS_SUFFIX=unstable
	export TF_VAR_chef_environment=$(TF_ENVIRONMENT)
else ifeq ($(TF_ENVIRONMENT), union)
	TF_VARS=dev
	export DNS_SUFFIX=dev
	export TF_VAR_chef_environment=$(TF_ENVIRONMENT)
else ifeq ($(TF_ENVIRONMENT), delivered)
	TF_VARS=acceptance
	export DNS_SUFFIX=acceptance
	export TF_VAR_chef_environment=$(TF_ENVIRONMENT)
endif

init:
	$(info ===============================================================)
ifeq ($(TF_ENVIRONMENT), userdev)
ifeq ($(TF_ENVIRONMENT_INDEX),)
	$(error The userdev environment requires an index! Please export $$TF_ENVIRONMENT_INDEX and try again.)
endif
	$(info AWS VPC............$(TF_VAR_environment))
else
	$(info AWS VPC............$(TF_ENVIRONMENT))
endif
	$(info Terraform State....$(TF_STATE))
	$(info Terraform Vars.....$(TF_VARS))
	$(info Chef Environment...$(TF_VAR_chef_environment))
	$(info Habitat Channel....$(DNS_SUFFIX))
	$(info Dashboard URL......$(DASHBOARD_URL))
	$(info ===============================================================)
	$(info )

DASHBOARD_URL ?= a2-$(DNS_SUFFIX).cd.chef.co

remote-state: init
	# We make sure that existing local state is deleted before configuring
	# the remote state. If it is not removed state gets contaminated between
	# environments.

	cd scenarios && \
	rm -rf .terraform && \
	terraform init -backend-config="key=a2/$(TF_STATE)"

plan: remote-state
	cd scenarios && terraform plan -var-file=vars/$(TF_VARS).tfvars

apply: remote-state
	cd scenarios && terraform apply -auto-approve -var-file=vars/$(TF_VARS).tfvars

destroy: remote-state
ifeq ($(TF_ENVIRONMENT), userdev)
	cd scenarios && terraform destroy -var-file=vars/$(TF_VARS).tfvars
else
	@echo "The destroy target is only supported for userdev environments."
endif

show: remote-state
	cd scenarios && terraform show

output: remote-state
	cd scenarios && terraform output

console: remote-state
	cd scenarios && terraform console

dashboard:
	cd ../../ && TF_STATE=$(TF_STATE) bash terraform/test-environments/scripts/update-dashboard-data.sh

mark-deploy:
	DASHBOARD_URL=$(DASHBOARD_URL) LOGS_URL=$(LOGS_URL) bash ./scripts/mark-dashboard.sh

unmark-deploy:
	aws --profile chef-cd s3 rm s3://$(DASHBOARD_URL)/assets/deploy_logs

deploy: mark-deploy apply dashboard unmark-deploy

lint:
	terraform fmt -check=true -list=true -diff=true

.PHONY: init remote-state plan apply destroy show output console lint deploy mark-deploy unmark-deploy
