# Existing environments: dev, acceptance, union, rehearsal, delivered.
CHANNEL ?= userdev
TF_VARS ?= $(CHANNEL)
TF_STATE ?= $(CHANNEL).tfstate

ifeq ($(CHANNEL), userdev)
	export TF_STATE=$(USER).tfstate
	export TF_VAR_dns_suffix ?= $(USER)
endif

init:
	$(info ===============================================================)
	$(info AWS VPC............delivered)
	$(info Terraform State....$(TF_STATE))
	$(info Terraform Vars.....$(TF_VARS))
	$(info Habitat Channel....$(TF_VAR_channel))
	$(info Website URL........a2-docs-$(TF_VAR_dns_suffix))
	$(info ===============================================================)
	$(info )

remote-state: init
	# We make sure that existing local state is deleted before configuring
	# the remote state. If it is not removed state gets contaminated between
	# environments.

	rm -rf .terraform && \
	terraform init -backend-config="key=a2-automate-chef-io/$(TF_STATE)"

plan: remote-state
	terraform plan -var-file=vars/$(TF_VARS).tfvars

apply: remote-state
	terraform apply -auto-approve -var-file=vars/$(TF_VARS).tfvars

destroy: remote-state
ifeq ($(TF_ENVIRONMENT), userdev)
	terraform destroy -var-file=vars/$(TF_VARS).tfvars
else
	@echo "The destroy target is only supported for userdev environments."
endif

show: remote-state
	terraform show

output: remote-state
	terraform output

console: remote-state
	terraform console

lint:
	terraform fmt -check=true -list=true -diff=true

.PHONY: init remote-state plan apply destroy show output console lint
