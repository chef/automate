# terraform.tfstate file will be generated here while executig 'automate-backend-ctl provision'. And other necessary files also will be copied here during terraform apply. So that we can destroy terraform aws resources from this seprate dir.

# We already have modules.json so that we dont need to terraform init here. Because here module path will be changed so we can't use parent dir's .terraform/modules/modules.json. 
