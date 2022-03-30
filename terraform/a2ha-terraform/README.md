This terraform code is related to a2ha. Basically it support 2 type of architecture. one is aws and second is existing_nodes.
Aws one will create infrastructure from scretch and automate will get configured after that while for existing_nodes, infra will be provided by the user. For existing_nodes,
we just have to configure the things related to automate and database using terraform.

Deployment architecture will get used to configure automate and databse related stuff and it only can use after AWS provision.
