package main

const cleanupScript = `
#!/bin/bash

while getopts ":c:d:n:hp" opt; do
  case "${opt}" in
    d)
      export DIR=${OPTARG}
      ;;
    n)
      export NODE=${OPTARG}
      ;;
    p)
      export PORT="CLEAN_ALL"
      ;;
	c)
      export CONFIG=${OPTARG}
      ;;
    h)
      usage
      ;;
    \?)
      echo "Invalid option: -${OPTARG}" >&2
      exit 1
      ;;
    :)
      echo "Option -${OPTARG} requires an argument." >&2
      exit 1
      ;;
  esac
done

elasticsearch_port=(9631 9200 9300 9638 5601)
postgresql_port=(9631 7432 5432 9638 6432)
automate_port=(9631 9638 5432 6432 7432 443)
chef_server_port=(9631 9638 5432 6432 7432 443)
#Taking and filtering ssh_key from $CONFIG file
SSH_KEY=$(grep -E '(^|\s)ssh_key_file($|\s)' $CONFIG | cut -c13- | sed 's/"//g' | sed 's/=//g')
#Taking and filtering ssh_user from $CONFIG file
SSH_USER=$(grep -E '(^|\s)ssh_user($|\s)' $CONFIG | cut -c10- | sed 's/"//g' | sed 's/=//g')
#This will take ips from $CONFIG of server's and convert "[]" this array format to bash array formate like ().
automate_server_private_ip=$(grep -E '(^|\s)automate_private_ips($|\s)' $CONFIG | cut -c24-)
automate_server_private_ip=${automate_server_private_ip// /}
automate_server_private_ip=${automate_server_private_ip//,/ }
automate_server_private_ip=${automate_server_private_ip##[}
automate_server_private_ip=${automate_server_private_ip%]}
eval automate_server_private_ip=($automate_server_private_ip)
chef_server_private_ip=$(grep -E '(^|\s)chef_server_private_ips($|\s)' $CONFIG | cut -c27-)
chef_server_private_ip=${chef_server_private_ip// /}
chef_server_private_ip=${chef_server_private_ip//,/ }
chef_server_private_ip=${chef_server_private_ip##[}
chef_server_private_ip=${chef_server_private_ip%]}
eval chef_server_private_ip=($chef_server_private_ip)
elasticsearch_private_ip=$(grep -E '(^|\s)elasticsearch_private_ips($|\s)' $CONFIG | cut -c29-)
elasticsearch_private_ip=${elasticsearch_private_ip// /}
elasticsearch_private_ip=${elasticsearch_private_ip//,/ }
elasticsearch_private_ip=${elasticsearch_private_ip##[}
elasticsearch_private_ip=${elasticsearch_private_ip%]}
eval elasticsearch_private_ip=($elasticsearch_private_ip)
postgresql_private_ip=$(grep -E '(^|\s)postgresql_private_ips($|\s)' $CONFIG | cut -c26-)
postgresql_private_ip=${postgresql_private_ip// /}
postgresql_private_ip=${postgresql_private_ip//,/ }
postgresql_private_ip=${postgresql_private_ip##[}
postgresql_private_ip=${postgresql_private_ip%]}
eval postgresql_private_ip=($postgresql_private_ip)
# Below 8 no. of loop will kill the process that is running on specific port.
export BANNER="
This script takes input from $CONFIG and clean your chef-automate HA nodes when in existing_node configuration
The following arguments are available:
 -c [/home/centos/config.toml] Absolute path of config.toml
 -n [automate|chef_server|postgresql|elasticsearch]   if you select this flag and give node name then that node will get cleanup
 -d [/hab | /var/tmp]     The directory which you give with this flag will be deleted
 -p                        if you give this flag, then will make sure no service is running on your required ports for automate HA
 [OPTIONS]
 -h                               Print this help message
 ex. 
 1) When you want to cleanup dir on all nodes and close port on all nodes
 bash cleanup.sh -c < Path to config.toml config.toml > -p 
 2) when you want to cleanup dir on particular node and close port on that particular node, 
 for example here we take elasticsearch 
 bash cleanup.sh -c < Path to config.toml config.toml > -n elasticsearch -d /hab -p
 3) when you want to cleanup dir on particular node and remove hab directory, 
 for example here we take elasticsearch
 bash cleanup.sh -c < Path to config.toml config.toml > -n elasticsearch -d /hab
 4) when you want to cleanup dir on particular node and remove both /hab and /var/tmp, 
 for example here we take elasticsearch
 bash cleanup.sh -c < Path to config.toml config.toml > -n elasticsearch 
"
usage() {
  echo "${BANNER}"
  exit 1
} 

echo $config


clean_dir() { 
    node="$1"
    path="$2"
     
	echo $node $path

    if [ "$path" = "/var/tmp" ];then 
	    path="/var/tmp/*"
    else
	    path="/hab*"
    fi

    if [ $node = "automate" ];then
        for i in ${automate_server_private_ip[@]};
        do  
            ssh -i $SSH_KEY $SSH_USER@$i /bin/bash  <<EOF
            sudo rm -rf $path
EOF
        done
    elif [ "$node" = "elasticsearch" ];then
        for i in ${elasticsearch_private_ip[@]};
        do
                ssh -i $SSH_KEY $SSH_USER@$i /bin/bash <<EOF
                sudo rm -rf $path
EOF
        done
    elif [ "$node" = "postgresql" ]; then
        for i in ${postgresql_private_ip[@]};
        do
                ssh -i $SSH_KEY $SSH_USER@$i /bin/bash <<EOF
                sudo rm -rf $path
EOF
        done
    elif [ "$node" = "chef_server" ]; then
        for i in ${chef_server_private_ip[@]};
        do
                ssh -i $SSH_KEY $SSH_USER@$i /bin/bash <<EOF
                sudo rm -rf $path
EOF
        done
    else
        echo "please put node name as automate,elasticsearch,postgresqland chef_server"
fi
}

close_port() {
    node="$1"
    if [ "$node" = "automate" ];
    then
        for i in ${automate_server_private_ip[@]};
        do
                ssh -i $SSH_KEY $SSH_USER@$i /bin/bash <<   EOF
                sudo kill -9 $(sudo lsof -t -i:$automate_port) > /dev/null 2>&1
EOF
    done
    elif [ "$node" = "elasticsearch" ];then
        for i in ${elasticsearch_private_ip[@]};
        do
                ssh -i $SSH_KEY $SSH_USER@$i /bin/bash <<   EOF
                sudo kill -9 $(sudo lsof -t -i:$elasticsearch_port) > /dev/null 2>&1
EOF
    done
    elif [ "$node" = "postgresql" ];then
    for i in ${postgresql_private_ip[@]};
        do
                ssh -i $SSH_KEY $SSH_USER@$i /bin/bash <<   EOF
                sudo kill -9 $(sudo lsof -t -i:$postgresql_port) > /dev/null 2>&1
EOF
    done
    elif [ "$node" = "chef_server" ];then
    for i in ${chef_server_server_private_ip[@]};
        do
                ssh -i $SSH_KEY $SSH_USER@$i /bin/bash <<   EOF
                sudo kill -9 $(sudo lsof -t -i:$chef_server_port) > /dev/null 2>&1
EOF
    done
    else
    echo "Cannot find node for port cleanup. Please put node name as automate,elasticsearch,postgresql and chef_server"
    fi

}

if [ "$PORT" = "CLEAN_ALL" ]; then

        if [ ! -z "$NODE" ]
        then
                close_port $NODE
        else
                close_port automate
                close_port elasticsearch
                close_port postgresql
                close_port chef_server
        fi
else
        echo "PORT has not been taken care"
fi

if [[ ! -z "$NODE" ]]
then 
	if [[ ! -z "$DIR" ]];then
		clean_dir $NODE $DIR
	else	
		clean_dir $NODE /hab
	        clean_dir $NODE /var/tmp/*
        fi
else 
	clean_dir automate /hab
	clean_dir automate /var/tmp/*
    clean_dir chef_server /hab
	clean_dir chef_server /var/tmp/*
    clean_dir elasticsearch /hab
	clean_dir elasticsearch /var/tmp/*
    clean_dir postgresql /hab
	clean_dir postgresql /var/tmp/*
fi
`
