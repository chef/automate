#!/bin/bash

GREEN='\033[0;32m'
NC='\033[0m' # No Color
#post list of from to where connection will be checked.
bastion_to_automate_port=9631
bastion_to_elasticsearch_port=9631
elasticsearch_port=(9200 9631)
elasticsearch_to_elasticsearch_port=(9300 9638 9631)
postgres_to_postgres_port=(5432 9638 9631)
postgres_to_elasticsearch_port=(9200 9638 9631)
elasticsearch_to_postgres_port=(9638 9631)
kibana=5601

rm output.txt
#Taking and filtering ssh_key from config.toml file
SSH_KEY=$(grep -E '(^|\s)ssh_key_file($|\s)' config.toml | cut -c13- | sed 's/"//g' | sed 's/=//g')

#Taking and filtering ssh_user from config.toml file
SSH_USER=$(grep -E '(^|\s)ssh_user($|\s)' config.toml | cut -c10- | sed 's/"//g' | sed 's/=//g')


#This will take ips from config.toml of server's and convert "[]" this array format to bash array formate like ().
automate_server_private_ip=$(grep -E '(^|\s)automate_private_ips($|\s)' config.toml | cut -c24-)
automate_server_private_ip=${automate_server_private_ip// /}
automate_server_private_ip=${automate_server_private_ip//,/ }
automate_server_private_ip=${automate_server_private_ip##[}
automate_server_private_ip=${automate_server_private_ip%]}
eval automate_server_private_ip=($automate_server_private_ip)

chef_server_private_ip=$(grep -E '(^|\s)chef_server_private_ips($|\s)' config.toml | cut -c27-)
chef_server_private_ip=${chef_server_private_ip// /}
chef_server_private_ip=${chef_server_private_ip//,/ }
chef_server_private_ip=${chef_server_private_ip##[}
chef_server_private_ip=${chef_server_private_ip%]}
eval chef_server_ip_list=($chef_server_private_ip)

elasticsearch_private_ip=$(grep -E '(^|\s)elasticsearch_private_ips($|\s)' config.toml | cut -c29-)
elasticsearch_private_ip=${elasticsearch_private_ip// /}
elasticsearch_private_ip=${elasticsearch_private_ip//,/ }
elasticsearch_private_ip=${elasticsearch_private_ip##[}
elasticsearch_private_ip=${elasticsearch_private_ip%]}
eval elasticsearch_private_ip=($elasticsearch_private_ip)

postgresql_private_ip=$(grep -E '(^|\s)postgresql_private_ips($|\s)' config.toml | cut -c26-)
postgresql_private_ip=${postgresql_private_ip// /}
postgresql_private_ip=${postgresql_private_ip//,/ }
postgresql_private_ip=${postgresql_private_ip##[}
postgresql_private_ip=${postgresql_private_ip%]}
eval postgresql_private_ip=($postgresql_private_ip)


#connection will be checked from bastion(provisioning) to automate. Ensuring that automate allow bastion connection on 9631 port 
for i in ${automate_server_private_ip[@]};
do 
		
		
		ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF

			sudo su -
            curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh > /dev/null 2>&1 \
| sudo bash
            export HAB_LICENSE=accept-no-persist
            hab pkg install core/netcat -bf
			ls /tmp | nc -l -p $bastion_to_automate_port & 1>/dev/null
			
EOF
done


for i in ${automate_server_private_ip[@]};
do 
			echo -e "${GREEN}bastion is trying to connect automate on specific port. IP that mentioned below is automate's ip${NC}"
            curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh > /dev/null 2>&1 \ | sudo bash
            export HAB_LICENSE=accept-no-persist
            hab pkg install core/netcat -bf
			nc -zv $i $bastion_to_automate_port >> output.txt
			echo
			echo
				
done


for i in ${automate_server_private_ip[@]};
do 
	
		ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
			
			ps aux | grep 'nc -l -p $bastion_to_automate_port' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
			
done

#connection will be checked from bastion(provisioning) to elasticsearch. Ensuring that automate allow bastion connection on 9631 port 
for i in ${elasticsearch_private_ip[@]};
do 
	
		
		ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash << EOF

			sudo su -
            curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh > /dev/null 2>&1 \
| sudo bash
            export HAB_LICENSE=accept-no-persist
            hab pkg install core/netcat -bf
			ls /tmp | nc -l -p $bastion_to_elasticsearch_port & 1>/dev/null
			  
			 
EOF
				
done


for i in ${elasticsearch_private_ip[@]};
do 
			echo -e "${GREEN}bastion is trying to connect elasticsearch on specific port. IP that mentioned below is elasticsearch' ip${NC}"
			nc -zv $i $bastion_to_elasticsearch_port >> output.txt
			echo
			echo
			
done


for i in ${elasticsearch_private_ip[@]};
do 
	
		ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash << EOF
			
			ps aux | grep 'nc -l -p $bastion_to_elasticsearch_port' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
	
done

#elasticsearch-backends -> elasticsearch-backends connections will be checked. First loop will send some packets on secific port.
#Second loop will try to establish connection using nc from automate to elasticseach.
#Third loop will kill the process taht we have started in first loop.
for i in ${elasticsearch_private_ip[@]};
do 
	for j in ${elasticsearch_port[@]}; 
	do 
		ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash << EOF

			sudo su -
            curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh > /dev/null 2>&1 \
| sudo bash
            export HAB_LICENSE=accept-no-persist
            hab pkg install core/netcat -bf
			ls /tmp | nc -l -p $j & 1>/dev/null
			  
			 
EOF
	
	done
			
done

for i in ${automate_server_private_ip[@]};
do 
	for j in ${elasticsearch_port[@]}; 
	do 
		for k in ${elasticsearch_private_ip[@]};
		do

			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF

				echo -e "${GREEN}automate is trying to connect elasticsearch on specific port. Ip that mentioned below is elasticsearch's ip${NC}"
                sudo su -
                curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh > /dev/null 2>&1 \
| sudo bash
                export HAB_LICENSE=accept-no-persist
                hab pkg install core/netcat -bf
				nc -zv $k $j >> output.txt
				echo
				echo

EOF
		done
	done
			
done

for i in ${elasticsearch_private_ip[@]};
do 
	for j in ${elasticsearch_port[@]}; 
	do 
		ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash << EOF
			
			ps aux | grep 'nc -l -p $j' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
	
	done
			
done

#elasticsearch-backends -> elasticsearch-backends connections will be checked. First loop will send some packets on secific port.
#Second loop will try to establish connection using nc from every elasticseach to elasticseach.
#Third loop will kill the process taht we have started in first loop.
for i in ${elasticsearch_private_ip[@]};
do 
	for j in ${elasticsearch_to_elasticsearch_port[@]}; 
	do 
		echo "elasticsearch $i" >> output.txt
		ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash << EOF

			sudo su -
            curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh > /dev/null 2>&1 \
| sudo bash
            export HAB_LICENSE=accept-no-persist
            hab pkg install core/netcat -bf
			ls /tmp | nc -l -p $j & 1>/dev/null
			  
			 
EOF
	
	done
			
done

for i in ${elasticsearch_private_ip[@]};
do 
	for j in ${elasticsearch_to_elasticsearch_port[@]}; 
	do 
		for k in ${elasticsearch_private_ip[@]};
		do

			ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash  << EOF

				echo -e "${GREEN}elasticsearch is trying to connect elasticsearch in a cluster. All elasticsearch node will be tested. Ip mention below is elasticsearch ip${NC}"
                sudo su -
                curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh > /dev/null 2>&1 \
| sudo bash
                export HAB_LICENSE=accept-no-persist
                hab pkg install core/netcat -bf
				nc -zv $k $j >> output.txt
				echo
				echo
								
EOF
		done
	done
			
done

for i in ${elasticsearch_private_ip[@]};
do 
	for j in ${elasticsearch_to_elasticsearch_port[@]}; 
	do 
		ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash << EOF
			
			ps aux | grep 'nc -l -p $j' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
	
	done
			
done

#postgres-backends -> postgres-backends connections will be checked. First loop will send some packets on secific port.
#Second loop will try to establish connection using nc from every postgres to postgres node.
#Third loop will kill the process taht we have started in first loop.
for i in ${postgresql_private_ip[@]};
do 
	for j in ${postgres_to_postgres_port[@]}; 
	do 
		
		ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash << EOF

			sudo su -
            curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh > /dev/null 2>&1 \
| sudo bash
            export HAB_LICENSE=accept-no-persist
            hab pkg install core/netcat -bf
			ls /tmp | nc -l -p $j
			  
			 
EOF
	
	done
			
done

for i in ${postgresql_private_ip[@]};
do 
	for j in ${postgres_to_postgres_port[@]}; 
	do 
		for k in ${postgresql_private_ip[@]};
		do

			ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash << EOF

				echo -e "${GREEN}postgresql is trying to connect postgresql in a cluster. All postgresql node will be tested. Ip mention below is postgresql ip${NC}"
                sudo su -
                curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh > /dev/null 2>&1 \
| sudo bash
                export HAB_LICENSE=accept-no-persist
                hab pkg install core/netcat -bf
				nc -zv $k $j >> output.txt
				echo
				echo
			
EOF
		done
	done
			
done

for i in ${postgresql_private_ip[@]};
do 
	for j in ${postgres_to_postgres_port[@]}; 
	do 
		ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash << EOF
			
			ps aux | grep 'nc -l -p $j' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
	
	done
			
done


#postgres-backends -> elasticsearch-backends connections will be checked. First loop will send some packets on secific port.
#Second loop will try to establish connection using nc from every postgres to elasticsearch node.
#Third loop will kill the process taht we have started in first loop.
for i in ${elasticsearch_private_ip[@]};
do 
	for j in ${postgres_to_elasticsearch_port[@]}; 
	do 
		
		ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash << EOF

			sudo su -
            curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh > /dev/null 2>&1 \
| sudo bash
            export HAB_LICENSE=accept-no-persist
            hab pkg install core/netcat -bf
			ls /tmp | nc -l -p $j
			  
			 
EOF
	
	done
			
done

for i in ${postgresql_private_ip[@]};
do 
	for j in ${postgres_to_elasticsearch_port[@]}; 
	do 
		for k in ${elasticsearch_private_ip[@]};
		do

			ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash  << EOF
		
				echo -e "${GREEN}postgres is trying to connect elasticsearch. Below ip mention is elasticsearch ip${NC}"
                sudo su -
                curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh > /dev/null 2>&1 \
| sudo bash
                export HAB_LICENSE=accept-no-persist
                hab pkg install core/netcat -bf
				nc -zv $k $j >> output.txt
				echo
				echo

EOF
		done
	done
			
done

for i in ${elasticsearch_private_ip[@]};
do 
	for j in ${postgres_to_elasticsearch_port[@]}; 
	do 
		ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash << EOF
			
			ps aux | grep 'nc -l -p $j' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
	
	done
			
done

#postgres-backends -> elasticsearch-backends connections will be checked. First loop will send some packets on secific port.
#Second loop will try to establish connection using nc from every postgres to elasticsearch node.
#Third loop will kill the process taht we have started in first loop.
for i in ${postgresql_private_ip[@]};
do 
	for j in ${elasticsearch_to_postgres_port[@]}; 
	do 
		ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash << EOF

			sudo su -
            curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh > /dev/null 2>&1 \
| sudo bash
            export HAB_LICENSE=accept-no-persist
            hab pkg install core/netcat -bf
			ls /tmp | nc -l -p $j & 1>/dev/null
			  
			 
EOF
	
	done
			
done

for i in ${elasticsearch_private_ip[@]};
do 
	for j in ${postgres_to_elasticsearch_port[@]}; 
	do 
		for k in ${postgresql_private_ip[@]};
		do

			ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash  << EOF

				echo -e "${GREEN}elasticsearch is trying to connnect postgres on specific port. Below ip mention is elasticsearch ip${NC}"
                sudo su -
                curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh > /dev/null 2>&1 \
| sudo bash
                export HAB_LICENSE=accept-no-persist
                hab pkg install core/netcat -bf
				nc -zv $k $j >> output.txt
				echo
				echo
				
				
EOF
		done
	done
			
done

for i in ${postgresql_private_ip[@]};
do 
	for j in ${elasticsearch_to_postgres_port[@]}; 
	do 
		ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash << EOF
			
			ps aux | grep 'nc -l -p $j' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
	
	done
			
done

#bastion will try to check connections on 5601 on elasticsearch node that is for kibana.
for i in ${elasticsearch_private_ip[@]};
do 
			
		ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash << EOF

			sudo su -
            curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh > /dev/null 2>&1 \
| sudo bash
            export HAB_LICENSE=accept-no-persist
            hab pkg install core/netcat -bf
			ls /tmp | nc -l -p $kibana & 1>/dev/null
			  			 
EOF
				
done


for i in ${elasticsearch_private_ip[@]};
do 
			echo -e "${GREEN}bastion is trying to connect elasticsearch on specific port. IP that mentioned below is elasticsearch' ip${NC}"
			nc -zv $i $kibana >> output.txt
			echo
			echo
			
done


for i in ${elasticsearch_private_ip[@]};
do 
	
		ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SSH_USER@$i /bin/bash << EOF
			
			ps aux | grep 'nc -l -p $kibana' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
	
done