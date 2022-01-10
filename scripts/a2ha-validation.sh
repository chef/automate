#!/bin/bash


validation () {
	RED='\033[0;31m'
	PURPLE='\033[0;35m'
	NC='\033[0m' # No Color
	#post list of from to where connection will be checked.
	bastion_to_automate_port=9631
	bastion_to_elasticsearch_port=9631
	bastion_to_postgresql_port=9631
	elasticsearch_port=(9200 9631)
	postgresql_port=(7432 9631)
	elasticsearch_to_elasticsearch_port=(9300 9638 9631)
	postgres_to_postgres_port=(5432 9638 9631)
	postgres_to_elasticsearch_port=(9200 9638 9631)
	elasticsearch_to_postgres_port=(9638 9631)
	kibana=5601

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
	eval chef_server_private_ip=($chef_server_private_ip)

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

	#This will install hab in bastion server to verify port
	sudo wget -o /tmp/hab-x86_64-linux.tar.gz https://packages.chef.io/files/stable/habitat/latest/hab-x86_64-linux.tar.gz
	sudo tar -xvzf /tmp/hab-x86_64-linux.tar.gz -C /usr/local/bin --strip-components 1
	export HAB_LICENSE=accept-no-persist
	hab pkg install core/netcat -bf


	# Below 5 no of loop will install hab utilty from remote server.

	for i in ${automate_server_private_ip[@]};
	do 
			NC_PKG=$(ls -dtr1 /hab/cache/artifacts/core-netcat-* | tail -1)
			scp -o StrictHostKeyChecking=no -i $SSH_KEY hab-x86_64-linux.tar.gz $SSH_USER@$i:/tmp/hab-x86_64-linux.tar.gz 
			scp -o StrictHostKeyChecking=no -i $SSH_KEY ${NC_PKG} $SSH_USER@$i:/tmp/ 
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF

			sudo su -
			tar -xvzf /tmp/hab-x86_64-linux.tar.gz -C /usr/local/bin --strip-components 1
			export HAB_LICENSE=accept-no-persist
			ls -dtr1 /tmp/core-netcat-* | tail -1 | xargs hab pkg install
				
EOF
	done

	for i in ${chef_server_private_ip[@]};
	do 
			
			
			NC_PKG=$(ls -dtr1 /hab/cache/artifacts/core-netcat-* | tail -1)
			scp -o StrictHostKeyChecking=no -i $SSH_KEY hab-x86_64-linux.tar.gz $SSH_USER@$i:/tmp/hab-x86_64-linux.tar.gz 
			scp -o StrictHostKeyChecking=no -i $SSH_KEY ${NC_PKG} $SSH_USER@$i:/tmp/  
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF

			sudo su -
			tar -xvzf /tmp/hab-x86_64-linux.tar.gz -C /usr/local/bin --strip-components 1
			export HAB_LICENSE=accept-no-persist
			ls -dtr1 /tmp/core-netcat-* | tail -1 | xargs hab pkg install
				
EOF
	done

	for i in ${elasticsearch_private_ip[@]};
	do 
			
			
			NC_PKG=$(ls -dtr1 /hab/cache/artifacts/core-netcat-* | tail -1)
			scp -o StrictHostKeyChecking=no -i $SSH_KEY hab-x86_64-linux.tar.gz $SSH_USER@$i:/tmp/hab-x86_64-linux.tar.gz 
			scp -o StrictHostKeyChecking=no -i $SSH_KEY ${NC_PKG} $SSH_USER@$i:/tmp/  
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF

			sudo su -
			tar -xvzf /tmp/hab-x86_64-linux.tar.gz -C /usr/local/bin --strip-components 1
			export HAB_LICENSE=accept-no-persist
			ls -dtr1 /tmp/core-netcat-* | tail -1 | xargs hab pkg install
				
EOF
	done

	for i in ${postgresql_private_ip[@]};
	do 
			
			
			NC_PKG=$(ls -dtr1 /hab/cache/artifacts/core-netcat-* | tail -1)
			scp -o StrictHostKeyChecking=no -i $SSH_KEY hab-x86_64-linux.tar.gz $SSH_USER@$i:/tmp/hab-x86_64-linux.tar.gz 
			scp -o StrictHostKeyChecking=no -i $SSH_KEY ${NC_PKG} $SSH_USER@$i:/tmp/  
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF

			sudo su -
			tar -xvzf /tmp/hab-x86_64-linux.tar.gz -C /usr/local/bin --strip-components 1
			export HAB_LICENSE=accept-no-persist
			ls -dtr1 /tmp/core-netcat-* | tail -1 | xargs hab pkg install
				
EOF
	done

	# Below 8 no. of loop will kill the process that is running on specific port.

	for i in ${automate_server_private_ip[@]};
	do

			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				ps aux | grep 'nc -l -p $bastion_to_automate_port' | awk {'print $2'} | xargs kill -9 > /dev/null 2>&1

EOF

	done

	for i in ${elasticsearch_private_ip[@]};
	do

			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF

				ps aux | grep 'nc -l -p $bastion_to_elasticsearch_port' | awk {'print $2'} | xargs kill -9 > /dev/null 2>&1

EOF

	done

	for i in ${elasticsearch_private_ip[@]};
	do
		for j in ${elasticsearch_port[@]};
		do
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF

				ps aux | grep 'nc -l -p $j' | awk {'print $2'} | xargs kill -9 > /dev/null 2>&1

EOF

		done

	done

	for i in ${elasticsearch_private_ip[@]};
	do
		for j in ${elasticsearch_to_elasticsearch_port[@]};
		do
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF

				ps aux | grep 'nc -l -p $j' | awk {'print $2'} | xargs kill -9 > /dev/null 2>&1

EOF

		done

	done

	for i in ${postgresql_private_ip[@]};
	do
		for j in ${postgres_to_postgres_port[@]};
		do
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF

				ps aux | grep 'nc -l -p $j' | awk {'print $2'} | xargs kill -9 > /dev/null 2>&1

EOF

		done

	done

	for i in ${elasticsearch_private_ip[@]};
	do
		for j in ${postgres_to_elasticsearch_port[@]};
		do
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF

				ps aux | grep 'nc -l -p $j' | awk {'print $2'} | xargs kill -9 > /dev/null 2>&1

EOF

		done

	done

	for i in ${postgresql_private_ip[@]};
	do
		for j in ${elasticsearch_to_postgres_port[@]};
		do
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF

				ps aux | grep 'nc -l -p $j' | awk {'print $2'} | xargs kill -9 > /dev/null 2>&1

EOF

		done

	done

	for i in ${elasticsearch_private_ip[@]};
	do

			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF

				ps aux | grep 'nc -l -p $kibana' | awk {'print $2'} | xargs kill -9 > /dev/null 2>&1
EOF

	done	

	echo
	echo -e "${PURPLE}Port checking process is started. It will take around 7-10 min. Please wait for some time.${NC}"
	echo 

	#connection will be checked from bastion(provisioning) to automate. Ensuring that automate allow bastion connection on 9631 port 
	# for i in ${automate_server_private_ip[@]};
	# do 
			
	# 		ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
	# 		(ls /tmp | nc -l -p $bastion_to_automate_port &) | sleep 2 && exit

	# EOF
	# done

	for i in ${automate_server_private_ip[@]};
	do 
				
				rm output.txt > /dev/null 2>&1
				nc -zv $i $bastion_to_automate_port > output.txt 2>&1
				VAR1=$( cat output.txt | grep -ow "Connection refused" )
				if [ "$VAR1" = "Connection refused" ]; then
					echo -e "${RED}BASTION IS NOT ABLE TO CONNECT AUTOMATE $i ON $bastion_to_automate_port PORT. PLEASE CHECK${NC}"
					echo
				fi

					
	done


	for i in ${automate_server_private_ip[@]};
	do 
		
			ssh -n -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				ps aux | grep 'nc -l -p $bastion_to_automate_port' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1
EOF
				
	done

	#connection will be checked from bastion(provisioning) to chef-server. Ensuring that automate allow bastion connection on 9631 port 
	# for i in ${chef_server_private_ip[@]};
	# do 
			
	# 		ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
	# 		(ls /tmp | nc -l -p $bastion_to_automate_port &) | sleep 2 && exit

	# EOF
	# done

	for i in ${chef_server_private_ip[@]};
	do 
				
				rm output.txt > /dev/null 2>&1
				nc -zv $i $bastion_to_automate_port > output.txt 2>&1
				VAR1=$( cat output.txt | grep -ow "Connection refused" )
				if [ "$VAR1" = "Connection refused" ]; then
					echo -e "${RED}BASTION IS NOT ABLE TO CONNECT CHEF_SERVER $i ON $bastion_to_automate_port PORT. PLEASE CHECK${NC}"
				fi

					
	done


	for i in ${chef_server_private_ip[@]};
	do 
		
			ssh -n -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				ps aux | grep 'nc -l -p $bastion_to_automate_port' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1
EOF
				
	done

	#connection will be checked from bastion(provisioning) to elasticsearch. Ensuring that automate allow bastion connection on 9631 port 
	for i in ${elasticsearch_private_ip[@]};
	do

			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				(ls /tmp | nc -l -p $bastion_to_elasticsearch_port &) | sleep 2 && exit


EOF
					
	done


	for i in ${elasticsearch_private_ip[@]};
	do 

				
				rm output.txt > /dev/null 2>&1
				nc -zv $i $bastion_to_elasticsearch_port > output.txt 2>&1
				
				VAR1=$( cat output.txt | grep -ow "Connection refused" )
				if [ "$VAR1" = "Connection refused" ]; then
					echo -e "${RED}BASTION IS NOT ABLE TO CONNECT ELASTICSEARCH $i ON $bastion_to_elasticsearch_port PORT. PLEASE CHECK${NC}"
				fi

				
	done


	for i in ${elasticsearch_private_ip[@]};
	do 
		
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				ps aux | grep 'nc -l -p $bastion_to_elasticsearch_port' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
		
	done

	#connection will be checked from bastion(provisioning) to postgresql. Ensuring that automate allow bastion connection on 9631 port 
	for i in ${postgresql_private_ip[@]};
	do

			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				(ls /tmp | nc -l -p $bastion_to_elasticsearch_port &) | sleep 2 && exit


EOF
					
	done


	for i in ${postgresql_private_ip[@]};
	do 

				
				rm output.txt > /dev/null 2>&1
				nc -zv $i $bastion_to_postgresql_port > output.txt 2>&1
				
				VAR1=$( cat output.txt | grep -ow "Connection refused" )
				if [ "$VAR1" = "Connection refused" ]; then
					echo -e "${RED}BASTION IS NOT ABLE TO CONNECT POSTGRESQL $i on $bastion_to_postgresql_port PORT. PLEASE CHECK${NC}"
				fi

				
	done


	for i in ${elasticsearch_private_ip[@]};
	do 
		
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				ps aux | grep 'nc -l -p $bastion_to_postgresq_port' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
		
	done

	#automate-frontends -> elasticsearch-backends connections will be checked. First loop will send some packets on secific port.
	#Second loop will try to establish connection using nc from automate to elasticseach.
	#Third loop will kill the process taht we have started in first loop.
	for i in ${elasticsearch_private_ip[@]};
	do 
		for j in ${elasticsearch_port[@]}; 
		do 
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				(ls /tmp | nc -l -p $j &) | sleep 2 && exit
				
				
EOF
		
		done
				
	done

	for i in ${automate_server_private_ip[@]};
	do 
		for j in ${elasticsearch_port[@]}; 
		do 
			for k in ${elasticsearch_private_ip[@]};
			do

				ssh -i $SSH_KEY $SSH_USER@$i /bin/bash > output.txt 2<&1 << EOF

					rm output.txt > /dev/null 2>&1
					nc -zv $k $j >> output.txt 2>&1
					VAR1=$( cat output.txt | grep -ow "Connection refused" )
					VAR2="Connection refused"
					if [ "$VAR1" = "$VAR2" ]; then
						echo -e "${RED}AUTOMATE $i IS NOT ABLE TO CONNECT ELASTICSEARCH $k ON $j PORT. PLEASE CHECK${NC}"
					fi


EOF

			done
		done
				
	done

	for i in ${elasticsearch_private_ip[@]};
	do 
		for j in ${elasticsearch_port[@]}; 
		do 
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				ps aux | grep 'nc -l -p $j' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
		
		done
				
	done

	#chef-server-frontends -> elasticsearch-backends connections will be checked. First loop will send some packets on secific port.
	#Second loop will try to establish connection using nc from automate to elasticseach.
	#Third loop will kill the process taht we have started in first loop.
	for i in ${elasticsearch_private_ip[@]};
	do 
		for j in ${elasticsearch_port[@]}; 
		do 
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				(ls /tmp | nc -l -p $j &) | sleep 2 && exit
				
				
EOF
		
		done
				
	done

	for i in ${chef_server_private_ip[@]};
	do 
		for j in ${elasticsearch_port[@]}; 
		do 
			for k in ${elasticsearch_private_ip[@]};
			do

				ssh -i $SSH_KEY $SSH_USER@$i /bin/bash > output.txt 2<&1 << EOF

					rm output.txt > /dev/null 2>&1
					nc -zv $k $j >> output.txt 2>&1
					VAR1=$( cat output.txt | grep -ow "Connection refused" )
					VAR2="Connection refused"
					if [ "$VAR1" = "$VAR2" ]; then
						echo -e "${RED}CHEF-SERVER $i IS NOT ABLE TO CONNECT ELASTICSEARCH $i ON $j PORT. PLEASE CHECK${NC}"
					fi


EOF

			done
		done
				
	done

	for i in ${elasticsearch_private_ip[@]};
	do 
		for j in ${elasticsearch_port[@]}; 
		do 
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				ps aux | grep 'nc -l -p $j' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
		
		done
				
	done

	#chef-server-frontends -> postgres-backends connections will be checked. First loop will send some packets on secific port.
	#Second loop will try to establish connection using nc from automate to elasticseach.
	#Third loop will kill the process taht we have started in first loop.
	for i in ${postgresql_private_ip[@]};
	do 
		for j in ${postgresql_port[@]}; 
		do 
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				(ls /tmp | nc -l -p $j &) | sleep 2 && exit
				
				
EOF
		
		done
				
	done

	for i in ${chef_server_private_ip[@]};
	do 
		for j in ${postgresql_port[@]}; 
		do 
			for k in ${postgresql_private_ip[@]};
			do

				ssh -i $SSH_KEY $SSH_USER@$i /bin/bash > output.txt 2<&1 << EOF

					rm output.txt > /dev/null 2>&1
					nc -zv $k $j >> output.txt 2>&1
					VAR1=$( cat output.txt | grep -ow "Connection refused" )
					VAR2="Connection refused"
					if [ "$VAR1" = "$VAR2" ]; then
						echo -e "${RED}CHEF-SERVER $i IS NOT ABLE TO CONNECT POSTGRESQL $i ON $j PORT. PLEASE CHECK${NC}"
					fi


EOF

			done
		done
				
	done

	for i in ${postgresql_private_ip[@]};
	do 
		for j in ${postgresql_port[@]}; 
		do 
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				ps aux | grep 'nc -l -p $j' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
		
		done
				
	done

	#automate-frontends -> postgres-backends connections will be checked. First loop will send some packets on secific port.
	#Second loop will try to establish connection using nc from automate to elasticseach.
	#Third loop will kill the process taht we have started in first loop.
	for i in ${postgresql_private_ip[@]};
	do 
		for j in ${postgresql_port[@]}; 
		do 
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				(ls /tmp | nc -l -p $j &) | sleep 2 && exit
				
				
EOF
		
		done
				
	done

	for i in ${automate_private_ip[@]};
	do 
		for j in ${postgresql_port[@]}; 
		do 
			for k in ${postgresql_private_ip[@]};
			do

				ssh -i $SSH_KEY $SSH_USER@$i /bin/bash > output.txt 2<&1 << EOF

					rm output.txt > /dev/null 2>&1
					nc -zv $k $j >> output.txt 2>&1
					VAR1=$( cat output.txt | grep -ow "Connection refused" )
					VAR2="Connection refused"
					if [ "$VAR1" = "$VAR2" ]; then
						echo -e "${RED}AUTOMATE $i IS NOT ABLE TO CONNECT POSTGRESQL $k ON $j PORT. PLEASE CHECK${NC}"
					fi


EOF

			done
		done
				
	done

	for i in ${postgresql_private_ip[@]};
	do 
		for j in ${postgresql_port[@]}; 
		do 
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
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
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				(ls /tmp | nc -l -p $j &) | sleep 2 && exit
				
				
EOF
		
		done
				
	done

	for i in ${elasticsearch_private_ip[@]};
	do 
		for j in ${elasticsearch_to_elasticsearch_port[@]}; 
		do 
			for k in ${elasticsearch_private_ip[@]};
			do

				ssh -i $SSH_KEY $SSH_USER@$i /bin/bash > output.txt 2<&1   << EOF
					
					#echo -e "${PURPLE}elasticsearch $i is trying to connect elasticsearch $k on a port $j.${NC}"
					rm output.txt > /dev/null 2>&1
					nc -zv $k $j > output.txt 2>&1
					
					VAR1=$(cat output.txt | grep -ow "Connection refused")
					if [ "$VAR1" = "Connection refused" ]; then
						echo -e "${RED}ELASTICSEARCH $i IS NOT ABLE TO CONNECT ELASTICSEARCH $k ON $j PORT. PLEASE CHECK${NC}"
					fi

									
EOF


				ssh -i $SSH_KEY $SSH_USER@$k /bin/bash << EOF

					(ls /tmp | nc -l -p $j &) | sleep 2 && exit


EOF
			done
		done
				
	done

	for i in ${elasticsearch_private_ip[@]};
	do 
		for j in ${elasticsearch_to_elasticsearch_port[@]}; 
		do 
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
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
			
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				(ls /tmp | nc -l -p $j &) | sleep 2 && exit
				
				
EOF
		
		done
				
	done

	for i in ${postgresql_private_ip[@]};
	do 
		for j in ${postgres_to_postgres_port[@]}; 
		do 
			for k in ${postgresql_private_ip[@]};
			do

				ssh -i $SSH_KEY $SSH_USER@$i /bin/bash > output.txt 2<&1  << EOF

					rm output.txt > /dev/null 2>&1
					nc -zv $k $j > output.txt 2>&1
					
					VAR1=$(cat output.txt | grep -ow "Connection refused")
					if [ "$VAR1" = "Connection refused" ]; then
						echo -e "${RED}POSTGRESQL $i IS NOT ABLE TO CONNECT POSTGRESQL $k ON $j PORT. PLEASE CHECK${NC}"
					fi

			
EOF


				ssh -i $SSH_KEY $SSH_USER@$k /bin/bash << EOF

					(ls /tmp | nc -l -p $j &) | sleep 2 && exit


EOF

			done
		done
				
	done

	for i in ${postgresql_private_ip[@]};
	do 
		for j in ${postgres_to_postgres_port[@]}; 
		do 
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
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
			
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				(ls /tmp | nc -l -p $j &) | sleep 2 && exit
				
				
EOF
		
		done
				
	done

	for i in ${postgresql_private_ip[@]};
	do 
		for j in ${postgres_to_elasticsearch_port[@]}; 
		do 
			for k in ${elasticsearch_private_ip[@]};
			do

				ssh -i $SSH_KEY $SSH_USER@$i /bin/bash > output.txt 2<&1  << EOF
			
					rm output.txt > /dev/null 2>&1
					nc -zv $k $j > output.txt 2>&1
					
					VAR1=$(cat output.txt | grep -ow "Connection refused")
					if [ "$VAR1" = "Connection refused" ]; then
						echo -e "${RED}POSTGRESQL $i IS NOT ABLE TO CONNECT ELASTICSEARCH $k ON $j PORT. PLEASE CHECK${NC}"
					fi


EOF

				ssh -i $SSH_KEY $SSH_USER@$k /bin/bash << EOF

					(ls /tmp | nc -l -p $j &) | sleep 2 && exit


EOF

			done
		done
				
	done

	for i in ${elasticsearch_private_ip[@]};
	do 
		for j in ${postgres_to_elasticsearch_port[@]}; 
		do 
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				ps aux | grep 'nc -l -p $j' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
		
		done
				
	done

	#elasticsearch-backends -> postgresql-backends connections will be checked. First loop will send some packets on secific port.
	#Second loop will try to establish connection using nc from every postgres to elasticsearch node.
	#Third loop will kill the process taht we have started in first loop.
	for i in ${postgresql_private_ip[@]};
	do 
		for j in ${elasticsearch_to_postgres_port[@]}; 
		do 
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				(ls /tmp | nc -l -p $j &) | sleep 2 && exit
				
				
EOF
		
		done
				
	done

	for i in ${elasticsearch_private_ip[@]};
	do 
		for j in ${postgres_to_elasticsearch_port[@]}; 
		do 
			for k in ${postgresql_private_ip[@]};
			do

				ssh -i $SSH_KEY $SSH_USER@$i /bin/bash > output.txt 2<&1  << EOF

					rm output.txt > /dev/null 2>&1
					nc -zv $k $j > output.txt 2>&1
					
					VAR1=$(cat output.txt | grep -ow "Connection refused")
					if [ "$VAR1" = "Connection refused" ]; then
						echo -e "${RED}ELASTICSEARCH $i IS NOT ABLE TO CONNECT POSTGRESQL $k ON $j PORT. PLEASE CHECK${NC}"
					fi

					
					
EOF


				ssh -i $SSH_KEY $SSH_USER@$k /bin/bash << EOF

					(ls /tmp | nc -l -p $j &) | sleep 2 && exit


EOF

			done
		done
				
	done

	for i in ${postgresql_private_ip[@]};
	do 
		for j in ${elasticsearch_to_postgres_port[@]}; 
		do 
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				ps aux | grep 'nc -l -p $j' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
		
		done
				
	done

	#bastion will try to check connections on 5601 on elasticsearch node that is for kibana.
	for i in ${elasticsearch_private_ip[@]};
	do 
				
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				(ls /tmp | nc -l -p $kibana &) | sleep 2 && exit
							
EOF
					
	done


	for i in ${elasticsearch_private_ip[@]};
	do 
				#echo -e "${PURPLE}bastion is trying to connect elasticsearch on $kibana port. IP that mentioned below is elasticsearch' ip${NC}"
				rm output.txt > /dev/null 2>&1
				nc -zv $i $kibana > output.txt 2>&1
				
				VAR1=$(cat output.txt | grep -ow "Connection refused")
				if [ "$VAR1" = "Connection refused" ]; then
					echo -e "${RED}BASTION IS NOT ABLE TO CONNECT ELASTICSEARCH $i ON $kibana(KIBANA) PORT. PLEASE CHECK${NC}"
				fi
				
	done


	for i in ${elasticsearch_private_ip[@]};
	do 
		
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash << EOF
				
				ps aux | grep 'nc -l -p $kibana' | awk {'print $2'} | xargs kill -9 1>/dev/null 2>&1

EOF
		
	done

	echo
	echo -e "${PURPLE}Thank you for your patience. Port checking process is completed If you see error above in red color then please check that port and make connection open for the specific scenario. If you don't see the error then you can start the deployment ${NC}"
	echo

	# Below 4 no of loop will remobe hab utilty from remote server.
	for i in ${automate_server_private_ip[@]};
	do 
			
			
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash >> output.txt << EOF
				sudo rm -rf /hab
				
EOF
	done

	for i in ${chef_server_private_ip[@]};
	do 
			
			
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash >> output.txt << EOF
				sudo rm -rf /hab
				
EOF
	done

	for i in ${elasticsearch_private_ip[@]};
	do 
			
			
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash >> output.txt << EOF
				sudo rm -rf /hab
				
EOF
	done

	for i in ${postgresql_private_ip[@]};
	do 
			
			
			ssh -i $SSH_KEY $SSH_USER@$i /bin/bash >> output.txt << EOF
				sudo rm -rf /hab
				
EOF
	done

	sudo rm -rf /hab
}

validation
