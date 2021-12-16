In some scenario we are required to migrate A2HA data to Automate HA cluster`(as we have new HA implementation in Automate)`. For that here are some steps that you'll have to follow.


1) Take backup of chef-automate from A2HA `(Old)` using below command, this command can be executed from any of front-end (chef-automate) node  
 in case of multiple frontends. 
 Usually if you don't specify any location for backup in config.toml then that backup will be store on /var/opt/chef-automate/backup location 
 if you hit below command.

    `sudo chef-automate backup create`
    

2) Make a tar file of backup that we have taken in step 1. Here make sure that you are also taking backup of `.tmp` directory. Otherwise you'll face some issue related to metadata.json.

    E.g. `tar -cvf backup.tar.gz path/to/backup/20201030172917/ /path/to/backup/automate-backup-elasticsearch/ /path/to/backup/.tmp/`
   


3) Create a abb file from any of chef-automate frontend node of A2HA. This will create a bundle of all necessary keys. Like pivotal.pem, secret key etc. Usually this will not be included in regular backup`(step 1)` so make sure you create a bundle for that.


    `sudo chef-automate bootstrap bundle create bootstrap.abb`

4) copy tar file and abb file that we created in step 2 and 3 respectively to any of the Automate HA chef-automate instance and extract it on specific location that you mentioned in config.toml file. Its is not necessary to extract that backup on below location. But make sure that restore command is able to read backup or not from your defined location on step1.


    E.g  `/mnt/automate-backup`
    
5) Restore A2HA backup on Automate HA. Read this docs for [chef-automate restore](https://docs.chef.io/automate/restore/). In below command there is also a frontend-20210624095659.aib generated in new Automate HA file mention that's because while restoration we also keep in mind all the services habitat release version. Because during restoration time A2HA restoration will try to find A2HA habitat pkg version so there can be a scenario occure where all`(A2HA and automate HA  frontends (automate's))` packages version can't be the same. That's why we are passing current Automate HA packages. You can find frontend aib file in /var/tmp directory of your Automate HA chef-automate instance.


    E.g. `sudo chef-automate backup restore /mnt/automate_backups/backups/20210622065515/ --patch-config /etc/chef-automate/config.toml --airgap-bundle /var/tmp/frontend-20210624095659.aib --skip-preflight`

 


6) After that if you not do this step then you will might face warning when you'll try to open chef-automate UI `It looks like you do not have permission to access any data in automate` So make sure you have unpacked the. abb file. Otherwise you'll not see login page. To unpack the bootstrap file that we copied from A2HA chef-automate using below command


    `sudo chef-automate bootstrap bundle unpack bootstrap.abb`
    


7) Copy the bootstrap.abb file to another automate node and chef node also if you are having multiple automate and chef instances. Because the secrets we have restored by unpacking the bootstrap file would be different for another automate instance. So we need to make that all the automate and chef instance would be in sync.



Important command and notes

Using the below command you can see what bootstrap includes in aib file and abb file. aib file will only include keys related data while abb file will include service packages also. You can use below command and can compare both the file's data


    `tail -n +3 bootstrap.abb | tar -tf -`
    

After using above command, if you want to see the data of service like secret service then you would see those services into `/hab/svc ` directory. This'll be needed if you want to compare aib data between multiple FE(In respective chef-automate and chef-server) nodes.  

     E.g For secret service `cat /hab/svc/secrets-service/data/secrets_key`
    
  

