Try these steps if Chef Automate returns an error while restoring data.

1. Check the Chef Automate status.

   ```sh
   chef-automate status
   ```

1. Check the status of your Habitat service on the Automate node.

   ```sh
   hab svc status
   ```

1. If the deployment services are not healthy, reload them.

    ```sh
    hab svc load chef/deployment-service
    ```

Now check the status of the Automate node and then try running the restore command from the bastion host.
