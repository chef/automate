Use the following commands on OpenSearch nodes to verify their health status.

1. Verify that the Habitat service is running.

    ```sh
    hab svc status
    ```

1. Check the status of OpenSearch indices.

    ```sh
    curl -k -X GET "https://localhost:9200/_cat/indices/*?v=true&s=index&pretty" -u admin:admin
    ```

1. View logs of the Chef Habitat services.

    ```sh
    journalctl -u hab-sup -f | grep 'automate-ha-opensearch'
    ```
