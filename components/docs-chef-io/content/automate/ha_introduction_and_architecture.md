+++
title = "Introduction"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Introduction"
    parent = "automate/deploy_high_availability/minimal_node_ha_deployment"
    identifier = "automate/deploy_high_availability/minimal_node_ha_deployment/ha_introduction_and_architecture.md Introduction"
    weight = 200
+++

This section will discuss the **Minimal Node HA Deployment** use case and how you can achieve it.

When you achieve Chef Automate HA using a minimum of five nodes, you can call it a Minimal Node HA Deployment. This deployment is the same as the typical HA deployment. The best way to achieve the Minimal Node HA Deployment in the backend is to keep the **PostGreSQL** and **OpenSearch** clusters in the same node. This will let your three nodes of PostGreSQL and OpenSearch services run.

You can achieve the Minimal Node HA Deployment in the frontend by setting the automate on both frontend nodes. The Automate is already running Chef Server services in this process, so you can avoid installing it explicitly on any node.

{{< figure src="/images/automate/minimal_node.png" alt="Minimal Node Deployment">}}
