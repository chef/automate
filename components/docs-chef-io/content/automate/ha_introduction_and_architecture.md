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

In this section, we will talk about the **Minimal Node HA Deployment** use-case and the way it can be achieved.

When Chef Automate HA can be achieved with minimum of five nodes, this is called as Minimal Node HA Deployment. This deployment is not different from the usual HA deployment.

The best way to achieve the Minimal Node HA Deployment in backend is to keep the **PostGreSQL** and **OpenSearch** clusters in same node. By this you will have three nodes of PostGreSQL and OpenSearch services running.

Whereas, the best way to achieve the Minimal Node HA Deployment in frontend, is to set the automate on both frontend nodes. In this whole process, the Automate is already running Chef Server services so do not install it explicitely on any node.

{{< figure src="/images/automate/minimal_node.png" alt="Minimal Node Deployment">}}
