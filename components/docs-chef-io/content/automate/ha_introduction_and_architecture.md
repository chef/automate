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

This section explains about the Minimal node HA Deployment use-case and the way we can achieve it.

What is minimal node ha deployment? 
So we can achive chef-automate HA with minimum of 5 nodes that is minimal node HA deployment .So this setup will behave exactly like that of noraml chef-automate HA.

So to achieve that, In backend instead of having seperate nodes for postgresql and opensearch cluster we can have this 2 clusters on same nodes. So same 3 nodes will have postgresql and opensearch services running . 

And on frontend side will have automate on both frontend nodes and will not install chef-server explicitly on any of nodes. As Automate has already running chef-server services on it and so it can be used .

{{< figure src="/images/automate/minimal_node.png" alt="Minimal node Deployment">}}