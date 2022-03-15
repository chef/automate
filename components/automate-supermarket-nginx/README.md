## Potential Issues

- Investigate High CPU Usage by automate-cs-nginx: While load testing to determine EC2 instance node counts, we found that automate-cs-nginx was using approximately 150% CPU on a moderately-highly loaded 8CPU instance. The a2-LB nginx was not using that much CPU, despite moving all the same traffic. We suspect this may be a misconfiguration in automate-cs-nginx, though it could just be our Lua routing.
