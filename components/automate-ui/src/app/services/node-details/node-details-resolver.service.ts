import { Injectable } from '@angular/core';
import { Router, Resolve, RouterStateSnapshot,
         ActivatedRouteSnapshot } from '@angular/router';

import { NodeRun } from '../../types/types';
import { NodeRunsService } from './node-runs.service';

@Injectable()
export class NodeDetailsResolverService implements Resolve<NodeRun> {
  constructor(private service: NodeRunsService, private router: Router) {}

  resolve(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): Promise<NodeRun> {
    const nodeId = route.paramMap.get('node-id');
    const runId = route.paramMap.get('run-id');
    const endTime = new Date(route.paramMap.get('end_time'));

    return this.service.getNodeRun(nodeId, runId, endTime).
      then(nodeRun => {
        if (nodeRun) {
          return nodeRun;
        } else {
          // if the node run was not found we assume this node has not runs.
          // Go to node missing runs page
          this.router.navigate(['/client-runs/' + nodeId + '/missing-runs']);
          return null;
        }
      }).catch(e => {
        // if the node run was not found we assume this node has no runs.
        // Go to node missing runs page
        if (JSON.stringify(e).indexOf('404') > 0) {
          this.router.navigate(['/client-runs/' + nodeId + '/missing-runs']);
        } else {
          this.router.navigate(['/client-runs']);
        }

        return null;
      });
  }
}
