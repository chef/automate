import { Injectable } from '@angular/core';
import { Router, Resolve, RouterStateSnapshot,
         ActivatedRouteSnapshot } from '@angular/router';

import { NodeRun } from 'app/types/types';
import { NodeRunsService } from './node-runs.service';

@Injectable()
export class NodeNoRunIdResolverService implements Resolve<NodeRun> {
  constructor(private service: NodeRunsService, private router: Router) {}

  resolve(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): Promise<NodeRun> {
    const nodeId = route.paramMap.get('node-id');

    return this.service.getNodeRunsByID( nodeId ).
      then(nodeRuns => {
        // if the node runs is found we select the latest run-id.
        // Go to node runs detail page
        if (nodeRuns && nodeRuns.length) {
          this.router.navigate(
            ['/infrastructure/client-runs/' + nodeId + '/runs/' + nodeRuns[0].runId]
          );
          return null;
        } else {
          // if the node run was not found we assume this node has no runs.
          // Go to node missing runs page
          this.router.navigate(['/infrastructure/client-runs/' + nodeId + '/missing-runs']);
          return null;
        }
      }).catch(e => {
        // if the node run was not found we assume this node has no runs.
        // Go to node missing runs page
        if (JSON.stringify(e).indexOf('404') > 0) {
          this.router.navigate(['/infrastructure/client-runs/' + nodeId + '/missing-runs']);
        } else {
          this.router.navigate(['/infrastructure/client-runs']);
        }

        return null;
      });
  }
}
