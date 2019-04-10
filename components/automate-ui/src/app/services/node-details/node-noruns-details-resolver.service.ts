import { Injectable } from '@angular/core';
import { Resolve, RouterStateSnapshot,
         ActivatedRouteSnapshot } from '@angular/router';
import {
  Node
} from '../../entities/client-runs/client-runs.model';
import {
  ClientRunsRequests
} from '../../entities/client-runs/client-runs.requests';

@Injectable()
export class NodeNoRunsDetailsResolverService implements Resolve<Node> {

  constructor(private requests: ClientRunsRequests) {}

  resolve(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): Promise<Node> {
    const nodeId = route.paramMap.get('node-id');

    const filters = {
      nodeId: nodeId
    };

    return this.requests.getNodes(filters).toPromise().
      then(nodes => {
        if (nodes && nodes.length > 0) {
          return nodes[0];
        } else {
          return this.createNullNode(nodeId);
        }
      }).catch(_e => {
        return this.createNullNode(nodeId);
      });
  }


  private createNullNode(nodeId: string): Node {
    return {
      id: nodeId,
      name: '',
      fqdn: '',
      checkin: null,
      uptime_seconds: 0,
      environment: '',
      source_fqdn: '',
      organization: '',
      platform: '',
      policy_group: '',
      status: 'failure',
      latestRunId: '',
      hasRuns: false,
      lastCcrReceived: null,
      deprecationsCount: 0,
      chefVersion: ''
    };
  }
}
