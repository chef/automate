import { TestBed } from '@angular/core/testing';

import { NodeNoRunsDetailsResolverService } from './node-noruns-details-resolver.service';
import { Node } from '../../entities/client-runs/client-runs.model';
import { ClientRunsRequests } from '../../entities/client-runs/client-runs.requests';
import { Router } from '@angular/router';
import { Observable, of as observableOf, throwError } from 'rxjs';

class MockRouter {
  constructor() {}
  navigate(_route: Array<string>) {
  }
}

class MockNodesService {
  response: Observable<Node[]> = observableOf<Node[]>(null);

  constructor() {}

  public getNodes(_filters: any): Observable<Node[]> {
    return this.response;
  }

  setResponse(response: Observable<Node[]>) {
    this.response = response;
  }
}

class MockRoute {
  paramMap = {
    get() {}
  };
}

describe('NodeNoRunsDetailsResolverService', () => {
  let route;
  let service: NodeNoRunsDetailsResolverService;
  const router = new MockRouter();
  let nodesService: MockNodesService;

  beforeEach(() => {
    nodesService = new MockNodesService();
    TestBed.configureTestingModule({
      providers: [
        NodeNoRunsDetailsResolverService,
        { provide: ClientRunsRequests, useValue: nodesService },
        { provide: Router, useValue: router }]
    });

    route = new MockRoute();
    service = TestBed.get(NodeNoRunsDetailsResolverService);
  });

  describe('node list with one value is returned (normal case)', () => {
    const nodeId = 'real-fake-node-id';
    beforeEach(() => {
        const nodesReturned: Node[] = [{
            id: nodeId,
            name: '',
            fqdn: '',
            source_fqdn: '',
            organization: '',
            checkin: null,
            uptime_seconds: 0,
            environment: '',
            platform: '',
            policy_group: '',
            status: 'failure',
            latestRunId: '',
            hasRuns: true,
            lastCcrReceived: new Date(),
            deprecationsCount: 0,
            chefVersion: '12.6.0'
          }];

        nodesService.setResponse(observableOf<Node[]>(nodesReturned));
    });

    it('should return the one node', (done) => {
      spyOn(route.paramMap, 'get').and.callFake(() => 'different-fake-node-id');

      service.resolve(route, null).then((node: Node) => {
        expect(node.id).toBe(nodeId);
        done();
      });
    });
  });

  describe('node list with two values is returned', () => {
    const nodeId = 'first Id';
    beforeEach(() => {
        const nodesReturned: Node[] = [
            {
                id: nodeId,
                name: '',
                fqdn: '',
                checkin: null,
                uptime_seconds: 0,
                environment: '',
                platform: '',
                policy_group: '',
                status: 'failure',
                organization: '',
                source_fqdn: '',
                latestRunId: '',
                hasRuns: true,
                lastCcrReceived: new Date(),
                deprecationsCount: 0,
                chefVersion: '12.6.0'
            },
            {
                id: 'should-not-be-used',
                name: '',
                fqdn: '',
                checkin: null,
                uptime_seconds: 0,
                environment: '',
                organization: '',
                platform: '',
                source_fqdn: '',
                policy_group: '',
                status: 'failure',
                latestRunId: '',
                hasRuns: true,
                lastCcrReceived: new Date(),
                deprecationsCount: 0,
                chefVersion: '12.6.0'
            }
        ];

        nodesService.setResponse(observableOf<Node[]>(nodesReturned));
    });

    it('should return the first node', (done) => {
      spyOn(route.paramMap, 'get').and.callFake(() => 'different-fake-node-id');

      service.resolve(route, null).then((node: Node) => {
        expect(node.id).toBe(nodeId);
        done();
      });
    });
  });

  describe('null node list is returned', () => {
    beforeEach(() => {
        nodesService.setResponse(observableOf<Node[]>(null));
    });

    it('should return an empty Node with the node ID requested with', (done) => {
      spyOn(route.paramMap, 'get').and.callFake(() => 'fake-node-id');

      service.resolve(route, null).then((node: Node) => {
        expect(node.id).toBe('fake-node-id');
        done();
      });
    });
  });

  describe('error when requesting node list is returned', () => {
    beforeEach(() => {
        nodesService.setResponse(throwError('test failure'));
    });

    it('should return an empty Node with the node ID requested with', (done) => {
      spyOn(route.paramMap, 'get').and.callFake(() => 'fake-node-id');

      service.resolve(route, null).then((node: Node) => {
        expect(node.id).toBe('fake-node-id');
        done();
      });
    });
  });
});
