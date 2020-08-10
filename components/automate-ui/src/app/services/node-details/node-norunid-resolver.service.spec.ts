import { Router, ActivatedRouteSnapshot } from '@angular/router';
import { TestBed } from '@angular/core/testing';

import { NodeRun } from 'app/types/types';
import { NodeNoRunIdResolverService } from './node-norunid-resolver.service';
import { NodeRunsService } from './node-runs.service';

class MockRouter {
  constructor() { }
  navigate(_route: Array<string>) {
  }
}

class MockNodeRunsService {
  response: Promise<NodeRun> = Promise.resolve<NodeRun>(null);

  constructor() { }

  public getNodeRunsByID(_nodeId: string) {
    return this.response;
  }

  setResponse(response: Promise<NodeRun>) {
    this.response = response;
  }
}

class MockRoute {
  paramMap = {
    get() { }
  };
}

describe('NodeDetailsResolverService', () => {
  let route: ActivatedRouteSnapshot;
  let service: NodeNoRunIdResolverService;
  const router = new MockRouter();

  const nodeRunsService = new MockNodeRunsService();

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        NodeNoRunIdResolverService,
        { provide: NodeRunsService, useValue: nodeRunsService },
        { provide: Router, useValue: router }]
    });

    service = TestBed.inject(NodeNoRunIdResolverService);
    route = new MockRoute() as unknown as ActivatedRouteSnapshot;
  });

  it('redirects to missing runs page when null nodeRun is returned', (done) => {

    nodeRunsService.setResponse(Promise.resolve<NodeRun>(null));
    spyOn(route.paramMap, 'get').and.callFake(() => 'fake-node-id');
    spyOn(router, 'navigate');

    service.resolve(route, null).then((_nodeRun: NodeRun) => {
      expect(router.navigate)
        .toHaveBeenCalledWith(['/infrastructure/client-runs/fake-node-id/missing-runs']);
      done();
    });

  });

  it('redirects to client runs page when non-404 error is returned', (done) => {
    nodeRunsService.setResponse(Promise.reject<NodeRun>('no real reason'));
    spyOn(router, 'navigate');

    service.resolve(route, null).then((_nodeRun: NodeRun) => {
      expect(router.navigate).toHaveBeenCalledWith(['/infrastructure/client-runs']);
      done();
    });
  });

  it('redirects to missing runs page when 404 error is returned', (done) => {
    nodeRunsService.setResponse(Promise.reject<NodeRun>('run not found 404'));
    spyOn(router, 'navigate');
    spyOn(route.paramMap, 'get').and.callFake(() => 'fake-node-id');

    service.resolve(route, null).then((_nodeRun: NodeRun) => {
      expect(router.navigate)
        .toHaveBeenCalledWith(['/infrastructure/client-runs/fake-node-id/missing-runs']);
      done();
    });
  });
});
