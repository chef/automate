import { TestBed } from '@angular/core/testing';

import { NodeNoRunIdResolverService } from './node-norunid-resolver.service';
import { NodeRunsService } from './node-runs.service';
import { Router } from '@angular/router';
import { NodeRun } from '../../types/types';

class MockRouter {
  constructor() {}
  navigate(_route: Array<string>) {
  }
}

class MockNodeRunsService {
  response: Promise<NodeRun> = Promise.resolve<NodeRun>(null);

  constructor() {}

  public getNodeRunsByID(_nodeId: string) {
    return this.response;
  }

  setResponse(response: Promise<NodeRun>) {
    this.response = response;
  }
}

class MockRoute {
  paramMap = {
    get() {}
  };
}

describe('NodeDetailsResolverService', () => {
  let route;
  let service: NodeNoRunIdResolverService;
  const router = new MockRouter();
  let nodeRunsService: MockNodeRunsService;

  nodeRunsService = new MockNodeRunsService();

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        NodeNoRunIdResolverService,
        { provide: NodeRunsService, useValue: nodeRunsService },
        { provide: Router, useValue: router }]
    });

    service = TestBed.inject(NodeNoRunIdResolverService);
    route = new MockRoute();
  });

  describe('null nodeRun is returned', () => {
    beforeEach(() => {
      nodeRunsService.setResponse(Promise.resolve<NodeRun>(null));
    });

    it('should redirect to missing runs page', (done) => {

      spyOn(route.paramMap, 'get').and.callFake(() => 'fake-node-id');
      spyOn(router, 'navigate');

      service.resolve(route, null).then((_nodeRun: NodeRun) => {
        expect(router.navigate)
          .toHaveBeenCalledWith(['/infrastructure/client-runs/fake-node-id/missing-runs']);
        done();
      });

    });
  });

  describe('non404 error in requesting nodeRun', () => {
    beforeEach(() => {
      nodeRunsService.setResponse(Promise.reject<NodeRun>('no real reason'));
    });

    it('should redirect to client runs page', (done) => {
      spyOn(router, 'navigate');

      service.resolve(route, null).then((_nodeRun: NodeRun) => {
        expect(router.navigate).toHaveBeenCalledWith(['/infrastructure/client-runs']);
        done();
      });
    });
  });

  describe('404 error in requesting nodeRun', () => {
    beforeEach(() => {
      nodeRunsService.setResponse(Promise.reject<NodeRun>('run not found 404'));
    });

    it('should redirect to missing runs page', (done) => {
      spyOn(router, 'navigate');
      spyOn(route.paramMap, 'get').and.callFake(() => 'fake-node-id');

      service.resolve(route, null).then((_nodeRun: NodeRun) => {
        expect(router.navigate)
          .toHaveBeenCalledWith(['/infrastructure/client-runs/fake-node-id/missing-runs']);
        done();
      });
    });
  });
});
