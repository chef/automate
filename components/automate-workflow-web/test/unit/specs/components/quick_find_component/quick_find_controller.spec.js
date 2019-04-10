import ng from 'angular';
import 'angular-mocks';
import quickFindController 
  from '../../../../../src/components/quick_find/quick_find_controller';

describe('quickFindController', () => {
  let createController;

  beforeEach(ng.mock.module(quickFindController));

  beforeEach(inject(($controller, $httpBackend) => {
    createController = (locals) => {
      return $controller('quickFindController', locals);
    };
  }));

  describe('getResults()', () => {
    let $httpBackend, ApiUrl, pipelines;

    beforeEach(inject((_$httpBackend_) => {
      $httpBackend = _$httpBackend_;

      ApiUrl = jasmine
        .createSpy('ApiUrl')
        .and.returnValue('/api/v0/e/Foobar/pipelines');

      pipelines = [
        {
          id: 1,
          org: 'fooOrg',
          project: 'fooProject'
        },
        {
          id: 2,
          org: 'barOrg',
          project: 'barProject'
        },
        {
          id: 3,
          org: 'bazOrg',
          project: 'bazProject'
        }
      ];

      $httpBackend
        .when('GET', '/api/v0/e/Foobar/pipelines')
        .respond(pipelines);
    }));

    it('should fetch list of pipelines', () => {
      $httpBackend.expectGET('/api/v0/e/Foobar/pipelines');
      let ctrl = createController({ ApiUrl: ApiUrl });
      ctrl.getResults('foo');
      $httpBackend.flush();
    });

    it('should return promise for all results where the project name contains the search term', (done) => {
      let ctrl = createController({ ApiUrl: ApiUrl });
      let resultsPromise = ctrl.getResults('foo');
      resultsPromise.then((results) => {
        expect(results).toEqual([
          {
            id: 1,
            org: 'fooOrg',
            project: 'fooProject'
          }
        ]);
        done();
      });
      $httpBackend.flush();
    });

    it('should not case match search results', (done) => {
      let ctrl = createController({ ApiUrl: ApiUrl });
      let resultsPromise = ctrl.getResults('Foo');
      resultsPromise.then((results) => {
        expect(results).toEqual([
          {
            id: 1,
            org: 'fooOrg',
            project: 'fooProject'
          }
        ]);
        done();
      });
      $httpBackend.flush();
    });
  });

  describe('selectResult()', () => {
    let $state;

    beforeEach(() => {
      $state = { go: jasmine.createSpy('go') };
    });
    
    it('should navigate to the project', () => {
      let ctrl = createController({ $state: $state });
      let result = {
        id: 3,
        org: 'fooOrg',
        project: 'barProject'
      };

      ctrl.selectResult(result);

      expect($state.go).toHaveBeenCalled();
      expect($state.go.calls.argsFor(0)).toEqual([
        'main.enterprise.organizations.organization.project',
        { org: 'fooOrg', project: 'barProject' },
        { reload: true }
      ]);
    });
  });

  describe('getFormattedItem()', () => {
      let $state;

      beforeEach(() => {
        $state = { go: jasmine.createSpy('go') };
      });
      
      it('should contain both the organization and project name', () => {
        let ctrl = createController({ $state: $state });
        
        let result = {
          id: 3,
          org: 'fooOrg',
          project: 'barProject'
        };

        let item = ctrl.getFormattedItem(result);

        expect(item).toContain(result.org);
        expect(item).toContain(result.project);
      });
    });
});
