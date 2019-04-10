import ng from 'angular';
import 'angular-mocks';
import dashboardController
  from '../../../../../src/routes/dashboard/dashboard_controller';

describe('dashboardController', () => {
  let scope, createController, httpBackend, window, pipeStats, pipeStatus,
      sessionObj, ApiUrl, Modal, location, clipboard;

  beforeEach(ng.mock.module(dashboardController, ($provide) => {
    $provide.decorator('Session', ($delegate) => {
      $delegate.get = (param) => {
        if (param) {
          return sessionObj[param];
        }
        return sessionObj;
      };
      return $delegate;
    });
  }));

  beforeEach(inject(($controller, $rootScope, $httpBackend, $compile, $window, $location, _Modal_, _clipboard_) => {
    scope = $rootScope.$new();

    ApiUrl = jasmine
      .createSpy('ApiUrl')
      .and.returnValue('/api/v0/e/Foobar/pipeline_status');

    location = $location;
    Modal = _Modal_;
    clipboard = _clipboard_;
    sessionObj = {};

    createController = () => {
      return $controller('dashboardController', {
        $scope: scope,
        ApiUrl: ApiUrl,
        Modal: Modal,
        $location: location,
        clipboard: _clipboard_
      });
    };

    httpBackend = $httpBackend;

    window = $window;
    window.sessionStorage.clear();

    pipeStats = {
      'verify': {
        'running': 0,
        'passed': 0,
        'failed': 1
      },
      'build': {
        'running': 1,
        'failed': 0
      },
      'acceptance': {
        'running': 0,
        'passed': 1,
        'failed': 0
      },
      'union': 'passed',
      'rehearsal': 'running',
      'delivered': 'failed'
    };

    pipeStatus = [
      {
        'id': '0432da7a-f8f4-4555-9aaf-f8e5e5cc5539',
        'title': 'Unsanitize version from Env in Smoke',
        'org': 'Chef_Delivery',
        'project': 'delivery',
        'submitted_at': '2015-01-02 00:11:22',
        'submitter': 'joe',
        'stage': 'union',
        'stage_status': 'passed',
        'includes': []
      },
      {
        'id': 'a4f7b370-63d6-44c8-b0ae-6efc588f22d6',
        'title': 'Lock ui-router to 0.2.13',
        'org': 'Chef_Delivery',
        'project': 'delivery',
        'submitted_at': '2015-01-03 00:12:23',
        'submitter': 'joe',
        'stage': 'rehearsal',
        'stage_status': 'running',
        'includes': []
      },
      {
        'id': 'e5de2039-e3cc-42b4-b317-3a8853a2713f',
        'title': 'Reorder software definitions',
        'org': 'Chef_Delivery',
        'project': 'delivery',
        'submitted_at': '2015-01-04 00:13:24',
        'submitter': 'joe',
        'stage': 'delivered',
        'stage_status': 'failed',
        'includes': []
      },
      {
        'id': 'a4f7b370-e3cc-42b4-b317-3a8853a2713f',
        'title': 'Reorder software definitions',
        'org': 'Chef_Delivery',
        'project': 'delivery',
        'submitted_at': '2015-01-05 00:14:25',
        'submitter': 'joe',
        'stage': 'verify',
        'stage_status': 'failed',
        'includes': []
      },
      {
        'id': '0432da7a-e3cc-42b4-b317-3a8853a2713f',
        'title': 'Lock ui-router to 0.2.13',
        'org': 'sandbox',
        'project': 'delivery',
        'submitted_at': '2015-01-06 00:15:26',
        'submitter': 'joe',
        'stage': 'build',
        'stage_status': 'running',
        'includes': []
      },
      {
        'id': 'e5de2039-f8f4-4555-9aaf-f8e5e5cc5539',
        'title': 'Unsanitize version from Env in Smoke',
        'org': 'Chef_Delivery',
        'project': 'delivery',
        'submitted_at': '2015-01-07 00:16:27',
        'submitter': 'joe',
        'stage': 'acceptance',
        'stage_status': 'passed',
        'includes': []
      }
    ];
    $httpBackend
      .when('GET', '/api/v0/e/Foobar/pipeline_status')
      .respond(pipeStatus);
  }));

  it('fetches accepted changes', () => {
    httpBackend.expectGET('/api/v0/e/Foobar/pipeline_status');
    let ctrl = createController();
    httpBackend.flush();

    let expected = [
      {
        'id': '0432da7a-f8f4-4555-9aaf-f8e5e5cc5539',
        'title': 'Unsanitize version from Env in Smoke',
        'org': 'Chef_Delivery',
        'project': 'delivery',
        'submitted_at': '2015-01-02 00:11:22',
        'submitter': 'joe',
        'stage': 'union',
        'stage_status': 'passed',
        'includes': []
      },
      {
        'id': 'a4f7b370-63d6-44c8-b0ae-6efc588f22d6',
        'title': 'Lock ui-router to 0.2.13',
        'org': 'Chef_Delivery',
        'project': 'delivery',
        'submitted_at': '2015-01-03 00:12:23',
        'submitter': 'joe',
        'stage': 'rehearsal',
        'stage_status': 'running',
        'includes': []
      },
      {
        'id': 'e5de2039-e3cc-42b4-b317-3a8853a2713f',
        'title': 'Reorder software definitions',
        'org': 'Chef_Delivery',
        'project': 'delivery',
        'submitted_at': '2015-01-04 00:13:24',
        'submitter': 'joe',
        'stage': 'delivered',
        'stage_status': 'failed',
        'includes': []
      }
    ];
    expect(scope.pipeStatus.acceptedChanges).toEqual(expected);
  });

  it('constructs pipeline stats', () => {
    let ctrl = createController();
    httpBackend.flush();
    expect(scope.pipeStats).toEqual(pipeStats);
  });

  it('fetches active changes with correct projectStats', () => {
    httpBackend.expectGET('/api/v0/e/Foobar/pipeline_status');
    let ctrl = createController();
    httpBackend.flush();

    let expected = [
      {
        key: 'Chef_Delivery/delivery',
        name: 'delivery',
        open: false,
        org: 'Chef_Delivery',
        changes:  [
          {
            'id': 'a4f7b370-e3cc-42b4-b317-3a8853a2713f',
            'title': 'Reorder software definitions',
            'org': 'Chef_Delivery',
            'project': 'delivery',
            'submitted_at': '2015-01-05 00:14:25',
            'submitter': 'joe',
            'stage': 'verify',
            'stage_status': 'failed',
            'includes': []
          },
          {
            'id': 'e5de2039-f8f4-4555-9aaf-f8e5e5cc5539',
            'title': 'Unsanitize version from Env in Smoke',
            'org': 'Chef_Delivery',
            'project': 'delivery',
            'submitted_at': '2015-01-07 00:16:27',
            'submitter': 'joe',
            'stage': 'acceptance',
            'stage_status': 'passed',
            'includes': []
          }
        ],
        stats: {
          verify: {
            running: 0,
            passed: 0,
            failed: 1
          },
          build: {
            running: 0,
            failed: 0
          },
          acceptance: {
            running: 0,
            passed: 1,
            failed: 0
          }
        }
      },
      {
        key: 'sandbox/delivery',
        name: 'delivery',
        org: 'sandbox',
        open: false,
        changes: [
          {
            'id': '0432da7a-e3cc-42b4-b317-3a8853a2713f',
            'title': 'Lock ui-router to 0.2.13',
            'org': 'sandbox',
            'project': 'delivery',
            'submitted_at': '2015-01-06 00:15:26',
            'submitter': 'joe',
            'stage': 'build',
            'stage_status': 'running',
            'includes': []
          }
        ],
        stats: {
          verify: {
            running: 0,
            passed: 0,
            failed: 0
          },
          build: {
            running: 1,
            failed: 0
          },
          acceptance: {
            running: 0,
            passed: 0,
            failed: 0
          }
        }
      }
    ];
    expect(scope.pipeStatus.activeProjects).toEqual(expected);
  });

  describe('filtering', () => {

    beforeEach(() => {
      let ctrl = createController();
      httpBackend.flush();

      scope.filterText = 'stage:verify status:failed';
      scope.$digest();
    });

    it('limits the list of changes and projects', () => {
      expect(scope.pipeStatus.acceptedChanges.length).toEqual(0);
      expect(scope.pipeStatus.activeProjects.length).toEqual(1);

      let filteredChange = scope.pipeStatus.activeProjects[0].changes[0];
      expect(filteredChange.stage).toBe('verify');
      expect(filteredChange.stage_status).toBe('failed');
    });

    it('writes to session storage', inject((Store) => {
      expect(Store.get('dashboard.filter')).toEqual('stage:verify status:failed');
    }));
  });

  describe('project selection', () => {
    let selectedProject;

    beforeEach(() => {
      let ctrl = createController();
      httpBackend.flush();

      selectedProject = scope.pipeStatus.activeProjects[0];
      selectedProject.open = true;

      scope.$emit('toggleProject', selectedProject);
      scope.$digest();
    });

    it('writes to session storage', inject((Store) => {
      expect(Store.get('dashboard.selected')).toEqual(["Chef_Delivery/delivery"]);
    }));
  });

  describe('when token info display query param is present', () => {
    beforeEach(() => {
      spyOn(location, 'search').and.returnValue({ token: true });
      spyOn(Modal, 'open');
      sessionObj = { token: 'dG9rZW4=' };
      let ctrl = createController();
      httpBackend.flush();
    });


    it('calls location.search() to see if it should display token info modal', () => {
      expect(location.search).toHaveBeenCalled();
    });

    it('opens a token info modal', () => {
      expect(Modal.open).toHaveBeenCalled();
    });

    it('fetches the token from Session information', () => {
      expect(scope.token).toEqual('dG9rZW4=');
    });

    describe('and the copy button is clicked', () => {
      beforeEach(() => {
        spyOn(clipboard, 'copyText');
        scope.copy();
      });

      it('copies the token to clipboard', () => {
        expect(clipboard.copyText).toHaveBeenCalledWith("dG9rZW4=");
      });
    });

    describe('and the close button is clicked', () => {
      beforeEach(() => {
        spyOn(Modal, 'close');
        scope.closeModal();
      });

      it('hides the token info modal', () => {
        expect(Modal.close).toHaveBeenCalled();
      });
    });
  });
});
