import ng from 'angular';
import 'angular-mocks';
import queueController
  from '../../../../../src/routes/runners/queue/queue_controller';

describe('queueController', () => {
  let scope, createController, jobs, session, $httpParamSerializer, $window,
    $document, mockEventSource, $http, $httpBackend;

  beforeEach(ng.mock.module(queueController));

  beforeEach(inject(($controller, $rootScope, $compile, _$httpParamSerializer_,
                     _$window_, _$document_, _$http_, _$httpBackend_) => {
    scope = $rootScope.$new();
    jobs = [
      {
        "id": "abcd",
        "project": "test_proj15801",
        "change": {
          "id": "918fc332-1f33-4d18-9285-1abe847240d1",
          "title": "New pipeline verification commit"
        },
        "org": "test",
        "stage": "verify",
        "phase": "syntax",
        "status": "pending",
        "submittedAt": "2017-06-20 22:11:04",
        "timeInState": "00:00:31"
      },
      {
        "id": "1234",
        "project": "test_proj15801",
        "change": {
          "id": "918fc332-1f33-4d18-9285-1abe847240d1",
          "title": "New pipeline verification commit"
        },
        "org": "test",
        "stage": "verify",
        "phase": "lint",
        "status": "pending",
        "submittedAt": "2017-06-20 22:11:04",
        "timeInState": "1d 18:58:33"
      },
      {
        "id": "5678",
        "project": "test_proj15801",
        "change": {
          "id": "918fc332-1f33-4d18-9285-1abe847240d1",
          "title": "New pipeline verification commit"
        },
        "org": "test",
        "stage": "verify",
        "phase": "unit",
        "status": "pending",
        "submittedAt": "2017-06-20 22:11:04",
        "timeInState": "1d 18:58:33"
      }
    ];
    let Session = { get: function() { return {enterprise: 'cd', username: 'foo', token: 'bar'}; }};
    $httpParamSerializer = _$httpParamSerializer_;
    $window = _$window_;
    $document = _$document_;
    $http = _$http_;
    $httpBackend = _$httpBackend_;
    createController = () => {
      return $controller('queueController', {
        $scope: scope,
        jobs: jobs,
        Session: Session
      });
    };
  }));

  // Need to mock out window.EventSource so we don't actually attempt to make
  // any real streaming connections. Plus we're going to need a mock to attach
  // spies to.
  beforeEach(() => {
    // Just need an object we can attach/dispatch events on
    mockEventSource = angular.extend($document[0].createElement('div'), {
      close: jasmine.createSpy('close')
    });
    spyOn($window, 'EventSource').and.returnValue(mockEventSource);
  });

  function getStreamUrl() {
    let streamPath = `/workflow/api/v0/e/cd/jobs/streaming`;
    let streamParams = $httpParamSerializer({
      'chef-delivery-user': 'foo',
      'chef-delivery-token': 'bar'
    });
    return `${streamPath}?${streamParams}`;
  };

  describe('setup', () => {
    it('should attach the job statuses to the scope', () => {
      createController();
      expect(scope.jobs).toEqual(jobs);
    });
  });

  describe('initStream', () => {
    beforeEach(() => {
      spyOn(mockEventSource, 'addEventListener');
    });

    it('should set up the window EventSource', () => {
      createController();
      expect($window.EventSource).toHaveBeenCalledWith(getStreamUrl());
    });

    it('should listen to the stream for "update_job_queue" events', () => {
      createController();
      expect(mockEventSource.addEventListener)
        .toHaveBeenCalledWith('update_job_queue', scope.handleUpdateJobQueueEvent);
    });
  });

  describe('handleUpdateJobQueueEvent', () => {

    it('should update the runners when JSON is an empty string', () => {
          createController();
      let json = [];

      scope.handleUpdateJobQueueEvent({ data: JSON.stringify(json) });

      expect(scope.jobs).toEqual([]);
    });

    it('should update the jobs from provided JSON', () => {
      let json = [
          {
            "id": "abcd",
            "project": "test_proj15801",
            "change": {
              "id": "918fc332-1f33-4d18-9285-1abe847240d1",
              "title": "New pipeline verification commit"
            },
            "org": "test",
            "stage": "verify",
            "phase": "syntax",
            "status": "running",
            "submittedAt": "2017-06-20 22:11:04",
            "timeInState": "00:00:34"
          },
          {
            "id": "5678",
            "project": "test_proj15801",
            "change": {
              "id": "918fc332-1f33-4d18-9285-1abe847240d1",
              "title": "New pipeline verification commit"
            },
            "org": "test",
            "stage": "verify",
            "phase": "unit",
            "status": "pending",
            "submittedAt": "2017-06-20 22:11:04",
            "timeInState": "1d 18:59:10"
          },
          {
            "id": "1234",
            "project": "test_proj15801",
            "change": {
              "id": "918fc332-1f33-4d18-9285-1abe847240d1",
              "title": "New pipeline verification commit"
            },
            "org": "test",
            "stage": "verify",
            "phase": "lint",
            "status": "running",
            "submittedAt": "2017-06-20 22:11:04",
            "timeInState": "00:00:12"
          }
        ];

        let expectedJson = [
            {
              "id": "abcd",
              "project": "test_proj15801",
              "change": {
                "id": "918fc332-1f33-4d18-9285-1abe847240d1",
                "title": "New pipeline verification commit"
              },
              "org": "test",
              "stage": "verify",
              "phase": "syntax",
              "status": "running",
              "submittedAt": "2017-06-20 22:11:04",
              "timeInState": "00:00:34"
            },
            {
              "id": "5678",
              "project": "test_proj15801",
              "change": {
                "id": "918fc332-1f33-4d18-9285-1abe847240d1",
                "title": "New pipeline verification commit"
              },
              "org": "test",
              "stage": "verify",
              "phase": "unit",
              "status": "pending",
              "submittedAt": "2017-06-20 22:11:04",
              "timeInState": "1d 18:59:10"
            },
            {
              "id": "1234",
              "project": "test_proj15801",
              "change": {
                "id": "918fc332-1f33-4d18-9285-1abe847240d1",
                "title": "New pipeline verification commit"
              },
              "org": "test",
              "stage": "verify",
              "phase": "lint",
              "status": "running",
              "submittedAt": "2017-06-20 22:11:04",
              "timeInState": "00:00:12"
            }
          ];

      createController();
      scope.handleUpdateJobQueueEvent({ data: JSON.stringify(json) });
      expect(scope.jobs).toEqual(expectedJson);
    });

    it('should update the jobs correctly when there are more jobs in the provided JSON', () => {
      let json = [
          {
            "id": "abcd",
            "project": "test_proj15801",
            "change": {
              "id": "918fc332-1f33-4d18-9285-1abe847240d1",
              "title": "New pipeline verification commit"
            },
            "org": "test",
            "stage": "verify",
            "phase": "syntax",
            "status": "running",
            "submittedAt": "2017-06-20 22:11:04",
            "timeInState": "00:00:31"
          },
          {
            "id": "5678",
            "project": "test_proj15801",
            "change": {
              "id": "918fc332-1f33-4d18-9285-1abe847240d1",
              "title": "New pipeline verification commit"
            },
            "org": "test",
            "stage": "verify",
            "phase": "unit",
            "status": "pending",
            "submittedAt": "2017-06-20 22:11:04",
            "timeInState": "1d 18:59:12"
          },
          {
            "id": "91011",
            "project": "test_proj15801",
            "change": {
              "id": "918fc332-1f33-4d18-9285-1abe847240d1",
              "title": "New pipeline verification commit"
            },
            "org": "test",
            "stage": "verify",
            "phase": "lint",
            "status": "pending",
            "submittedAt": "2017-06-20 22:11:20",
            "timeInState": "1d 18:59:12"
          },
          {
            "id": "1234",
            "project": "test_proj15801",
            "change": {
              "id": "918fc332-1f33-4d18-9285-1abe847240d1",
              "title": "New pipeline verification commit"
            },
            "org": "test",
            "stage": "verify",
            "phase": "lint",
            "status": "running",
            "submittedAt": "2017-06-20 22:11:04",
            "timeInState": "00:00:00"
          }
        ];

        let expectedJson = [
          {
            "id": "abcd",
            "project": "test_proj15801",
            "change": {
              "id": "918fc332-1f33-4d18-9285-1abe847240d1",
              "title": "New pipeline verification commit"
            },
            "org": "test",
            "stage": "verify",
            "phase": "syntax",
            "status": "running",
            "submittedAt": "2017-06-20 22:11:04",
            "timeInState": "00:00:31"
          },
          {
            "id": "5678",
            "project": "test_proj15801",
            "change": {
              "id": "918fc332-1f33-4d18-9285-1abe847240d1",
              "title": "New pipeline verification commit"
            },
            "org": "test",
            "stage": "verify",
            "phase": "unit",
            "status": "pending",
            "submittedAt": "2017-06-20 22:11:04",
            "timeInState": "1d 18:59:12"
          },
          {
            "id": "91011",
            "project": "test_proj15801",
            "change": {
              "id": "918fc332-1f33-4d18-9285-1abe847240d1",
              "title": "New pipeline verification commit"
            },
            "org": "test",
            "stage": "verify",
            "phase": "lint",
            "status": "pending",
            "submittedAt": "2017-06-20 22:11:20",
            "timeInState": "1d 18:59:12"
          },
          {
            "id": "1234",
            "project": "test_proj15801",
            "change": {
              "id": "918fc332-1f33-4d18-9285-1abe847240d1",
              "title": "New pipeline verification commit"
            },
            "org": "test",
            "stage": "verify",
            "phase": "lint",
            "status": "running",
            "submittedAt": "2017-06-20 22:11:04",
            "timeInState": "00:00:00"
          }
        ];

      createController();
      scope.handleUpdateJobQueueEvent({ data: JSON.stringify(json) });
      expect(scope.jobs).toEqual(expectedJson);
    });
  });

  describe('cleanup', () => {

    it('should close the streaming connection', () => {
      createController();
      scope.$broadcast('$destroy');
      expect(mockEventSource.close).toHaveBeenCalled();
    });
  });

});
