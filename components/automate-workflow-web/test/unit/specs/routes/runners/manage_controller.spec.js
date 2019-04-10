import ng from 'angular';
import 'angular-mocks';
import manageController
  from '../../../../../src/routes/runners/manage/manage_controller';

describe('manageController', () => {
  let scope, createController, runners, newRunners, $httpParamSerializer,
      $window, $document, mockEventSource, $http, $httpBackend;

  beforeEach(ng.mock.module(manageController));

  beforeEach(inject(($controller, $rootScope, $compile, _$httpParamSerializer_,
                     _$window_, _$document_, _$http_, _$httpBackend_) => {
    scope = $rootScope.$new();
    runners = [
            {
                id: '1234',
                hostname: 'number.one.runner',
                job: [],
                openssh_public_key: 'ssh-rsa AAAAB3NZAc1pHtRbZQkmVfZr1q== job_runner@number.one.runner\n',
                health: {
                    status: 'ok',
                    command_output: 'Executing remotely on [workflow_runner]...\r\nSuccess!\r\nConnection to workflow_runner closed.\r\n'
                }
            },
            {
                id: 'abcd',
                hostname: 'number.two.runner',
                job: [],
                openssh_public_key: 'ssh-rsa AAAAB3NzaC1PhTrBzqKMvFzR1Q== job_runner@number.two.runner\n',
                health: {
                    status: 'ok',
                    command_output: 'Executing remotely on [workflow_runner]...\r\nSuccess!\r\nConnection to workflow_runner closed.\r\n'
                }
            }
        ];
    let Session = { get: function() { return {enterprise: 'cd', username: 'foo', token: 'bar'}; }};
    $httpParamSerializer = _$httpParamSerializer_;
    $window = _$window_;
    $document = _$document_;
    $http = _$http_;
    $httpBackend = _$httpBackend_;
    createController = () => {
      return $controller('manageController', {
        $scope: scope,
        runners: runners,
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
    let streamPath = `/workflow/api/v0/e/cd/runners/streaming`;
    let streamParams = $httpParamSerializer({
      'chef-delivery-user': 'foo',
      'chef-delivery-token': 'bar'
    });
    return `${streamPath}?${streamParams}`;
  };

  describe('setup', () => {

    it('should attach the runners statuses to the scope', () => {
      createController();
      expect(scope.runners).toEqual(runners);
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

    it('should listen to the stream for "runners_state_updated" events', () => {
      createController();

      expect(mockEventSource.addEventListener)
        .toHaveBeenCalledWith('runners_state_updated', scope.handleRunnersUpdateEvent);
    });
  });

  describe('handleRunnersUpdateEvent', () => {

    it('should update the runners when JSON is an empty string', () => {
      createController();
      let json = [];

      scope.handleRunnersUpdateEvent({ data: JSON.stringify(json) });

      expect(scope.runners).toEqual([]);
    });

    it('should update the runners from provided JSON', () => {
      let json = [
              {
                  id: '1234',
                  hostname: 'number.one.runner',
                  job: [],
                  openssh_public_key: 'ssh-rsa AAAAB3NZAc1pHtRbZQkmVfZr1q== job_runner@number.one.runner\n',
                  health: {
                      status: 'ok',
                      command_output: 'Executing remotely on [workflow_runner]...\r\nSuccess!\r\nConnection to workflow_runner closed.\r\n'
                  }
              },
              {
                  id: 'abcd',
                  hostname: 'number.two.runner',
                  job: {
                      id: 'a1b2c3d4',
                      title: 'Make a change!',
                      org: 'products',
                      project: 'automate'
                  },
                  openssh_public_key: 'ssh-rsa AAAAB3NzaC1PhTrBzqKMvFzR1Q== job_runner@number.two.runner\n',
                  health: {
                      status: 'ok',
                      command_output: 'Executing remotely on [workflow_runner]...\r\nSuccess!\r\nConnection to workflow_runner closed.\r\n'
                  }
              }
          ];
      createController();

      scope.handleRunnersUpdateEvent({ data: JSON.stringify(json) });

      expect(scope.runners).toEqual(json);
    });

    it('should update the runners from provided JSON when the incoming_data is longer than the existing list', () => {
      let json = [
              {
                  id: '1234',
                  hostname: 'number.one.runner',
                  job: [],
                  openssh_public_key: 'ssh-rsa AAAAB3NZAc1pHtRbZQkmVfZr1q== job_runner@number.one.runner\n',
                  health: {
                      status: 'ok',
                      command_output: 'Executing remotely on [workflow_runner]...\r\nSuccess!\r\nConnection to workflow_runner closed.\r\n'
                  }
              },
              {
                  id: 'efgh',
                  hostname: 'number.two.runner',
                  job: {
                      id: 'a1b2c3d4',
                      title: 'Make a change!',
                      org: 'products',
                      project: 'automate'
                  },
                  openssh_public_key: 'ssh-rsa AAAAB3NzaC1PhTrBzqKMvFzR1Q== job_runner@number.two.runner\n',
                  health: {
                      status: 'ok',
                      command_output: 'Executing remotely on [workflow_runner]...\r\nSuccess!\r\nConnection to workflow_runner closed.\r\n'
                  }
              },
              {
                  id: 'abcd',
                  hostname: 'number.two.runner',
                  job: {
                      id: 'a1b2c3d4',
                      title: 'Make a change!',
                      org: 'products',
                      project: 'automate'
                  },
                  openssh_public_key: 'ssh-rsa AAAAB3NzaC1PhTrBzqKMvFzR1Q== job_runner@number.two.runner\n',
                  health: {
                      status: 'ok',
                      command_output: 'Executing remotely on [workflow_runner]...\r\nSuccess!\r\nConnection to workflow_runner closed.\r\n'
                  }
              }
          ];

          let expectedJson = [
                  {
                      id: '1234',
                      hostname: 'number.one.runner',
                      job: [],
                      openssh_public_key: 'ssh-rsa AAAAB3NZAc1pHtRbZQkmVfZr1q== job_runner@number.one.runner\n',
                      health: {
                          status: 'ok',
                          command_output: 'Executing remotely on [workflow_runner]...\r\nSuccess!\r\nConnection to workflow_runner closed.\r\n'
                      }
                  },
                  {
                      id: 'efgh',
                      hostname: 'number.two.runner',
                      job: {
                          id: 'a1b2c3d4',
                          title: 'Make a change!',
                          org: 'products',
                          project: 'automate'
                      },
                      openssh_public_key: 'ssh-rsa AAAAB3NzaC1PhTrBzqKMvFzR1Q== job_runner@number.two.runner\n',
                      health: {
                          status: 'ok',
                          command_output: 'Executing remotely on [workflow_runner]...\r\nSuccess!\r\nConnection to workflow_runner closed.\r\n'
                      }
                  },
                  {
                      id: 'abcd',
                      hostname: 'number.two.runner',
                      job: {
                          id: 'a1b2c3d4',
                          title: 'Make a change!',
                          org: 'products',
                          project: 'automate'
                      },
                      openssh_public_key: 'ssh-rsa AAAAB3NzaC1PhTrBzqKMvFzR1Q== job_runner@number.two.runner\n',
                      health: {
                          status: 'ok',
                          command_output: 'Executing remotely on [workflow_runner]...\r\nSuccess!\r\nConnection to workflow_runner closed.\r\n'
                      }
                  }
              ];
      createController();

      scope.handleRunnersUpdateEvent({ data: JSON.stringify(json) });

      expect(scope.runners).toEqual(expectedJson);
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
