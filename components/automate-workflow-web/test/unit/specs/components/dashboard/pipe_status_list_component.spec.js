import ng from 'angular';
import 'angular-mocks';
import pipeStatusListComponent
  from '../../../../../src/components/dashboard/pipe_status_list/pipe_status_list';

describe('pipeStatusListComponent', () => {
  let scope, isolateScope, element;

  beforeEach(ng.mock.module(pipeStatusListComponent, ($controllerProvider) => {
    $controllerProvider.register('DashboardController', ($scope) => {

    });
  }));

  beforeEach(inject(($compile, $rootScope) => {
    scope = $rootScope.$new();
    scope.pipeStatus = {
      acceptedChanges: [
        {
          'id': '0432da7a-f8f4-4555-9aaf-f8e5e5cc5539',
          'title': 'Unsanitize version from Env in Smoke',
          'org': 'Chef_Delivery',
          'project': 'delivery',
          'stage': 'union',
          'stage_status': 'passed'
        },
        {
          'id': 'a4f7b370-63d6-44c8-b0ae-6efc588f22d6',
          'title': 'Lock ui-router to 0.2.13',
          'org': 'Chef_Delivery',
          'project': 'delivery',
          'stage': 'rehearsal',
          'stage_status': 'running'
        },
        {
          'id': 'e5de2039-e3cc-42b4-b317-3a8853a2713f',
          'title': 'Reorder software definitions',
          'org': 'Chef_Delivery',
          'project': 'delivery',
          'stage': 'delivered',
          'stage_status': 'failed'
        }
      ],
      activeProjects: [
        {
          key: 'Chef_Delivery/delivery',
          org: 'Chef_Delivery',
          name: 'delivery',
          changes:  [
            {
              'id': 'a4f7b370-e3cc-42b4-b317-3a8853a2713f',
              'title': 'Reorder software definitions',
              'org': 'Chef_Delivery',
              'project': 'delivery',
              'stage': 'verify',
              'stage_status': 'failed'
            },
            {
              'id': 'e5de2039-f8f4-4555-9aaf-f8e5e5cc5539',
              'title': 'Unsanitize version from Env in Smoke',
              'org': 'Chef_Delivery',
              'project': 'delivery',
              'stage': 'acceptance',
              'stage_status': 'passed'
            }
          ]
        },
        {
          key: 'sandbox/delivery',
          org: 'sandbox',
          name: 'delivery',
          changes: [
            {
              'id': '0432da7a-e3cc-42b4-b317-3a8853a2713f',
              'title': 'Lock ui-router to 0.2.13',
              'org': 'sandbox',
              'project': 'delivery',
              'stage': 'build',
              'stage_status': 'running'
            }
          ]
        }
      ]
    };

    element = $compile(ng.element('<div cd-pipe-status-list pipe-status="pipeStatus"></div>'))(scope);

    scope.$digest();

    isolateScope = element.isolateScope();
  }));

  describe('accepted change rows', () => {

    it('renders the correct number of accepted change rows', () => {
      let trs = ng.element(element[0].querySelectorAll('.urd-items'));
      expect(trs.length).toEqual(3);
    });
  });

  describe('active project rows', () => {

    it('renders the correct number of accepted change rows', () => {
      let tbodies = ng.element(element[0].querySelectorAll('.project-items'));
      expect(tbodies.length).toEqual(2);
    });
  });
});
