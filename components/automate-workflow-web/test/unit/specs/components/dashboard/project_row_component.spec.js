import ng from 'angular';
import 'angular-mocks';
import projectRowComponent
  from '../../../../../src/components/dashboard/project_row/project_row';

describe('projectRowComponent', () => {
  let scope, isolateScope, element, mockEvent;

  beforeEach(ng.mock.module(projectRowComponent));

  beforeEach(inject(($compile, $rootScope, Store) => {
    scope = $rootScope.$new();
    scope.project = {
      name: 'delivery',
      changes:  [
        {
          approved_by: "duffieldt",
          delivered_by: null,
          id: "df0420bc-deaf-4515-a7ed-5c3cb6c25e80",
          org: "Chef_Delivery",
          project: "delivery",
          stage: "acceptance",
          stage_status: "passed",
          submitted_at: "2015-06-15 23:47:02",
          submitter: "duffieldt",
          title: "Add deliv_scope and deliv_scm modules",
        },
        {
          approved_by: null,
          delivered_by: null,
          id: "cbc58d44-c2bc-4e5e-8ac4-62631cba14f6",
          org: "Chef_Delivery",
          project: "delivery",
          stage: "verify",
          stage_status: "failed",
          submitted_at: "2015-06-16 15:26:17",
          submitter: "seth",
          title: "Add script for license key generation"
        },
        {
          approved_by: null,
          delivered_by: null,
          id: "c88ad4fd-7e2f-47f1-a5ec-00ccbfd1b4d7",
          org: "Chef_Delivery",
          project: "delivery",
          stage: "verify",
          stage_status: "passed",
          submitted_at: "2015-06-16 00:41:46",
          submitter: "wk",
          title: "First pass to make Delivery talk to github API"
        }
      ]
    };

    element = $compile(ng.element('<tbody cd-project-row project="project"></tbody>'))(scope);
    isolateScope = element.isolateScope();
    scope.$digest();
  }));

  describe('toggle()', () => {

    beforeEach(() => {
      mockEvent = {
        stopPropagation: () => {}
      };
    });

    it('toggles the open state of the project', () => {
      isolateScope.toggle();
      expect(isolateScope.project.open).toEqual(true);

      isolateScope.toggle();
      expect(isolateScope.project.open).toEqual(false);
    });

    it('emits a toggle event', () => {
      spyOn(isolateScope, '$emit');
      isolateScope.toggle();

      expect(isolateScope.$emit).toHaveBeenCalledWith('toggleProject', scope.project);
    });

    it('emits a filter-click event', () => {
      spyOn(isolateScope, '$emit');
      spyOn(mockEvent, 'stopPropagation');
      isolateScope.applyFilter(mockEvent, 'stage:verify status:running');

      expect(mockEvent.stopPropagation).toHaveBeenCalled();
      expect(isolateScope.$emit).toHaveBeenCalledWith('filterClick', 'project:delivery stage:verify status:running');
    });
  });
});
