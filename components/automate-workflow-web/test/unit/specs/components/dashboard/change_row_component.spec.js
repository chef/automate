import ng from 'angular';
import 'angular-mocks';
import changeRowComponent
  from '../../../../../src/components/dashboard/change_row/change_row';

describe('changeRowComponent', () => {

  let scope, element, isolateScope;

  beforeEach(ng.mock.module(changeRowComponent));

  beforeEach(inject(($compile, $rootScope) => {
    scope = $rootScope.$new();

    scope.change = {
      approved_by: "duffieldt",
      delivered_by: null,
      id: "df0420bc-deaf-4515-a7ed-5c3cb6c25e80",
      org: "Chef_Delivery",
      project: "delivery",
      stage: "acceptance",
      stage_status: "passed",
      submitted_at: "2015-06-15 23:47:02",
      submitter: "duffieldt",
      title: "Add deliv_scope and deliv_scm modules"
    };

    element = $compile(ng.element('<tr class="change-rows" cd-change-row change="change">'))(scope);
    isolateScope = element.isolateScope();
    scope.$digest();
  }));

});
