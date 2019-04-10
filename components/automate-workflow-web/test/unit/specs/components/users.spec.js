import ng from 'angular';
import 'angular-mocks';
import UsersComponent
  from '../../../../src/components/users/users';
import User from '../../../../src/common/models/user';
import Users from '../../../../src/common/users/users';

describe('usersComponent', () => {
  let scope, element, isolateScope;

  beforeEach(ng.mock.module(UsersComponent));
  beforeEach(ng.mock.module(User));

  function createDirective(User, $compile, $rootScope) {
    scope = $rootScope.$new();
    scope.users = User.$collection();

    element = $compile(ng.element('<span cd-users users="users"></span>'))(scope);
    isolateScope = element.isolateScope();
    scope.$digest();
  }

  beforeEach(inject(createDirective));

  describe('scope.userType', () => {

    it('scope.userType should initialize as "internal"', () => {
      expect(isolateScope.userType).toEqual('internal');
    });

    it('selectUserType(external) should set scope.userType to "external"', () => {
      isolateScope.selectUserType('external');
      expect(isolateScope.userType).toEqual('external');
    });

    it('selectUserType(saml) should set scope.userType to "saml"', () => {
      isolateScope.selectUserType('saml');
      expect(isolateScope.userType).toEqual('saml');
    });
  });

  describe('scope.toggleNewUserForm()', () => {

    it('scope.toggleNewUserForm should be false initially', () => {
      expect(isolateScope.showNewUserForm).toEqual(false);
    });

    it('scope.toggleNewUserForm() should set showNewUserForm to true', () => {
      expect(isolateScope.user).toEqual(undefined);
      isolateScope.toggleNewUserForm();
      expect(isolateScope.showNewUserForm).toEqual(true);
      expect(isolateScope.user).toEqual({ roles: [] });
    });

    it('running scope.toggleNewUserForm() again should set showNewUserForm to false', () => {
      isolateScope.toggleNewUserForm();
      isolateScope.toggleNewUserForm();
      expect(isolateScope.showNewUserForm).toEqual(false);
    });
  });

   describe('scope.cancel()', () => {
    it('sets scope.showNewUserForm to false', () => {
      isolateScope.cancel();
      expect(isolateScope.showNewUserForm).toEqual(false);
    });
  });
});
