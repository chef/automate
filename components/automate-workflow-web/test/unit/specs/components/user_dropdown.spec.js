import ng from 'angular';
import 'angular-mocks';
import UserDropdown
  from '../../../../src/components/user_dropdown/user_dropdown';
import licenseModalTemplate
  from '../../../../src/common/ui/modal/license_modal.html';

describe('userDropdownComponent', () => {
  let scope, element, currentUser, httpBackend;
  let versionResponse = 'delivery 0.6.47\n';

  beforeEach(ng.mock.module(UserDropdown, ($provide) => {
    $provide.value('CurrentUser', { user: () => currentUser });
    $provide.factory('welcomeModalDirective', () => {
      return { $get: () => {} };
    });
  }));

  function createDirective() {
    return inject(($compile, $rootScope, $httpBackend) => {
      currentUser = { first: 'rainbow', last: 'dash' };
      httpBackend = $httpBackend;
      httpBackend
        .when('GET', '/workflow/status/version')
        .respond(versionResponse);
      httpBackend
        .when('GET', '/elasticsearch/node-state/_count')
        .respond([]);
      scope = $rootScope.$new();
      element = $compile(ng.element('<span cd-user-dropdown></span>'))(scope);
    });
  }

  it('is hidden by default', () => {
    createDirective();
    expect(scope.dropdownShowing).toEqual(false);
  });

  describe('scope.getBuildVersion()', () => {
    describe('if the http GET is successful', () => {
      it('gets the current build version', () => {
        createDirective();
        httpBackend.flush();
        expect(scope.buildVersion).toBe('0.6.47');
      });
    });

    describe('if the http GET is unsuccessful', () => {
      beforeEach(() => {
        versionResponse = 404;
      });

      afterEach(() => {
        versionResponse = 'delivery 0.6.47\n';
      })

      it('gets the current build version', () => {
        createDirective();
        httpBackend.flush();
        expect(scope.buildVersion).toBe('0.0.1');
      });
    });
  });

  describe('scope.toggleDropdown()', () => {

    it('toggles the dropdown', () => {
      createDirective();
      scope.toggleDropdown();
      expect(scope.dropdownShowing).toEqual(true);
      scope.toggleDropdown();
      expect(scope.dropdownShowing).toEqual(false);
    });
  });

  describe('scope.logout()', () => {

    beforeEach(inject(($state, Auth, Store) => {
      createDirective();
      spyOn(Auth, 'unAuthenticate');
      spyOn(Store, 'clear');
      spyOn($state, 'go');
    }));

    it('removes the user session', inject((Auth, Store) => {
      scope.logout();
      expect(Auth.unAuthenticate).toHaveBeenCalled();
      expect(Store.clear).toHaveBeenCalledWith('session');
    }));

    it('redirects to the login view', inject(($state) => {
      scope.logout();
      expect($state.go).not.toHaveBeenCalledWith('authenticate.login');
    }));
  });

  describe('scope.openLicense()', () => {

    it('opens the license agreement modal', inject((Modal) => {
      createDirective();
      spyOn(Modal, 'open');

      scope.openLicense();

      expect(Modal.open).toHaveBeenCalledWith(
        'Chef Automate License Information',
        licenseModalTemplate,
        'default-modal',
        scope
      );
    }));
  });
});
