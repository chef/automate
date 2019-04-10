import ng from 'angular';
import 'angular-mocks';
import authService from '../../../../../src/common/auth/auth_service';
import CurrentUser from '../../../../../src/common/auth/current_user';
import Session from '../../../../../src/common/auth/session';

describe('Auth', () => {
  let Auth, httpBackend, currentUser, session, rootScope;

  beforeEach(ng.mock.module(authService, ($provide) => {
    $provide.value('ApiUrl', (url) => url);
  }));

  beforeEach(ng.mock.module(CurrentUser));
  beforeEach(ng.mock.module(Session));

  beforeEach(inject((_Auth_, $httpBackend, CurrentUser, Session) => {
    Auth = _Auth_;
    currentUser = CurrentUser;
    session = Session;
    httpBackend = $httpBackend;
  }));

  afterEach(() => {
    httpBackend.verifyNoOutstandingExpectation();
    httpBackend.verifyNoOutstandingRequest();
  });

  describe('isAuthenticated', () => {
    let promiseResolved, promiseRejected;

    beforeEach(() => {
      promiseResolved = false;
      promiseRejected = false;
    });

    it('should reject promise if no session exists', inject(($rootScope) => {
      spyOn(session, 'hasSession').and.returnValue(false);

      Auth.isAuthenticated()
        .then(() => { promiseResolved = true; })
        .catch(() => { promiseRejected = true; });

      // Propagate promise resolution to 'then' functions using $apply().
      $rootScope.$apply();

      expect(promiseResolved).toBe(false);
      expect(promiseRejected).toBe(true);
    }));

    it('should resolve promise if a user is authenticated', () => {
      spyOn(session, 'hasSession').and.returnValue(true);
      httpBackend.when('HEAD', '/verify-token').respond(200);

      Auth.isAuthenticated()
        .then(() => { promiseResolved = true; })
        .catch(() => { promiseRejected = true; });

      httpBackend.flush();

      expect(promiseResolved).toBe(true);
      expect(promiseRejected).toBe(false);
    });

    it('should reject promise if a user is not authenticated', () => {
      spyOn(session, 'hasSession').and.returnValue(true);
      httpBackend.when('HEAD', '/verify-token').respond(401);

      Auth.isAuthenticated()
        .then(() => { promiseResolved = true; })
        .catch(() => { promiseRejected = true; });

      httpBackend.flush();

      expect(promiseResolved).toBe(false);
      expect(promiseRejected).toBe(true);
    });
  });

  describe('unAuthenticate', () => {
    it('should remove the session and revoke the token when a session is present', () => {
      spyOn(session, 'remove');
      spyOn(session, 'hasSession').and.returnValue(true);
      spyOn(session, 'get').and.returnValue({username: 'foo'});
      spyOn(currentUser, 'removeUser');
      spyOn(window.location, 'assign');

      Auth.unAuthenticate();

      expect(session.remove).toHaveBeenCalled();
      expect(currentUser.removeUser).toHaveBeenCalled();
      expect(window.location.assign).toHaveBeenCalled();
    });

    it('should be a no-op when no session is present', () => {
      spyOn(session, 'hasSession').and.returnValue(false);
      spyOn(window.location, 'assign');

      Auth.unAuthenticate();
    });
  });
});
