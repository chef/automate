import ng from 'angular';
import 'angular-mocks';
import CurrentUser
  from '../../../../../src/common/auth/current_user';
import User from '../../../../../src/common/models/user';

describe('CurrentUser', () => {
  let MockCurrentUser, sessionObj, httpBackend;

  beforeEach(ng.mock.module(CurrentUser, ($provide) => {
    $provide.value('Session', {
      get: () => sessionObj,
    });
    $provide.value('ApiUrl', () => '/authz/users/dm');
  }));

  beforeEach(inject((CurrentUser) => {
    MockCurrentUser = CurrentUser;
    sessionObj = { username: 'applejack' };
  }));

  it('user() should return the currentUser', () => {
    expect(MockCurrentUser.user()).toBeDefined();
  });

  describe('displayName', () => {

    describe ('when both first and last names exist', () => {

      it('is the first and last name', () => {
        inject(($httpBackend) => {

          $httpBackend
            .when('GET', /users/)
            .respond({ name: 'applejack', first: 'Apple', last: 'Jack' });

          let user = MockCurrentUser.user();
          $httpBackend.flush();

          expect(user.displayName).toBe('Apple Jack');
        })
      });
    });

    describe ('when neither first nor last name exists', () => {
      
      it('is the username', () => {
        inject(($httpBackend) => {

          $httpBackend
            .when('GET', /users/)
            .respond({ name: 'applejack' });

          let user = MockCurrentUser.user();
          $httpBackend.flush();

          expect(user.displayName).toBe('applejack');
        })
      });
    });

    describe ('when only first or last name exists', () => {
      
      it('is the username', () => {
        inject(($httpBackend) => {

          $httpBackend
            .when('GET', /users/)
            .respond({ name: 'applejack', first: 'Apple' });

          let user = MockCurrentUser.user();
          $httpBackend.flush();
          
          expect(user.displayName).toBe('applejack');
        })
      });
    });
  });
});
