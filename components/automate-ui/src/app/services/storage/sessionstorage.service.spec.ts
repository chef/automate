import { SessionStorageService } from './sessionstorage.service';
import { ChefSessionService } from '../chef-session/chef-session.service';

describe('sessionStorageService', () => {
  let sessionStorageService: SessionStorageService;

  beforeEach(() => {
    sessionStorageService = new SessionStorageService(<ChefSessionService>{uuid: 'Bob'});
  });

  describe('Boolean', () => {

    it('can store a Boolean', () => {
      sessionStorageService.putBoolean('test', true);
      // boolean key is prepended by username
      expect(sessionStorage.getItem('Bob-test')).toBeDefined();
    });

    it('can return a Boolean', () => {
      sessionStorageService.putBoolean('test2', true);
      expect(sessionStorageService.getBoolean('test2')).toBe(true);
    });
  });
});
