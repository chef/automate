import { LocalStorageService } from './localstorage.service';
import { ChefSessionService } from '../chef-session/chef-session.service';

describe('LocalStorageService', () => {
  let localStorageService: LocalStorageService;

  beforeEach(() => {
    localStorageService = new LocalStorageService(<ChefSessionService>{uuid: 'Bob'});
  });

  describe('Boolean', () => {

    it('can store a Boolean', () => {
      localStorageService.putBoolean('test', true);
      // boolean key is prepended by username
      expect(localStorage.getItem('Bob-test')).toBeDefined();
    });

    it('can return a Boolean', () => {
      localStorageService.putBoolean('test2', true);
      expect(localStorageService.getBoolean('test2')).toBe(true);
    });
  });
});
