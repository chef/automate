import ng from 'angular';
import 'angular-mocks';
import sessionService from '../../../../../src/common/auth/session';
import moment from 'moment';

describe('Session', () => {
  let Session, sessionObj, cookies;

  beforeEach(ng.mock.module(sessionService, ($provide) => {
    localStorage.setItem('canonical-enterprise', 'gotham');

    $provide.value('$location', {
      absUrl: () => 'https://www.somedomain.com/e/gotham/',
      host: () => 'somedomain.com',
      protocol: () => 'https'
    });

    $provide.value('Store', {
      get: () => null,
      put: (value) => value,
      setNamespace: () => true
    });

    $provide.value('$cookies', {
      put: () => null,
      remove: () => null
    });
  }));

  beforeEach(inject((_Session_, $cookies) => {
    Session = _Session_;
    cookies = $cookies;
    sessionObj = {
      username: 'Batman',
      token: 'fake_token_here',
      enterprise: 'gotham',
      ttl: 604800
    };
  }));

  it('should have an enterprise', () => {
    let ent = Session.get('enterprise');
    expect(ent).toBe('gotham');
  });

  it('should be able to set a session', () => {
    spyOn(cookies, 'put');
    Session.set(sessionObj);
    expect(cookies.put).toHaveBeenCalledWith('chef-delivery-user', sessionObj.username, jasmine.any(Object));
    expect(cookies.put).toHaveBeenCalledWith('chef-delivery-token', sessionObj.token, jasmine.any(Object));
    expect(cookies.put).toHaveBeenCalledWith('chef-delivery-enterprise', sessionObj.enterprise, jasmine.any(Object));
    expect(Session.get('username')).toBe('Batman');
    expect(Session.get('enterprise')).toBe('gotham');
    expect(Session.get('token')).toBeDefined();
  });

  it('should be able to validate a session exists', () => {
    Session.set(sessionObj);
    expect(Session.hasSession()).toBe(true);
  });

  it('should be able to delete a session', () => {
    spyOn(cookies, 'remove');
    Session.set(sessionObj);
    expect(Session.get()).not.toEqual({});
    Session.remove();
    expect(cookies.remove).toHaveBeenCalledWith('chef-delivery-user', jasmine.any(Object));
    expect(cookies.remove).toHaveBeenCalledWith('chef-delivery-token', jasmine.any(Object));
    // The enterprise is never deleted, but everything else should go away
    expect(Session.get()).toEqual({ enterprise: 'gotham' });
  });
});
