import ng from 'angular';
import 'angular-mocks';
import storeService from '../../../../../src/common/store/store';

describe('Store', () => {
  let Store, sessionStorage, localStorage;

  beforeEach(ng.mock.module(storeService));

  beforeEach(inject((_Store_, $window) => {
    Store = _Store_;
    localStorage = $window.localStorage;
    sessionStorage = $window.sessionStorage;
  }));

  it('should be able to store a value in the session', () => {
    Store.put('name', 'Batman');
    expect(sessionStorage['name']).toBeDefined();
  });

  it('should be able to store a value in localStorage', () => {
    Store.put('name', 'Batman', true);
    expect(localStorage['name']).toBeDefined();
  });

  it('should be able to store an object', () => {
    let obj = { 'name': 'Batman' };
    Store.put('obj', obj);
    expect(Store.get('obj')).toBeDefined();
  });

  it('should be able to retrieve an item', () => {
    Store.put('name', 'Batman');
    expect(Store.get('name')).toBe('Batman');
  });

  it('should be able to retrieve an object', () => {
    let obj = { 'name': 'Batman' };
    Store.put('obj', obj);
    expect(Store.get('obj')).toEqual(obj);
  });
});
