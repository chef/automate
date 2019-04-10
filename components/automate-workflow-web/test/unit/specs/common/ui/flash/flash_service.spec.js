import ng from 'angular';
import 'angular-mocks';
import _ from 'lodash';
import flashService from '../../../../../../src/common/ui/flash/flash';

describe('flashService', () => {
  let Flash;

  beforeEach(ng.mock.module(flashService));

  beforeEach(inject((_Flash_) => {
    Flash = _Flash_;
  }));

  it('should register a handler function', () => {
    Flash.register(function () {});
  });

  it('should thow and error if a function is not provided to register', () => {
    expect(_.bind(Flash.register, null, 'foo')).toThrow();
  });

  it('should throw an error if notify or error is called without a registered handler', () => {
    expect(_.bind(Flash.notify, null)).toThrow();
    expect(_.bind(Flash.error, null)).toThrow();
  });

  it('should fire handler with a "notify" string when notify is called', () => {
    let outertype = '';
    Flash.register(function (header, message, type) { outertype = type; });
    Flash.notify('HEADER', 'MESSAGE');
    expect(outertype).toBe('notify');
  });

  it('should fire handler with an "error" string when error is called', () => {
    let outertype = '';
    Flash.register(function (header, message, type) { outertype = type; });
    Flash.error('HEADER', 'MESSAGE');
    expect(outertype).toBe('error');
  });
});
