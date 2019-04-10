import ng from 'angular';
import 'angular-mocks';
import { bind } from 'lodash';
import ModalService from '../../../../../../src/common/ui/modal/modal_service';

describe('Modal', () => {
  let Modal;

  beforeEach(ng.mock.module(ModalService));

  beforeEach(inject((_Modal_) => {
    Modal = _Modal_;
  }));

  it('should register two handler functions for open and close', () => {
    Modal.register(function () {}, function () {});
  });

  it('should throw an error when functions are not provided', () => {
    expect(bind(Modal.register, null, 'foo', 'bar')).toThrow();
    expect(bind(Modal.register, null, function () {}, 'bar')).toThrow();
    expect(bind(Modal.register, null, 'foo', function () {})).toThrow();
  });

  it('should fire the open handler when open is called', () => {
    let handler;
    Modal.register(
      function () { handler = 'open'; },
      function () { handler = 'closed'; }
    );
    Modal.open();
    expect(handler).toBe("open");
  });

  it('should fire the close handler when close is called', () => {
    let handler;
    Modal.register(
      function () { handler = 'open'; },
      function () { handler = 'closed'; }
    );
    Modal.close();
    expect(handler).toBe('closed');
  });
});
