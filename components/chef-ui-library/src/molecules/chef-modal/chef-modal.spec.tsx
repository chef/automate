import { ChefModal } from './chef-modal';

describe('chef-modal', () => {
  let modal;

  beforeEach(() => {
    modal = new ChefModal();
  });

  it('exists', () => {
    expect(modal).toBeTruthy();
  });

  it('is not visible by default', () => {
    expect(modal.visible).toEqual(false);
  });

  it('is unlocked by default', () => {
    expect(modal.locked).toEqual(false);
  });

  describe ('renderButton', () => {
    it('returns the exit button if the modal is unlocked', () => {
      expect(modal.renderButton()).not.toEqual('');
    });

    it('returns nil if the modal is locked', () => {
      modal.locked = true;
      expect(modal.renderButton()).toEqual('');
    });
  });

  describe('handleClose', () => {

    it('emits a close event', () => {
      modal.visible = true;
      modal.close = {
        emit: jest.fn()
      };

      modal.handleClose();

      expect(modal.close.emit.mock.calls.length).toBe(1);
    });

  });

});
