import { ChefClipboard } from './chef-clipboard';

let clipboard;

describe('chef-clipboard', () => {

  beforeAll(() => {
    jest.useFakeTimers();

    clipboard = new ChefClipboard();
    clipboard.copy = jest.fn();
  });

  it('exists', () => {
    expect(clipboard).toBeTruthy();
  });

  describe('when clicked', () => {
    it('copies value to clipboard', () => {
      const value = 'to be copied';
      clipboard.value = value;

      clipboard.handleClick();

      expect(clipboard.copy).toHaveBeenCalledWith(value);
    });

    it('sets tooltip text to `Copied!` for `1.5s`', () => {
      clipboard.handleClick();

      expect(clipboard.tooltipText).toEqual('Copied!');
      jest.advanceTimersByTime(1500);
      expect(clipboard.tooltipText).toEqual('Copy to clipboard');
    });
  });
});
