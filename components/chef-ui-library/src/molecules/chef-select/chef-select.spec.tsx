import { ChefSelect } from './chef-select';
import { ChefOption } from '../../atoms/chef-option/chef-option';

describe('chef-select', () => {

  it('exists', () => {
    expect(new ChefSelect()).toBeTruthy();
  });

  describe('handleFocus', () => {

    it('sets focus to true', () => {
      const select = new ChefSelect();

      select.handleFocus();

      expect(select.focused).toBe(true);
    });

  });

  describe('handleFocusOut', () => {
    let select;
    let eventMock;

    beforeEach(() => {
      select = new ChefSelect();
      select.focused = true;
      select.active = true;
      eventMock = {
        stopPropagation: jest.fn(),
        relatedTarget: null
      };
    });

    it('sets focus to false', () => {
      select.handleFocusOut(eventMock);
      expect(select.focused).toBe(false);
    });

    it('sets active to false', () => {
      select.handleFocusOut(eventMock);
      expect(select.active).toBe(false);
    });

    it('does not set focus to false when the relatedTarget is the dropdown', () => {
      eventMock.relatedTarget = { nodeName: 'CHEF-DROPDOWN' };
      select.handleFocusOut(eventMock);
      expect(select.focused).toBe(true);
    });

    it('does not set active to false when the relatedTarget is the dropdown', () => {
      eventMock.relatedTarget = { nodeName: 'CHEF-DROPDOWN' };
      select.handleFocusOut(eventMock);
      expect(select.active).toBe(true);
    });

  });

  describe('handleClickActivation', () => {
    let select;
    let option;
    let clickEventMock;

    beforeEach(() => {
      select = new ChefSelect();
      select.el = {
        querySelector: jest.fn().mockReturnValue({ focus: jest.fn() })
      };
      select.change = {
        emit: jest.fn()
      };
      option = new ChefOption();
      clickEventMock = {
        target: {
          closest: () => option
        }
      };
    });

    it('sets active to true', () => {
      select.handleClickActivation(clickEventMock);
      expect(select.active).toBe(true);
    });

    it('sets active to false when it is true', () => {
      select.active = true;
      select.handleClickActivation(clickEventMock);
      expect(select.active).toBe(false);
    });

    it('focuses on the dropdown during activation', () => {
      const focus = jest.fn();
      const querySelector = jest.fn().mockReturnValue({ focus });
      select.el = { querySelector };
      select.active = false;
      select.handleKeyActivation();

      expect(querySelector).toBeCalledWith('chef-dropdown');
      expect(focus).toBeCalled();
    });

    it('emits a change event', () => {
      select.active = true;
      select.handleKeyActivation();

      expect(select.change.emit.mock.calls.length).toBe(1);
    });

  });

  describe('handleKeyActivation', () => {
    let select;

    beforeEach(() => {
      select = new ChefSelect();
      select.el = {
        querySelector: jest.fn().mockReturnValue({ focus: jest.fn() })
      };
      select.change = {
        emit: jest.fn()
      };
    });

    it('activates', async () => {
      select.handleKeyActivation();
      expect(select.active).toBe(true);
    });

    it('highlights the first option by default', () => {
      select.handleKeyActivation();
      expect(select.focusedIndex).toBe(0);
    });

    it('deactivates if already activated', () => {
      select.active = true;
      select.handleKeyActivation();

      expect(select.active).toBe(false);
    });

    it('selects an option if active', () => {
      // Toggle key activation twice to set the state properly.
      select.handleKeyActivation();
      select.handleKeyActivation();

      expect(select.selectedIndex).toBe(0);
    });

    it('focuses on the dropdown during activation', () => {
      const focus = jest.fn();
      const querySelector = jest.fn().mockReturnValue({ focus });
      select.el = { querySelector };
      select.active = false;
      select.handleKeyActivation();

      expect(querySelector).toBeCalledWith('chef-dropdown');
      expect(focus).toBeCalled();
    });

    it('emits a change event', () => {
      select.active = true;
      select.handleKeyActivation();

      expect(select.change.emit.mock.calls.length).toBe(1);
    });

  });

  describe('handleEscape', () => {

    it('sets active to false', () => {
      const select = new ChefSelect();
      select.active = true;

      select.handleEscape();

      expect(select.active).toBe(false);
    });

  });

  describe('handleUp and handleDown', () => {
    let select;
    let mockEvent;

    beforeEach(() => {
      select = new ChefSelect();
      select.options = new Array(10);
      mockEvent = {
        preventDefault: jest.fn()
      };
    });

    describe('handleUp', () => {

      it('decreases the focused index by 1', () => {
        select.focusedIndex = 2;
        select.handleUp(mockEvent);

        expect(select.focusedIndex).toBe(1);
      });

      it('will not decrease focusedIndex below 0', () => {
        select.focusedIndex = 0;
        select.handleUp(mockEvent);

        expect(select.focusedIndex).toBe(0);
      });

    });

    describe('handleDown', () => {

      it('increases the focused index by 1', () => {
        select.focusedIndex = 0;
        select.handleDown(mockEvent);

        expect(select.focusedIndex).toBe(1);
      });

      it('will not increase the focused index greater than the length of the options list', () => {
        select.focusedIndex = 4;
        select.options = new Array(5);
        select.handleDown(mockEvent);

        expect(select.focusedIndex).toBe(4);
      });

    });

  });

});
