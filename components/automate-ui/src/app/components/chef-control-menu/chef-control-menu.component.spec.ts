import { CUSTOM_ELEMENTS_SCHEMA, DebugElement } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';

import { ChefControlMenuComponent } from './chef-control-menu.component';

describe('ChefControlMenuComponent', () => {
  let component: ChefControlMenuComponent;
  let fixture: ComponentFixture<ChefControlMenuComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({'selector': 'chef-dropdown'}),
        MockComponent({'selector': 'chef-icon'}),
        MockComponent({'selector': 'chef-option-new'}),
        ChefControlMenuComponent
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ChefControlMenuComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('handleFocusOut', () => {
    let eventMock;

    beforeEach(() => {
      component.isFocused = true;
      component.isActive = true;
      eventMock = new Event('focusout');
    });

    it('sets focus to false', () => {
      component.handleFocusOut(eventMock);
      expect(component.isFocused).toBe(false);
    });

    it('sets active to false', () => {
      component.handleFocusOut(eventMock);
      expect(component.isActive).toBe(false);
    });

    it('does not set focus to false when the relatedTarget is the dropdown', () => {
      eventMock.relatedTarget = { nodeName: 'CHEF-DROPDOWN' };
      component.handleFocusOut(eventMock);
      expect(component.isFocused).toBe(true);
    });

    it('does not set isActive to false when the relatedTarget is the dropdown', () => {
      eventMock.relatedTarget = { nodeName: 'CHEF-DROPDOWN' };
      component.handleFocusOut(eventMock);
      expect(component.isActive).toBe(true);
    });

  });


  describe('handleClickActivation', () => {
    let clickEventMock;

    beforeEach(() => {
      fixture.detectChanges();
      clickEventMock = new Event('click');
    });

    it('sets active to true', () => {
      component.handleClickActivation(clickEventMock);
      expect(component.active).toBe(true);
    });

    it('sets active to false when it is true', () => {
      component.isActive = true;
      component.handleClickActivation(clickEventMock);
      expect(component.isActive).toBe(false);
    });

    it('focuses on the dropdown during activation', () => {
      // const focus = jest.fn();
      // const querySelector = jest.fn().mockReturnValue({ focus });
      // component.el.querySelector = querySelector;
      component.isActive = false;
      component.handleKeyActivation();

      expect(querySelector).toBeCalledWith('chef-dropdown');
      expect(focus).toBeCalled();
    });

    it('emits a change event', () => {
      component.isActive = true;
      component.handleKeyActivation();

      expect(component.change.emit.mock.calls.length).toBe(1);
    });

  });


  describe('handleKeyActivation', () => {
    // let select;

    beforeEach(() => {
      // select = new ChefControlMenu();
      // select.el.querySelector = jest.fn().mockReturnValue({ focus: jest.fn() });
      // select.change = {
      //   emit: jest.fn()
      // };
    });

    it('activates', async () => {
      component.handleKeyActivation();
      expect(component.isActive).toBe(true);
    });

    it('highlights the first option by default', () => {
      component.handleKeyActivation();
      expect(component.focusedIndex).toBe(0);
    });

    it('deactivates if already activated', () => {
      component.isActive = true;
      component.handleKeyActivation();

      expect(component.isActive).toBe(false);
    });

    it('components an option if active', () => {
      // Toggle key activation twice to set the state properly.
      component.handleKeyActivation();
      component.handleKeyActivation();

      expect(component.selectedIndex).toBe(0);
    });

    it('focuses on the dropdown during activation', () => {
      const focus = jest.fn();
      const querySelector = jest.fn().mockReturnValue({ focus });
      select.el.querySelector = querySelector;
      component.isActive = false;
      component.handleKeyActivation();

      expect(querySelector).toBeCalledWith('chef-dropdown');
      expect(focus).toBeCalled();
    });

    it('emits a change event', () => {
      component.isActive = true;
      component.handleKeyActivation();

      expect(component.change.emit.mock.calls.length).toBe(1);
    });

  });

  describe('handleEscape', () => {

    it('sets active to false', () => {
      component.isActive = true;

      component.handleEscape();

      expect(component.isActive).toBe(false);
    });

  });

  describe('handleUp and handleDown', () => {
    let mockEvent;

    beforeEach(() => {
      component.options = new Array(10);
      mockEvent = new Event('keydown');
    });

    describe('handleUp', () => {

      it('decreases the focused index by 1', () => {
        component.focusedIndex = 2;
        component.handleUp(mockEvent);

        expect(component.focusedIndex).toBe(1);
      });

      it('will not decrease focusedIndex below 0', () => {
        component.focusedIndex = 0;
        component.handleUp(mockEvent);

        expect(component.focusedIndex).toBe(0);
      });

    });

    describe('handleDown', () => {

      it('increases the focused index by 1', () => {
        component.focusedIndex = 0;
        component.handleDown(mockEvent);

        expect(component.focusedIndex).toBe(1);
      });

      it('will not increase the focused index greater than the length of the options list', () => {
        component.focusedIndex = 4;
        component.options = new Array(5);
        component.handleDown(mockEvent);

        expect(component.focusedIndex).toBe(4);
      });

    });

});
