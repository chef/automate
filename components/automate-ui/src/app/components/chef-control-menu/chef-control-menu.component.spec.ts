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

});
