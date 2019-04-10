import { TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DateSelectorComponent } from './date-selector.component';
import { MockComponent } from 'ng2-mock-component';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

describe('DateSelectorComponent', () => {
  let fixture, component, element;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        DateSelectorComponent,
        MockComponent({selector: 'chef-icon'})
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(DateSelectorComponent );
    component = fixture.componentInstance;
    element = fixture.debugElement;
  });

  it('is closed', () => {
    expect(element.query(By.css('.dropdown-menu'))).toBeNull();
    expect(component.dropdownOpen).toBe(false);
  });

  describe('when clicked', () => {
    it('opens', () => {
      const el = element.query(By.css('.dropdown-description'));
      el.triggerEventHandler('click');
      fixture.detectChanges();
      expect(element.query(By.css('.dropdown-menu'))).not.toBeNull();
      expect(component.dropdownOpen).toBe(true);
    });
  });
});
