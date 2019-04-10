import { TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { By } from '@angular/platform-browser';
import { RunListTableComponent } from './run-list-table.component';
import { ChefStatusIconPipe } from '../../pipes/chef-status-icon.pipe';

describe('RunListTableComponent', () => {
  let fixture, component, element;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        RunListTableComponent,
        ChefStatusIconPipe
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(RunListTableComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
  });

  it('renders the component correctly', () => {
    expect(element.query(By.css('.recipe-list'))).not.toBeNull();
  });

  describe('when it is not a policyfile node', () => {
    beforeEach(() => {
      component.item =  {
        status: 'failure',
        name: 'this_item::default'
      };
      component.policy_revision = null;
    });

    it('does not render the PF identifier', () => {
      fixture.detectChanges();
      expect(element.query(By.css('.policy-identifier'))).toBeNull();
    });
  });

  describe('when it is a policyfile node', () => {
    beforeEach(() => {
      component.item =  {
        status: 'failure',
        name: 'this_item::default',
        policy_identifier: 'some_id'
      };
    });

    it('does render the PF identifier', () => {
      fixture.detectChanges();
      expect(element.query(By.css('.policy-identifier'))).not.toBeNull();
    });
  });
});
