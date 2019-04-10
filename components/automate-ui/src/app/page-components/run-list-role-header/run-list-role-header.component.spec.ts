import { TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { By } from '@angular/platform-browser';
import { RunListRoleHeaderComponent } from './run-list-role-header.component';
import { RunListTableComponent } from '../run-list-table/run-list-table.component';
import { ChefStatusIconPipe } from '../../pipes/chef-status-icon.pipe';

describe('RunListRoleHeaderComponent', () => {
  let fixture, element;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        RunListRoleHeaderComponent,
        RunListTableComponent,
        ChefStatusIconPipe
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(RunListRoleHeaderComponent);
    element = fixture.debugElement;
  });

  it('renders the component correctly', () => {
    expect(element.query(By.css('.run-list-role-header'))).not.toBeNull();
  });
});
