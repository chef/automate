import { CUSTOM_ELEMENTS_SCHEMA, EventEmitter } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ProjectsDropdownComponent } from './projects-dropdown.component';

describe('ProjectsDropdownComponent', () => {
  let component: ProjectsDropdownComponent;
  let fixture: ComponentFixture<ProjectsDropdownComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ProjectsDropdownComponent ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ProjectsDropdownComponent);
    component = fixture.componentInstance;
    component.projectsUpdated = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
