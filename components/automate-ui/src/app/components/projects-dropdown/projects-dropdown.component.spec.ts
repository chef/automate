import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

import { ProjectsDropdownComponent } from './projects-dropdown.component';

describe('ProjectsDropdownComponent', () => {
  let component: ProjectsDropdownComponent;
  let fixture: ComponentFixture<ProjectsDropdownComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ProjectsDropdownComponent],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ProjectsDropdownComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('#projectsArray', () => {
    it('#projectsArray returns an object naturally ordered by name', () => {
      const unsortedProjects: {} = [
        {
          checked: false,
          id: '123abc',
          name: '123abc',
          status: 'NO_RULES',
          type: 'CUSTOM'
        },
        {
          checked: false,
          id: 'project-6',
          name: 'project-6',
          status: 'NO_RULES',
          type: 'CUSTOM'
        },
        {
          checked: false,
          id: '12345zero',
          name: '12345zero',
          status: 'NO_RULES',
          type: 'CUSTOM'
        },
        {
          checked: false,
          id: '123xyz',
          name: '123xyz',
          status: 'NO_RULES',
          type: 'CUSTOM'
        },
        {
          checked: false,
          id: 'abc123',
          name: 'abc123',
          status: 'NO_RULES',
          type: 'CUSTOM'
        }
      ];

      // assign unsorted projects for the component to use
      component.projects = unsortedProjects;

      // expect sorted outcomes here
      expect(component.projectsArray().length).toBe(5);
      expect(component.projectsArray()[0]).toEqual(
        jasmine.objectContaining({ name: '123abc' })
      );
      expect(component.projectsArray()[1]).toEqual(
        jasmine.objectContaining({ name: '123xyz' })
      );
      expect(component.projectsArray()[2]).toEqual(
        jasmine.objectContaining({ name: '12345zero' })
      );
      expect(component.projectsArray()[3]).toEqual(
        jasmine.objectContaining({ name: 'abc123' })
      );
      expect(component.projectsArray()[4]).toEqual(
        jasmine.objectContaining({ name: 'project-6' })
      );
    });
  });
});
