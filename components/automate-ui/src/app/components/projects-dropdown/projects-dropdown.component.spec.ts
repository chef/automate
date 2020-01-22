import { CUSTOM_ELEMENTS_SCHEMA, EventEmitter } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ProjectsDropdownComponent } from './projects-dropdown.component';

describe('ProjectsDropdownComponent', () => {
  let component: ProjectsDropdownComponent;
  let fixture: ComponentFixture<ProjectsDropdownComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ProjectsDropdownComponent ],
      imports: [ FormsModule ],
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

  describe('dropdown', () => {
    beforeEach(() => {
      component.active = true;
      component.filteredProjects = [
        {
          name: 'Project 1',
          id: 'project-1',
          type: 'CUSTOM',
          status: 'NO_RULES',
          checked: false
        },
        {
          name: 'Project 2',
          id: 'project-2',
          type: 'CUSTOM',
          status: 'NO_RULES',
          checked: true
        }
      ];
      fixture.detectChanges();
    });

    it('displays a list of checkbox options', () => {
      const options = Array.from(fixture.nativeElement.querySelectorAll('chef-checkbox'));
      expect(options.length).toEqual(2);
      options.forEach((option: HTMLInputElement, index: number) => {
        const { name, checked } = component.filteredProjects[index];
        expect(option.textContent).toEqual(name);
        expect(option.checked).toEqual(checked);
      });
    });

  });
});
