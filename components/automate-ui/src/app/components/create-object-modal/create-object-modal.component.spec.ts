import { EventEmitter, SimpleChange } from '@angular/core';
import { ReactiveFormsModule, FormBuilder } from '@angular/forms';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';

import { using } from 'app/testing/spec-helpers';
import { Project } from 'app/entities/projects/project.model';
import { CreateObjectModalComponent } from './create-object-modal.component';

describe('CreateObjectModalComponent', () => {
  let fixture: ComponentFixture<CreateObjectModalComponent>;
  let component: CreateObjectModalComponent;

  beforeEach(async () => {
    TestBed.configureTestingModule({
      declarations: [
        CreateObjectModalComponent,
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-checkbox', inputs: ['checked'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
        MockComponent({ selector: 'app-projects-dropdown',
          inputs: ['projects', 'disabled', 'projectsUpdated' ],
          outputs: ['onProjectChecked'] })
      ],
     imports: [
        ReactiveFormsModule
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateObjectModalComponent);
    component = fixture.componentInstance;
    component.conflictErrorEvent = new EventEmitter();
    component.createForm = new FormBuilder().group({
      name: ['', null],
      id: ['', null]
    });
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  using([
    ['my-name', 'my-name', 'mirrors input with no leading/trailing whitespace'],
    ['  my-name  ', 'my-name', 'trims leading whitespace'],
    ['my-name  ', 'my-name', 'trims trailing whitespace']
  ], function (inputVal: string, outputVal: string, desc: string) {

    it('handleNameInput ' + desc, () => {
      component.createForm.controls['name'].setValue(inputVal);
      component.handleNameInput(new KeyboardEvent('keyup',
        // Typescript bug requires us to work around with <any>
        // https://github.com/Microsoft/TypeScript/issues/15228
        <any>{bubbles : true, cancelable : true, key : inputVal, char : inputVal, shiftKey : true
      }));
      expect(component.createForm.controls['id'].value).toBe(outputVal);
    });
  });

  it('ngOnChanges initializes checked status to false for all projects ', () => {
    const assignableProjects: Project[] = [
      {id: 'proj1', name: 'proj1', type: 'CHEF_MANAGED', status: 'NO_RULES'},
      {id: 'proj3', name: 'proj3', type: 'CUSTOM', status: 'EDITS_PENDING'},
      { id: 'proj2', name: 'proj2', type: 'CUSTOM', status: 'RULES_APPLIED' }
    ];
    component.ngOnChanges(
      { assignableProjects: new SimpleChange({}, assignableProjects, true) });
    expect(Object.values(component.projects).length).toEqual(assignableProjects.length);
    assignableProjects.forEach(p => {
      const proj = component.projects[p.id];
      expect(proj).toEqual({ ...p, checked: false });
    });
  });

  it('ngOnChanges resets checked status to false for all projects ', () => {
    component.projects = {
      'proj1':
        { id: 'proj1', name: 'proj1', type: 'CHEF_MANAGED', status: 'NO_RULES', checked: true },
      'proj3':
        { id: 'proj3', name: 'proj3', type: 'CUSTOM', status: 'EDITS_PENDING', checked: false },
      'proj2':
        { id: 'proj2', name: 'proj2', type: 'CUSTOM', status: 'RULES_APPLIED', checked: true }
    };
    component.ngOnChanges(
      { visible: new SimpleChange(false, true, true) });
    Object.values(component.projects).forEach(p => {
      expect(p.checked).toBe(false);
    });
  });

});
