import { EventEmitter, SimpleChange } from '@angular/core';
import { ReactiveFormsModule, FormBuilder } from '@angular/forms';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { ngrxReducers, runtimeChecks, NgrxStateAtom } from 'app/ngrx.reducers';
import { using } from 'app/testing/spec-helpers';
import { GetPolicies } from 'app/entities/policies/policy.actions';
import { CreateObjectModalComponent } from './create-object-modal.component';

describe('CreateObjectModalComponent', () => {
  let fixture: ComponentFixture<CreateObjectModalComponent>;
  let component: CreateObjectModalComponent;
  let store: Store<NgrxStateAtom>;

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
        MockComponent({ selector: 'app-resource-dropdown',
          inputs: ['resources', 'resourcesUpdated', 'objectNounPlural'] }),
        MockComponent({ selector: 'app-projects-dropdown',
          inputs: ['projects', 'projectsUpdated', 'checkedProjectIDs'] })
      ],
     imports: [
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ]
    }).compileComponents();
    store = TestBed.inject(Store);
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

  it('upon opening, checked status of all projects is set to false', () => {
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

  it('upon opening, checked status of all policies is set to false', () => {
    component.policies = [
        { id: 'proj1', name: 'proj1', type: 'CHEF_MANAGED', members: [], projects: [],
          checked: true },
        { id: 'proj3', name: 'proj3', type: 'CUSTOM', members: [], projects: [], checked: false },
        { id: 'proj2', name: 'proj2', type: 'CUSTOM', members: [], projects: [], checked: true }
    ];

    component.ngOnChanges(
      { visible: new SimpleChange(false, true, true) });

    Object.values(component.policies).forEach(p => {
      expect(p.checked).toBe(false);
    });
  });

  it('upon opening, dispatches a call to refresh policies', () => {
    spyOn(store, 'dispatch');

    component.ngOnChanges(
      { visible: new SimpleChange(false, true, true) });

    expect(store.dispatch).toHaveBeenCalledWith(new GetPolicies());
  });

  it('upon opening, "also create" checkboxes are checked', () => {
    component.createForm = new FormBuilder().group({
      name: '',
      addPolicies: '',
      addTeams: ''
    });

    // force them to start at false
    component.createForm.controls.addPolicies.setValue(false);
    component.createForm.controls.addTeams.setValue(false);

    component.createProjectModal = true;

    component.ngOnChanges(
      { visible: new SimpleChange(false, true, true) });

    expect(component.createForm.controls.addPolicies.value).toEqual(true);
    expect(component.createForm.controls.addTeams.value).toEqual(true);
  });

});
