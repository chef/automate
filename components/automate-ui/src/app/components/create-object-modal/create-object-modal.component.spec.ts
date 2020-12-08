import { EventEmitter, SimpleChange } from '@angular/core';
import { ReactiveFormsModule, FormBuilder } from '@angular/forms';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { ngrxReducers, runtimeChecks, NgrxStateAtom } from 'app/ngrx.reducers';
import { using } from 'app/testing/spec-helpers';
import { GetPolicies, GetPoliciesSuccessPayload, GetPoliciesSuccess } from 'app/entities/policies/policy.actions';
import { Policy, IAMType } from 'app/entities/policies/policy.model';
import { CreateObjectModalComponent } from './create-object-modal.component';

describe('CreateObjectModalComponent', () => {
  let fixture: ComponentFixture<CreateObjectModalComponent>;
  let component: CreateObjectModalComponent;
  let store: Store<NgrxStateAtom>;

  beforeEach(waitForAsync () => {
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

  it('upon opening, checked status of all PROJECTS is set to false', () => {
    component.checkedProjectIDs = ['proj1', 'proj2', 'proj3'];

    component.ngOnChanges(
      { visible: new SimpleChange(false, true, true) });

    // By resetting this list, that triggers ProjectsDropdownComponent to set all projects false
    expect(component.checkedProjectIDs.length).toEqual(0);
  });

  it('upon opening, checked status of all POLICIES is set to false', () => {
    component.policies = [
      {
        title: 'section1',
        itemList: [
          { id: 'proj1', name: 'proj1', checked: true }
        ]
      },
      {
        title: 'section2',
        itemList: [
          { id: 'proj1', name: 'proj1', checked: true },
          { id: 'proj3', name: 'proj3', checked: false },
          { id: 'proj2', name: 'proj2', checked: true }
        ]
      }
    ];

    component.ngOnChanges(
      { visible: new SimpleChange(false, true, true) });

    const checkedStatusOfEveryPolicy =
      component.policies.flatMap(p => p.itemList.map(i => i.checked));
    expect(checkedStatusOfEveryPolicy)
      .withContext('some polices were not reset to false')
      .toEqual(Array(checkedStatusOfEveryPolicy.length).fill(false));
  });

  it('upon opening, dispatches a call to refresh policies', () => {
    spyOn(store, 'dispatch');

    component.objectNoun = 'token';
    component.ngOnChanges(
      { visible: new SimpleChange(false, true, true) });

    expect(store.dispatch).toHaveBeenCalledWith(new GetPolicies());
  });

  it('upon opening, "also create policies" checkbox is checked', () => {
    component.createForm = new FormBuilder().group({
      name: '',
      addPolicies: ''
    });

    // force it to start at false
    component.createForm.controls.addPolicies.setValue(false);

    component.createProjectModal = true;

    component.ngOnChanges(
      { visible: new SimpleChange(false, true, true) });

    expect(component.createForm.controls.addPolicies.value).toEqual(true);
  });

  describe('ordering', () => {

    it('segregates chef-managed and custom policies into separate sections', () => {
      const policies: GetPoliciesSuccessPayload = {
        policies: [
          genPolicy('zz', 'CHEF_MANAGED'),
          genPolicy('cc', 'CUSTOM'),
          genPolicy('aa', 'CHEF_MANAGED'),
          genPolicy('dd', 'CUSTOM'),
          genPolicy('bb', 'CUSTOM')
        ]
      };
      store.dispatch(new GetPoliciesSuccess(policies));
      fixture.detectChanges();

      const chefPolicies = component.policies[0];
      const customPolicies = component.policies[1];
      expect(chefPolicies.itemList.map(p => p.name)).toEqual(['aa', 'zz']);
      expect(customPolicies.itemList.map(p => p.name)).toEqual(['bb', 'cc', 'dd']);
    });

    it('uses standard sort for custom policies', () => {
      const policies: GetPoliciesSuccessPayload = {
        policies: [
          genPolicy('zz', 'CUSTOM'),
          genPolicy('cc', 'CUSTOM'),
          genPolicy('aa', 'CUSTOM'),
          genPolicy('bb', 'CUSTOM')
        ]
      };
      store.dispatch(new GetPoliciesSuccess(policies));
      fixture.detectChanges();

      const customPolicies = component.policies[1];
      expect(customPolicies.itemList.map(p => p.name))
        .toEqual(['aa', 'bb', 'cc', 'zz']);
    });

    it('uses standard sort for chef-managed policies OTHER THAN ingest', () => {
      const policies: GetPoliciesSuccessPayload = {
        policies: [
          genPolicy('zz', 'CHEF_MANAGED'),
          genPolicy('cc', 'CHEF_MANAGED'),
          genPolicy('aa', 'CHEF_MANAGED'),
          genPolicy('bb', 'CHEF_MANAGED')
        ]
      };
      store.dispatch(new GetPoliciesSuccess(policies));
      fixture.detectChanges();

      const chefPolicies = component.policies[0];
      expect(chefPolicies.itemList.map(p => p.name))
        .toEqual(['aa', 'bb', 'cc', 'zz']);
    });

    it('puts ingest at the top of chef-managed', () => {
      const policies: GetPoliciesSuccessPayload = {
        policies: [
          genPolicy('zz', 'CHEF_MANAGED'),
          genPolicy('ingest-access', 'CHEF_MANAGED'),
          genPolicy('aa', 'CHEF_MANAGED'),
          genPolicy('bb', 'CHEF_MANAGED')
        ]
      };
      store.dispatch(new GetPoliciesSuccess(policies));
      fixture.detectChanges();

      const chefPolicies = component.policies[0];
      expect(chefPolicies.itemList.map(p => p.name))
        .toEqual(['ingest-access', 'aa', 'bb', 'zz']);
    });
  });

  function genPolicy(id: string, type: IAMType): Policy {
    return {
      id,
      name: id,
      type: type,
      members: [],
      projects: []
    };
  }

});
