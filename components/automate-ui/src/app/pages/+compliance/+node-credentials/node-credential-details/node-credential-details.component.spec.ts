import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { NodeCredentialDetailsScreenComponent } from './node-credential-details.component';
import { MockComponent } from 'ng2-mock-component';
import { GetNodeCredential, UpdateNodeCredential } from 'app/entities/node-credentials/node-credential.actions';
import { NodeCredential, NodeCredentialTypes, SaveNodeCredential, NodeObject } from 'app/entities/node-credentials/node-credential.model';
import { Router } from '@angular/router';

describe('NodeCredentialDetailsScreenComponent', () => {
  let router: Router;
  let component: NodeCredentialDetailsScreenComponent;
  let fixture: ComponentFixture<NodeCredentialDetailsScreenComponent>;
  let store: Store<NgrxStateAtom>;
  const nodeCred: NodeCredential = {
    name: 'test1',
    id: 'uuid-111',
    type: NodeCredentialTypes.SSH,
    last_modified: '',
    data: [{
      key: 'key',
      value: 'key'
    },
    {
      key: 'username',
      value: 'test'
    },
    {
      key: 'password',
      value: 'test'
    }],
    tags: []
  };
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({
          selector: 'app-authorized',
          inputs: ['allOf']
        }),
        MockComponent({
          selector: 'app-create-org-modal',
          inputs: ['visible', 'creating', 'conflictErrorEvent', 'createForm'],
          outputs: ['close', 'createClicked']
        }),
        MockComponent({ selector: 'chef-button',
          inputs: ['disabled', 'routerLink'] }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-option' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'chef-breadcrumb', inputs: ['link'] }),
        MockComponent({ selector: 'chef-breadcrumbs' }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        NodeCredentialDetailsScreenComponent
      ],
      providers: [
        FeatureFlagsService,
        SaveNodeCredential
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    store = TestBed.inject(Store);
    fixture = TestBed.createComponent(NodeCredentialDetailsScreenComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    router = TestBed.inject(Router);
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  const nodeCredential: NodeObject = {
    id: 'uuid-111',
    name: 'test1',
    type: NodeCredentialTypes.SSH,
    ssh: {
      username: '',
      password: '',
      key: ''
    },
    winrm: {
      username: '',
      password: ''
    },
    sudo: {
      password: '',
      options: ''
    }
  };

  describe('navigated to details page', () => {
    beforeEach(() => {
      store.dispatch(new GetNodeCredential({id: nodeCred.id}));
    });

    it('should be created', () => {
      expect(component).toBeTruthy();
    });

    it('defaults to showing details section', () => {
      expect(component.tabValue).toBe('details');
    });

    it('shows/hides sections when based on selection', () => {
      spyOn(router, 'navigate');

      component.onSelectedTab({ target: { value: 'details' } });
      expect(component.tabValue).toBe('details');
      expect(router.navigate).toHaveBeenCalled();

    });

    it('shows/hides sections when based on selection', () => {
      spyOn(router, 'navigate');

      component.onSelectedTab({ target: { value: 'reset' } });
      expect(component.tabValue).toBe('reset');
      expect(router.navigate).toHaveBeenCalled();
    });

    it('resets  form  after success update to empty string', () => {

    component.resetForm.controls['id'].setValue(nodeCredential.id);
    component.resetForm.controls['name'].setValue(nodeCredential.name);
    component.resetForm.controls['type'].setValue(nodeCredential.type);
    component.resetForm.controls.ssh.setValue(nodeCredential.ssh);
    component.resetForm.controls.winrm.setValue(nodeCredential.winrm);
    component.resetForm.controls.sudo.setValue(nodeCredential.sudo);


    const node = component.saveCred.getNodeCredentialCreate(nodeCredential);
    store.dispatch(new UpdateNodeCredential(node));
    expect(component.resetInProgress).toBe(false);
    expect(component.resetForm.controls['ssh'].value).toEqual(
      {username: '', password: '', key: ''});
    });
  });

});
