import { ReactiveFormsModule, FormBuilder } from '@angular/forms';
import { TestBed, ComponentFixture, waitForAsync } from '@angular/core/testing';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { ngrxReducers, runtimeChecks, NgrxStateAtom } from 'app/ngrx.reducers';
import { CreateNodeCredentialModalComponent } from './create-node-credential-modal.component';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { SaveNodeCredential, NodeCredential, NodeCredentialTypes } from 'app/entities/node-credentials/node-credential.model';
import { HttpErrorResponse } from '@angular/common/http';
import { HttpStatus } from 'app/types/types';
import { CreateNodeCredentialFailure, CreateNodeCredentialSuccess } from 'app/entities/node-credentials/node-credential.actions';


describe('CreateNodeCredentialModalComponent', () => {
  let store: Store<NgrxStateAtom>;
  let fixture: ComponentFixture<CreateNodeCredentialModalComponent>;
  let component: CreateNodeCredentialModalComponent;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        CreateNodeCredentialModalComponent,
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-checkbox', inputs: ['checked'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] })
      ],
      providers: [
        FeatureFlagsService,
        SaveNodeCredential
      ],
      imports: [
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ]
    }).compileComponents();
    store = TestBed.inject(Store);
    spyOn(store, 'dispatch').and.callThrough();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateNodeCredentialModalComponent);
    component = fixture.componentInstance;
    component.createNodeCredForm = new FormBuilder().group({
      id: [''],
      name: [''],
      type: [''],
      ssh: this.sshForms,
      winrm: this.winrmForms,
      sudo: this.sudoForms
    });
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  const nodeCredential: NodeCredential = {
      id: '1',
      name: 'new nodeCredential',
      type: NodeCredentialTypes.SSH,
      last_modified: '2020-08-24T05:23:24Z',
      tags: [],
      data: [{
        key: 'username',
        value: 'test'
      },
      {
        key: 'password',
        value: 'test'
      },
      {
        key: 'key',
        value: 'test'
      }]
    };

  it('closes create modal resets id, name, type, last_modified,tags and data to empty string',
    () => {
      expect(component.creatingUser).toBe(false);
      expect(component.createNodeCredForm.controls['id'].value).toEqual('');
      expect(component.createNodeCredForm.controls['name'].value).toEqual('');
      expect(component.createNodeCredForm.controls['type'].value).toEqual('');
      expect(component.createNodeCredForm.controls['ssh'].value).toEqual(null);
      expect(component.createNodeCredForm.controls['winrm'].value).toEqual(null);
      expect(component.createNodeCredForm.controls['sudo'].value).toEqual(null);
    });

  // looking for a solution for ths test case.
  xit('on conflict error, modal remains open and displays conflict error', () => {
    component.createNodeCredForm.controls['id'].setValue(nodeCredential.id);
    component.createNodeCredForm.controls['name'].setValue(nodeCredential.name);
    component.createNodeCredForm.controls['type'].setValue(nodeCredential.type);
    component.createNodeCredForm.controls.ssh.setValue(nodeCredential.data[0]);

    component.createUser();

    const conflict = <HttpErrorResponse>{
      status: HttpStatus.CONFLICT,
      ok: false
    };
    store.dispatch(new CreateNodeCredentialFailure(conflict));

    expect(component.creatingUser).toBe(true);
  });

  it('on success, closes modal and adds new nodeCredential', () => {
    component.createNodeCredForm.controls['id'].setValue(nodeCredential.id);
    component.createNodeCredForm.controls['name'].setValue(nodeCredential.name);
    component.createNodeCredForm.controls['type'].setValue(nodeCredential.type);
    component.createNodeCredForm.controls.ssh.setValue(nodeCredential.data[0]);

    component.createUser();

    store.dispatch(new CreateNodeCredentialSuccess(nodeCredential));
    expect(component.creatingUser).toBe(true);
    expect(component.visible).toBe(false);

  });

  it('on create error, modal is closed (because error is handled by failure banner)', () => {
    component.createNodeCredForm.controls['id'].setValue(nodeCredential.id);
    component.createNodeCredForm.controls['name'].setValue(nodeCredential.name);
    component.createNodeCredForm.controls['type'].setValue(nodeCredential.type);
    component.createNodeCredForm.controls.ssh.setValue(nodeCredential.data[0]);
    component.createUser();

    const error = <HttpErrorResponse>{
      status: HttpStatus.BAD_REQUEST,
      ok: false
    };

    store.dispatch(new CreateNodeCredentialFailure(error));

    expect(component.visible).toBe(false);
  });
});
