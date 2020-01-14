import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { ChefServerDetailsComponent } from './chef-server-details.component';
import { HttpErrorResponse } from '@angular/common/http';
import { RouterTestingModule } from '@angular/router/testing';
import { CommonModule }  from '@angular/common';
import { BrowserModule } from '@angular/platform-browser';
import { ReactiveFormsModule, FormsModule, FormBuilder } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule, Store } from '@ngrx/store';
import { Org } from 'app/entities/orgs/org.model';
import { CreateOrgSuccess, CreateOrgFailure } from 'app/entities/orgs/org.actions';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { HttpStatus } from 'app/types/types';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';

describe('ChefServerDetailsComponent', () => {
  let component: ChefServerDetailsComponent;
  let fixture: ComponentFixture<ChefServerDetailsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        CommonModule,
        BrowserModule,
        ReactiveFormsModule,
        FormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      declarations: [
        MockComponent({ selector: 'chef-control-menu' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-option' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-table-new' }),
        MockComponent({ selector: 'chef-table-header' }),
        MockComponent({ selector: 'chef-table-body' }),
        MockComponent({ selector: 'chef-table-row' }),
        MockComponent({ selector: 'chef-table-header-cell' }),
        MockComponent({ selector: 'chef-table-cell' }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        ChefServerDetailsComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ChefServerDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.updateServerForm = new FormBuilder().group({
      name: ['', null],
      description: ['', null],
      fqdn: ['', null],
      ip_address: ['', null]
    });

    component.orgForm = new FormBuilder().group({
      name: ['', null],
      admin_user: ['', null],
      admin_key: ['', null]
    });
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('create org', () => {
    let store: Store<NgrxStateAtom>;
    const org = <Org> {
        id: '1',
        name: 'new org',
        admin_user: 'new org user',
        admin_key: 'new admin key'
      };

    beforeEach(() => {
      store = TestBed.get(Store);
    });

    it('openCreateModal opens modal', () => {
      expect(component.createModalVisible).toBe(false);
      component.openCreateModal('create');
      expect(component.createModalVisible).toBe(true);
    });

    it('opening create modal resets name, description, fqdn and ip_address to empty string', () => {
      component.openCreateModal('create');
      expect(component.orgForm.controls['name'].value).toEqual('');
      expect(component.orgForm.controls['admin_user'].value).toEqual('')
      expect(component.orgForm.controls['admin_key'].value).toEqual('')
    });

    it('on success, closes modal and adds new server', () => {
      component.orgForm.controls['name'].setValue(org.name);
      component.orgForm.controls['admin_user'].setValue(org.admin_user);
      component.orgForm.controls['admin_key'].setValue(org.admin_key);
      component.createServerOrg();

      store.dispatch(new CreateOrgSuccess({'org': org}));

      component.orgs.map(orgs => {
        expect(orgs).toContain(org);
      });
    });

    it('on conflict error, modal is open with conflict error', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.openCreateModal('create');
      component.orgForm.controls['name'].setValue(org.name);
      component.orgForm.controls['admin_user'].setValue(org.admin_user);
      component.orgForm.controls['admin_key'].setValue(org.admin_key);
      component.createServerOrg();

      const conflict = <HttpErrorResponse>{
        status: HttpStatus.CONFLICT,
        ok: false
      };
      store.dispatch(new CreateOrgFailure(conflict));

      expect(component.createModalVisible).toBe(true);
    });

    it('on create error, modal is closed with failure banner', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.openCreateModal('create');
      component.orgForm.controls['name'].setValue(org.name);
      component.orgForm.controls['admin_user'].setValue(org.admin_user);
      component.orgForm.controls['admin_key'].setValue(org.admin_key);
      component.createServerOrg();

      const error = <HttpErrorResponse>{
        status: HttpStatus.INTERNAL_SERVER_ERROR,
        ok: false
      };
      store.dispatch(new CreateOrgFailure(error));

      expect(component.createModalVisible).toBe(true);
    });

  });
});
