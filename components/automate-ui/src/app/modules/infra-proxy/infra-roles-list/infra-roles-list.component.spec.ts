import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { InfraRolesListComponent } from './infra-roles-list.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';

describe('InfraRolesListComponent', () => {
    let component: InfraRolesListComponent;
    let fixture: ComponentFixture<InfraRolesListComponent>;
    beforeEach(async(() => {
      TestBed.configureTestingModule({
        declarations: [
          MockComponent({ selector: 'chef-th' }),
          MockComponent({ selector: 'chef-td' }),
          MockComponent({ selector: 'chef-error' }),
          MockComponent({ selector: 'chef-form-field' }),
          MockComponent({ selector: 'chef-heading' }),
          MockComponent({ selector: 'chef-icon' }),
          MockComponent({ selector: 'chef-loading-spinner' }),
          MockComponent({ selector: 'mat-select' }),
          MockComponent({ selector: 'mat-option' }),
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
          InfraRolesListComponent
        ],
        providers: [
          FeatureFlagsService
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
      fixture = TestBed.createComponent(InfraRolesListComponent);
      component = fixture.componentInstance;
      fixture.detectChanges();
    });
    it('should create', () => {
      expect(component).toBeTruthy();
    });
  });
