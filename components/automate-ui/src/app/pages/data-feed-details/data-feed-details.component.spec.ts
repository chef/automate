import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from '../../../app/ngrx.reducers';
import { DataFeedDetailsComponent } from './data-feed-details.component';
import { MockComponent } from 'ng2-mock-component';
import { FeatureFlagsService } from '../../../app/services/feature-flags/feature-flags.service';

describe('DataFeedDetailsComponent', () => {
    let component: DataFeedDetailsComponent;
    let fixture: ComponentFixture<DataFeedDetailsComponent>;

    beforeEach(waitForAsync(() => {
      TestBed.configureTestingModule({
        declarations: [
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
          MockComponent({ selector: 'a', inputs: ['routerLink'] }),
          MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
          DataFeedDetailsComponent
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
      fixture = TestBed.createComponent(DataFeedDetailsComponent);
      component = fixture.componentInstance;
      fixture.detectChanges();
    });

    it('should create', () => {
      expect(component).toBeTruthy();
    });

  });
