import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';

import { CookbookDependenciesComponent } from './cookbook-dependencies.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { PolicyFileRequests } from 'app/entities/policy-files/policy-file.requests';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { CookbookRuleList, CookbookDependencyList, CookbookList } from '../policy-file-details/policy-file-details.component';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('CookbookDependenciesComponent', () => {
  let component: CookbookDependenciesComponent;
  let fixture: ComponentFixture<CookbookDependenciesComponent>;

  beforeEach(waitForAsync(() => {
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
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        CookbookDependenciesComponent
      ],
      providers: [
        FeatureFlagsService,
        PolicyFileRequests,
        HttpClient,
        HttpHandler,
        { provide: TelemetryService, useClass: MockTelemetryService }
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
    fixture = TestBed.createComponent(CookbookDependenciesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('dependency rule list', () => {
    const availableCookbookRule: CookbookRuleList[] = [{
      name: 'test',
      version: '1.3.8',
      operator: '='
    }];
    const emptyCookbookRule: CookbookRuleList[] = [];

    const availableCookbookDependency: CookbookDependencyList[] = [{
      name: 'test',
      version: '1.3.8',
      operator: '=',
      dependName: 'cron',
      dependOperator: '>=',
      dependVersion: '3.2.1'
      }
    ];
    const emptyCookbookDependency: CookbookRuleList[] = [];

    const availableCookbooks: CookbookList[] = [{
      name: 'test',
      version: '1.3.8',
      source: '../test'
      }
    ];
    const emptyCookbook: CookbookRuleList[] = [];

    it('render the dependency list', () => {
      component.slidePanel(availableCookbookRule, availableCookbookDependency, availableCookbooks);
      expect(component.cookbookRules.length).not.toBeNull();
      expect(component.cookbookDependencies.length).not.toBeNull();
      expect(component.cookbooks.length).not.toBeNull();
      expect(component.isSlideOpen).toBe(true);
    });

    it('show no preview image', () => {
      component.slidePanel(emptyCookbookRule, emptyCookbookDependency, emptyCookbook);
      expect(component.cookbookRules.length).toBe(0);
      expect(component.cookbookDependencies.length).toBe(0);
      expect(component.cookbooks.length).toBe(0);
      expect(component.isSlideOpen).toBe(true);
    });
  });
});
