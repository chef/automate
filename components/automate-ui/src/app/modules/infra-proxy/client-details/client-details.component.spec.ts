import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { ClientDetailsComponent } from './client-details.component';
import { MockComponent } from 'ng2-mock-component';
import { MockChefButton, MockChefHeading, MockChefIcon, MockChefLoadingSpinner, MockChefOption, MockChefPageHeader, MockChefSnippet, MockChefSubheading, MockChefTabSelector, MockChefTable, MockChefTbody, MockChefTd, MockChefTh, MockChefThead, MockChefToolbar, MockChefTr } from 'app/testing/mock-components';
describe('ClientDetailsComponent', () => {
  let component: ClientDetailsComponent;
  let fixture: ComponentFixture<ClientDetailsComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        ClientDetailsComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      imports: [
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        MockChefButton,
        MockComponent({ selector: 'mat-select' }),
        MockChefHeading,
        MockChefIcon,
        MockChefLoadingSpinner,
        MockChefOption,
        MockChefPageHeader,
        MockChefSubheading,
        MockChefSnippet,
        MockChefToolbar,
        MockChefTable,
        MockChefThead,
        MockChefTbody,
        MockChefTr,
        MockChefTh,
        MockChefTd,
        MockChefSnippet,
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockChefTabSelector,
        MockComponent({ selector: 'app-reset-client-key', inputs: ['openEvent'] }),
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
    fixture = TestBed.createComponent(ClientDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('defaults to showing details section', () => {
    expect(component.tabValue).toBe('details');
  });
});
