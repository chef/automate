import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from '../../../app/ngrx.reducers';
import { NotificationDetailsComponent } from './notification-details.component';
import { MockComponent } from 'ng2-mock-component';
import { MockChefButton, MockChefError, MockChefFormField, MockChefHeading, MockChefIcon, MockChefLoadingSpinner, MockChefOption, MockChefPageHeader, MockChefSubheading, MockChefToolbar } from 'app/testing/mock-components';
import { FeatureFlagsService } from '../../../app/services/feature-flags/feature-flags.service';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('NotificationDetailsComponent', () => {
  let component: NotificationDetailsComponent;
  let fixture: ComponentFixture<NotificationDetailsComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        NotificationDetailsComponent
      ],
      providers: [
        FeatureFlagsService,
        { provide: TelemetryService, useClass: MockTelemetryService }
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks }),
        MockChefButton,
        MockComponent({ selector: 'mat-select' }),
        MockChefError,
        MockChefFormField,
        MockChefHeading,
        MockChefIcon,
        MockChefLoadingSpinner,
        MockChefOption,
        MockChefPageHeader,
        MockChefSubheading,
        MockChefToolbar,
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(NotificationDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

});
