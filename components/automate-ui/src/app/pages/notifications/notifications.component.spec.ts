import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { MockChefButton, MockChefError, MockChefHeading, MockChefIcon, MockChefLoadingSpinner, MockChefPageHeader, MockChefSubheading, MockChefTable, MockChefTbody, MockChefTd, MockChefTh, MockChefThead, MockChefToolbar, MockChefTr } from 'app/testing/mock-components';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { NotificationsComponent } from './notifications.component';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { TelemetryService } from '../../services/telemetry/telemetry.service';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';

class MockTelemetryService {
  track() { }
}

describe('NotificationsComponent', () => {
  let component: NotificationsComponent;
  let fixture: ComponentFixture<NotificationsComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        NotificationsComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        FeatureFlagsService,
        HttpClient,
        HttpHandler
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({
        selector: 'app-create-notification-modal',
        inputs: ['openEvent']
        }),
        MockComponent({ selector: 'app-delete-object-modal',
        inputs: ['default', 'visible', 'objectNoun', 'objectName'],
        outputs: ['close', 'deleteClicked'] }),
        MockChefButton,
        MockChefError,
        MockChefHeading,
        MockChefIcon,
        MockChefLoadingSpinner,
        MockChefPageHeader,
        MockChefSubheading,
        MockChefToolbar,
        MockChefTable,
        MockChefThead,
        MockChefTbody,
        MockChefTr,
        MockChefTh,
        MockChefTd
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(NotificationsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
