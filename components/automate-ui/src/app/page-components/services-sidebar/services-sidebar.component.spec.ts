import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { ServicesSidebarComponent } from './services-sidebar.component';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';

import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track(_event?: string, _properties?: any): void { }
}

describe('ServicesSidebarComponent', () => {
  let component: ServicesSidebarComponent;
  let fixture: ComponentFixture<ServicesSidebarComponent>;
  let store: Store<NgrxStateAtom>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      declarations: [
        ServicesSidebarComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    });

    fixture = TestBed.createComponent(ServicesSidebarComponent);
    component = fixture.componentInstance;
    store = TestBed.inject(Store);
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
