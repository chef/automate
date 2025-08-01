import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';
import { MockChefHeading, MockChefLoadingSpinner, MockChefOption, MockChefPageHeader, MockChefSubheading, MockChefTable, MockChefTbody, MockChefTd, MockChefTh, MockChefThead, MockChefTr } from 'app/testing/mock-components';
import { CUSTOM_ELEMENTS_SCHEMA, NO_ERRORS_SCHEMA } from '@angular/core';

import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { RolesListComponent } from './roles-list.component';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('RolesListComponent', () => {
  let component: RolesListComponent;
  let fixture: ComponentFixture<RolesListComponent>;

  beforeEach(waitForAsync(() => {

    TestBed.configureTestingModule({
      declarations: [
        RolesListComponent
      ],
      imports: [
        ChefPipesModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks }),
        MockComponent({ selector: 'app-authorized',
                        inputs: ['allOf'],
                        template: '<ng-content></ng-content>' }),
        MockComponent({ selector: 'app-delete-object-modal',
                        inputs: ['visible', 'objectNoun', 'objectName'],
                        outputs: ['close', 'deleteClicked']}),
        MockChefLoadingSpinner,
        MockChefPageHeader,
        MockChefHeading,
        MockChefSubheading,
        MockChefTable,
        MockChefTbody,
        MockChefTd,
        MockChefTh,
        MockChefThead,
        MockChefTr,
        MockChefOption,
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockComponent({ selector: 'a', inputs: ['routerLink']})
      ],
      providers: [
        FeatureFlagsService,
        { provide: TelemetryService, useClass: MockTelemetryService }
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA, NO_ERRORS_SCHEMA]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(RolesListComponent);
    component = fixture.componentInstance;

    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
