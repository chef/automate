import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, defaultInitialState, runtimeChecks } from 'app/ngrx.reducers';
import { MockComponent } from 'ng2-mock-component';
import { MockChefHeading, MockChefLoadingSpinner, MockChefPageHeader, MockChefSubheading, MockChefTable, MockChefTbody, MockChefTd, MockChefTh, MockChefThead, MockChefTr } from 'app/testing/mock-components';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { customMatchers } from 'app/testing/custom-matchers';
import { PolicyListComponent } from './policy-list.component';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('PolicyListComponent', () => {
  let store: Store<NgrxStateAtom>;
  let component: PolicyListComponent;
  let fixture: ComponentFixture<PolicyListComponent>;
  let element: HTMLElement;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        PolicyListComponent
      ],
      providers: [
        FeatureFlagsService,
        { provide: TelemetryService, useClass: MockTelemetryService }
      ],
      imports: [
        MockComponent({ selector: 'app-authorized', inputs: ['allOf'] }),
        MockComponent({
          selector: 'app-delete-object-modal',
          inputs: ['visible', 'objectNoun', 'objectName', 'moreDetails'],
          outputs: ['close', 'deleteClicked']
        }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockChefHeading,
        MockChefPageHeader,
        MockChefSubheading,
        MockChefLoadingSpinner,
        MockChefTable,
        MockChefTbody,
        MockChefTd,
        MockChefTh,
        MockChefThead,
        MockChefTr,
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        ChefPipesModule,
        StoreModule.forRoot(ngrxReducers, { initialState: defaultInitialState, runtimeChecks })
      ]
    }).compileComponents();
      store = TestBed.inject(Store);
      spyOn(store, 'dispatch').and.callThrough();
  }));

  beforeEach(() => {
    jasmine.addMatchers(customMatchers);
    fixture = TestBed.createComponent(PolicyListComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement.nativeElement;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('contains key elements', () => {
    expect(element).toContainPath('chef-page-header');
  });

  it('displays policy data', () => {
    fixture.detectChanges();
    expect(element).toContainPath('app-authorized');
  });
});
