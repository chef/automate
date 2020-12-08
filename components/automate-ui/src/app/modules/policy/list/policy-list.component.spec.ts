import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, defaultInitialState, runtimeChecks } from 'app/ngrx.reducers';
import { MockComponent } from 'ng2-mock-component';

import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { customMatchers } from 'app/testing/custom-matchers';
import { PolicyListComponent } from './policy-list.component';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';

describe('PolicyListComponent', () => {
  let store: Store<NgrxStateAtom>;
  let component: PolicyListComponent;
  let fixture: ComponentFixture<PolicyListComponent>;
  let element: HTMLElement;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'app-authorized', inputs: ['allOf'] }),
        MockComponent({
          selector: 'app-delete-object-modal',
          inputs: ['visible', 'objectNoun', 'objectName', 'moreDetails'],
          outputs: ['close', 'deleteClicked']
        }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        PolicyListComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      imports: [
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
