import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { CookbooksComponent } from './cookbooks.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { GetCookbooksSuccess } from 'app/entities/cookbooks/cookbook.actions';
import { By } from '@angular/platform-browser';
import { Cookbook } from 'app/entities/cookbooks/cookbook.model';

describe('CookbooksComponent', () => {
  let component: CookbooksComponent;
  let fixture: ComponentFixture<CookbooksComponent>;
  let element;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),

        CookbooksComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CookbooksComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('cookbook list', () => {
    let store: Store<NgrxStateAtom>;
    const availableCookbooks: Cookbook[] = [
      {name: 'aix', version: '2.3.12'}
    ];
    const emptyCookbooks: Cookbook[] = [];

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('render the cookbook list', () => {
      store.dispatch(new GetCookbooksSuccess({cookbooks: availableCookbooks}));
      expect(component.cookbooks.length).not.toBeNull();
      expect(element.query(By.css('.empty-section'))).toBeNull();
    });

    it('show no preview image', () => {
      store.dispatch(new GetCookbooksSuccess({cookbooks: emptyCookbooks}));
      expect(component.cookbooks.length).toBe(0);
    });
  });
});
