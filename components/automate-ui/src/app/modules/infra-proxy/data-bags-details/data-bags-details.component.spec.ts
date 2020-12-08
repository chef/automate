import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { MockComponent } from 'ng2-mock-component';
import { Router } from '@angular/router';
import { StoreModule, Store } from '@ngrx/store';

import {
  NgrxStateAtom,
  ngrxReducers,
  defaultInitialState,
  runtimeChecks,
  defaultRouterState,
  defaultRouterRouterState
} from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { GetDataBagDetailsSuccess } from 'app/entities/data-bags/data-bag-details.actions';
import { DataBags } from 'app/entities/data-bags/data-bags.model';
import { DataBagsDetailsComponent } from './data-bags-details.component';


const declarations: any[] = [
  MockComponent({ selector: 'chef-icon' }),
  MockComponent({ selector: 'mat-select' }),
  MockComponent({ selector: 'mat-option' }),
  MockComponent({ selector: 'chef-breadcrumb', inputs: ['link'] }),
  MockComponent({ selector: 'chef-breadcrumbs' }),
  MockComponent({ selector: 'chef-page-header' }),
  MockComponent({ selector: 'chef-heading' }),
  MockComponent({ selector: 'chef-subheading' }),
  MockComponent({ selector: 'chef-loading-spinner' }),
  DataBagsDetailsComponent
];
const serverId = '6e98f609-586d-4816-a6de-e841e659b11d';
const orgId = '6e98f609-586d-4816-a6de';
const dataBagName = 'demo_data_bag';

const data_bags: DataBags[] = [{
  name: 'auth'
}];

describe('DataBagsDetailsComponent', () => {
  let component: DataBagsDetailsComponent;
  let fixture: ComponentFixture<DataBagsDetailsComponent>;
  let router: Router;
  let store: Store<NgrxStateAtom>;

  const initialState = {
    ...defaultInitialState,
    router: {
      ...defaultRouterState,
      state: {
        ...defaultRouterRouterState,
        url: `infrastructure/chef-servers/${serverId}/org/${orgId}/data_bags/${name}`,
        params: { id: serverId, orgid: orgId, name: dataBagName }
      }
    }
  };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: declarations,
      providers: [
        FeatureFlagsService
      ],
      imports: [
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { initialState, runtimeChecks })
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    router = TestBed.inject(Router);
    spyOn(router, 'navigate').and.stub();
    store = TestBed.inject(Store);

    fixture = TestBed.createComponent(DataBagsDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('Check data bag items success', () => {
    store.dispatch(new GetDataBagDetailsSuccess({ data_bags }));
    fixture.detectChanges();
    expect(component.dataBagDetails).toEqual(data_bags);
  });

});

