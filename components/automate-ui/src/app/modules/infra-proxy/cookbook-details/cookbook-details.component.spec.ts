import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { RouterTestingModule } from '@angular/router/testing';
import { MockComponent } from 'ng2-mock-component';
import { Router } from '@angular/router';
import { StoreModule, Store, Action } from '@ngrx/store';
import * as routerStore from '@ngrx/router-store';
import {
  NgrxStateAtom,
  ngrxReducers,
  defaultInitialState,
  runtimeChecks,
  defaultRouterState,
  defaultRouterRouterState
} from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { GetCookbookVersionsSuccess } from 'app/entities/cookbooks/cookbook-versions.actions';
import { CookbookVersions } from 'app/entities/cookbooks/cookbook-versions.model';
import { CookbookDetailsComponent } from './cookbook-details.component';


const declarations: any[] = [
  MockComponent({ selector: 'chef-heading' }),
  MockComponent({ selector: 'chef-icon' }),
  MockComponent({ selector: 'chef-loading-spinner' }),
  MockComponent({ selector: 'mat-select' }),
  MockComponent({ selector: 'mat-option' }),
  MockComponent({ selector: 'chef-page-header' }),
  MockComponent({ selector: 'chef-subheading' }),
  MockComponent({ selector: 'chef-toolbar' }),
  MockComponent({ selector: 'a', inputs: ['routerLink'] }),
  MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
  CookbookDetailsComponent
];
const serverId = '6e98f609-586d-4816-a6de-e841e659b11d';
const orgId = '6e98f609-586d-4816-a6de';
const cookbook_name = 'aix';
const cookbookVersion: CookbookVersions = {
  name: 'cookbook version',
  versions: []
};

describe('CookbookDetailsComponent', () => {
  let component: CookbookDetailsComponent;
  let fixture: ComponentFixture<CookbookDetailsComponent>;
  let router: Router;
  let store: Store<NgrxStateAtom>;


  const initialState = {
    ...defaultInitialState,
    router: {
      ...defaultRouterState,
      state: {
        ...defaultRouterRouterState,
        url: `infrastructure/chef-servers/${serverId}/org/${orgId}/cookbooks/${cookbook_name}`,
        params: { id: serverId, orgid: orgId, cookbook_name: cookbook_name }
      }
    }
  };

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: declarations,
      providers: [
        FeatureFlagsService
      ],
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        StoreModule.forRoot(ngrxReducers, { initialState, runtimeChecks })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    router = TestBed.inject(Router);
    spyOn(router, 'navigate').and.stub();
    store = TestBed.inject(Store);

    fixture = TestBed.createComponent(CookbookDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('defaults to showing users section', () => {
    expect(component.tabValue).toBe('details');
  });

  describe('empty state', () => {
    beforeEach(() => {
      store.dispatch(new GetCookbookVersionsSuccess(cookbookVersion));
      fixture.detectChanges();
    });

    it('cookbook should be empty', () => {
      expect(component.cookbook).toBeUndefined();
    });
  });
});

export class GetRoute implements Action {
  readonly type = routerStore.ROUTER_NAVIGATION;

  constructor(public payload: any) { }
}
