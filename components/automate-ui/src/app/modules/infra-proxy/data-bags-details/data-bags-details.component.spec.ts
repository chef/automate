import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
// import { MatOptionSelectionChange } from '@angular/material/core/option';
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
import { By } from '@angular/platform-browser';
import { GetDataBagItemsSuccess, DeleteDataBagItemSuccess } from 'app/entities/data-bags/data-bag-details.actions';
import { DataBagItems } from 'app/entities/data-bags/data-bags.model';
import { DataBagsDetailsComponent } from './data-bags-details.component';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

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
  MockComponent({
    selector: 'app-delete-infra-object-modal',
    inputs: [ 'visible', 'objectNoun', 'objectName'],
    outputs: ['close', 'deleteClicked']
  }),
  MockComponent({ selector: 'chef-button'}),
  MockComponent({ selector: 'app-create-databag-item-modal', inputs: ['openEvent']}),
  DataBagsDetailsComponent
];
const serverId = '6e98f609-586d-4816-a6de-e841e659b11d';
const orgId = '6e98f609-586d-4816-a6de';
const dataBagName = 'auth';

const items: DataBagItems[] = [{
  name: 'auth'
}];

const total = 10;

describe('DataBagsDetailsComponent', () => {
  let component: DataBagsDetailsComponent;
  let fixture: ComponentFixture<DataBagsDetailsComponent>;
  let router: Router;
  let store: Store<NgrxStateAtom>;
  let element;

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
        { provide: TelemetryService, useClass: MockTelemetryService },
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
    element = fixture.debugElement;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('Check data bag items success', () => {
    store.dispatch(new GetDataBagItemsSuccess({ items, total }));
    fixture.detectChanges();
    expect(component.dataBagItems).toEqual(items);
  });

  describe('delete item', () => {
    const dataBagItem: DataBagItems = {
      name: 'test_data_bag_item',
      databag_name: 'test_data_bag',
      active: true
    };


    beforeEach(() => {
      store = TestBed.inject(Store);
      store.dispatch(new GetDataBagItemsSuccess({
        items, total
      }));
      fixture.detectChanges();
    });

    it('opens the delete modal', () => {
      component.startDataBagItemDelete(dataBagItem);
      fixture.detectChanges();

      expect(component.deleteModalVisible).toBe(true);
    });

    it('confirming delete closes modal and removes the item', () => {
      component.startDataBagItemDelete(dataBagItem);
      component.deleteDataBagItem();

      store.dispatch(new DeleteDataBagItemSuccess({name: dataBagItem.name}));
      fixture.detectChanges();

      expect(component.deleteModalVisible).toBe(false);
    });
  });

  describe('data bag item list', () => {

    const emptyDataBags: DataBagItems[] = [];

    it('render the databag item list', () => {
      store.dispatch(new GetDataBagItemsSuccess({items, total}));
      expect(component.dataBagItems.length).not.toBeNull();
      expect(element.query(By.css('.empty-section'))).toBeNull();
    });

    it('show no preview image', () => {
      store.dispatch(new GetDataBagItemsSuccess({items: emptyDataBags, total}));
      expect(component.dataBagItems.length).toBe(0);
    });
  });
});
