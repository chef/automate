import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { Store, StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { By } from '@angular/platform-browser';
import { GetDataBagsSuccess } from 'app/entities/data-bags/data-bags.actions';
import { DataBag } from 'app/entities/data-bags/data-bags.model';
import { DataBagsListComponent } from './data-bags-list.component';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('DataBagsListComponent', () => {
  let component: DataBagsListComponent;
  let fixture: ComponentFixture<DataBagsListComponent>;
  let element;
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({ selector: 'app-create-data-bag-modal',
        inputs: ['openEvent', 'server_Id', 'org_Id'] }),
        MockComponent({ selector: 'app-delete-infra-object-modal',
        inputs: ['visible', 'objectNoun', 'objectAction', 'custom', 'objectName'],
        outputs: ['close', 'deleteClicked'] }),
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
        DataBagsListComponent
      ],
      providers: [
        FeatureFlagsService,
        { provide: TelemetryService, useClass: MockTelemetryService }
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DataBagsListComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('databag list', () => {
    let store: Store<NgrxStateAtom>;
    const availableDataBags: DataBag[] = [
      {
        server_id: 'test_server_id',
        org_id: 'test_org_id',
        name: 'aix'
      }
    ];

    const emptyDataBags: DataBag[] = [];

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('render the databag list', () => {
      store.dispatch(new GetDataBagsSuccess({data_bags: availableDataBags}));
      expect(component.dataBags.length).not.toBeNull();
      expect(element.query(By.css('.empty-section'))).toBeNull();
    });

    it('show no preview image', () => {
      store.dispatch(new GetDataBagsSuccess({data_bags: emptyDataBags}));
      expect(component.dataBags.length).toBe(0);
    });
  });
});
