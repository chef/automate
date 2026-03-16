import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { MockChefButton, MockChefError, MockChefFormField, MockChefModal } from 'app/testing/mock-components';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { EditDataBagItemModalComponent } from './edit-data-bag-item-modal.component';
import { DataBagsItemDetails } from 'app/entities/data-bags/data-bags.model';
import { EventEmitter } from '@angular/core';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('EditDataBagItemModalComponent', () => {
  let component: EditDataBagItemModalComponent;
  let fixture: ComponentFixture<EditDataBagItemModalComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [
        EditDataBagItemModalComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService }
      ],
      imports: [
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks }),
        MockChefModal,
        MockChefButton,
        MockChefError,
        MockChefFormField
      ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(EditDataBagItemModalComponent);
    component = fixture.componentInstance;
    component.openEvent = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('#UpdateDataBagItemForm', () => {
    const dataBagItem: DataBagsItemDetails = {
      data: '{}',
      data_bag_name: 'data-bag-819756700',
      name: 'item-655403600',
      org_id: 'test_org',
      server_id: 'chef-manage-sever'
    };

    it('should be valid when json data is filled out', () => {
      component.updateForm.controls['data'].setValue(dataBagItem.data);
      expect(component.updateForm.valid).toBeTruthy();
    });
  });

});
