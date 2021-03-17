import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';
import { MockComponent } from 'ng2-mock-component';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { EventEmitter } from '@angular/core';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { HttpStatus } from 'app/types/types';
import { CreateDatabagItemModalComponent } from './create-databag-item-modal.component';
import {
  CreateDataBagItemSuccess,
  CreateDataBagItemPayload,
  CreateDataBagItemFailure
} from 'app/entities/data-bags/data-bag-details.actions';
import {
  DataBagItem
} from 'app/entities/data-bags/data-bags.model';

describe('CreateDatabagItemModalComponent', () => {
  let component: CreateDatabagItemModalComponent;
  let fixture: ComponentFixture<CreateDatabagItemModalComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-toolbar' }),
        CreateDatabagItemModalComponent
      ],
      imports: [
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      providers: [
        HttpClient,
        HttpHandler
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateDatabagItemModalComponent);
    component = fixture.componentInstance;
    component.openEvent = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('#createDataBagItemForm', () => {
    let store: Store<NgrxStateAtom>;
    const dataBagItem: DataBagItem = {
      server_id: 'test_server',
      org_id: 'test_server',
      name: 'test_data_bag',
      data: 'test_data'
    };

    const responseData: CreateDataBagItemPayload = {
      name: 'test_name',
      id: 'test_id'
    };

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('should be invalid when no fields are filled out', () => {
      expect(component.createForm.valid).toBeFalsy();
    });

    it('should be valid when all fields are filled out', () => {
      component.createForm.controls['itemId'].setValue(dataBagItem.data);
      expect(component.createForm.valid).toBeTruthy();
    });

    it('opening create modal resets ID to empty string',
      () => {
      component.visible = true;
      expect(component.createForm.controls['itemId'].value).toEqual('');
    });

    it('hide modal after create a data bag.', () => {
      component.createForm.controls['itemId'].setValue(dataBagItem.data);
      component.createDataBagItem();

      store.dispatch(new CreateDataBagItemSuccess({name: responseData.name, id: responseData.id}));
      expect(component.visible).toBe(false);
    });

    it('on create error, modal is closed with failure banner', () => {
      component.createForm.controls['itemId'].setValue(dataBagItem.data);
      component.createDataBagItem();

      const error = <HttpErrorResponse>{
        status: HttpStatus.INTERNAL_SERVER_ERROR,
        ok: false
      };
      store.dispatch(new CreateDataBagItemFailure(error));
      expect(component.conflictError).toBe(false);
    });

  });
});
