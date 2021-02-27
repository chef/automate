import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';
import { MockComponent } from 'ng2-mock-component';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { CreateDataBagModalComponent } from './create-data-bag-modal.component';
import { EventEmitter } from '@angular/core';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { HttpStatus } from 'app/types/types';
import { MatSelectModule } from '@angular/material/select';
import {
  CreateDataBagSuccess,
  CreateDataBagFailure
} from 'app/entities/data-bags/data-bags.actions';
import {
  DataBag
} from 'app/entities/data-bags/data-bags.model';


describe('CreateDataBagModalComponent', () => {
  let component: CreateDataBagModalComponent;
  let fixture: ComponentFixture<CreateDataBagModalComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-toolbar' }),
        CreateDataBagModalComponent
      ],
      imports: [
        MatSelectModule,
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
    fixture = TestBed.createComponent(CreateDataBagModalComponent);
    component = fixture.componentInstance;
    component.openEvent = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('#createDataBagForm', () => {
    let store: Store<NgrxStateAtom>;
    const dataBag: DataBag = {
      server_id: 'test_server',
      org_id: 'test_server',
      name: 'test_data_bag'
    }

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('should be invalid when no fields are filled out', () => {
      expect(component.createForm.valid).toBeFalsy();
    });

     it('should be valid when all fields are filled out', () => {
      component.createForm.controls['name'].setValue(dataBag.name);
      expect(component.createForm.valid).toBeTruthy();
    });

    it('opening create modal resets name to empty string',
      () => {
      component.visible = true;
      expect(component.createForm.controls['name'].value).toEqual('');
    });

    it('hide modal after create a data bag.', () => {
      component.createForm.controls['name'].setValue(dataBag.name);
      component.createDataBag();

      store.dispatch(new CreateDataBagSuccess({databag: dataBag}));
      expect(component.visible).toBe(false);
    });

    it('on create error, modal is closed with failure banner', () => {
      component.createForm.controls['name'].setValue(dataBag.name);
      component.createDataBag();

      const error = <HttpErrorResponse>{
        status: HttpStatus.INTERNAL_SERVER_ERROR,
        ok: false
      };
      store.dispatch(new CreateDataBagFailure(error));
      expect(component.conflictError).toBe(false);
    });
  });
});
