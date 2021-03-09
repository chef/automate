import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { CreateClientModalComponent } from './create-client-modal.component';
import { ClientKey } from 'app/entities/clients/client.model';
import { EventEmitter } from '@angular/core';

describe('CreateClientModalComponent', () => {
  let component: CreateClientModalComponent;
  let fixture: ComponentFixture<CreateClientModalComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-checkbox', inputs: ['checked']}),
        CreateClientModalComponent
      ],
      imports: [
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateClientModalComponent);
    component = fixture.componentInstance;
    component.openEvent = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('#createClientForm', () => {
    const clientKey: ClientKey = {
      name: 'test_name',
      public_key: 'test_public_key',
      expiration_date: 'test_expiration_date',
      private_key: 'test_private_key'
    };

    it('should be invalid when no fields are filled out', () => {
      expect(component.createForm.valid).toBeFalsy();
    });

    it('should be valid when all fields are filled out', () => {
      component.createForm.controls['name'].setValue(clientKey.name);
      expect(component.createForm.valid).toBeTruthy();
    });

    it('opening create modal resets name to empty string', () => {
      component.visible = true;
      expect(component.createForm.controls['name'].value).toEqual('');
    });
  });

});
