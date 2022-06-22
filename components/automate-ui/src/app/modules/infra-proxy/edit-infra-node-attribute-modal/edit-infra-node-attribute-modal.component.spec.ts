import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { EventEmitter } from '@angular/core';
import { EditInfraNodeAttributeModalComponent } from './edit-infra-node-attribute-modal.component';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { UpdateNodeAttributesSuccess } from 'app/entities/infra-nodes/infra-nodes.actions';
import { InfraNodeAttribute } from 'app/entities/infra-nodes/infra-nodes.model';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('EditInfraNodeAttributeModalComponent', () => {
  let component: EditInfraNodeAttributeModalComponent;
  let fixture: ComponentFixture<EditInfraNodeAttributeModalComponent>;
  let store: Store<NgrxStateAtom>;

  beforeEach( waitForAsync (() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-modal',
          inputs: ['visible']
        }),
        EditInfraNodeAttributeModalComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        HttpClient, HttpHandler
      ],
      imports: [
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    store = TestBed.inject(Store);
    fixture = TestBed.createComponent(EditInfraNodeAttributeModalComponent);
    component = fixture.componentInstance;
    component.openEvent = new EventEmitter();
    fixture.detectChanges();
  });

  const node: InfraNodeAttribute = {
    server_id: 'test',
    org_id: 'test',
    name: 'node-692057300',
    attributes: ['tag2']
  };

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('edit attributes', () => {
    store.dispatch(new UpdateNodeAttributesSuccess(node));
    expect(component.updateInProgress).toBe(false);
    expect(component.visible).toBe(false);
  });
});
