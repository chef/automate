import { ReactiveFormsModule } from '@angular/forms';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { EventEmitter } from '@angular/core';
import { EditInfraNodeModalComponent } from './edit-infra-node-modal.component';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { UpdateNodeSuccess } from 'app/entities/infra-nodes/infra-nodes.actions';
import { InfraNode } from 'app/entities/infra-nodes/infra-nodes.model';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('EditInfraNodeModalComponent', () => {
  let component: EditInfraNodeModalComponent;
  let fixture: ComponentFixture<EditInfraNodeModalComponent>;
  let store: Store<NgrxStateAtom>;

  beforeEach( waitForAsync(() => {
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
        EditInfraNodeModalComponent
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
    fixture = TestBed.createComponent(EditInfraNodeModalComponent);
    component = fixture.componentInstance;
    component.openEvent = new EventEmitter();
    fixture.detectChanges();
  });

  const node: InfraNode = {
    server_id: 'test',
    org_id: 'test',
    name: 'node-692057300',
    environment: 's',
    policy_name: '',
    policy_group: '',
    run_list: ['role[test-saveee]', 'role[test-drag-drop]', 'role[test-himanshi]', 'role[test-role_1]'],
    tags: ['tag2'],
    automatic_attributes: '{}',
    default_attributes: '{}',
    normal_attributes: '{}',
    override_attributes: '{}'
  };

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('update the run list', () => {
    store.dispatch(new UpdateNodeSuccess(node));
    expect(component.updateInProgress).toBe(false);
    expect(component.visible).toBe(false);
  });
});
