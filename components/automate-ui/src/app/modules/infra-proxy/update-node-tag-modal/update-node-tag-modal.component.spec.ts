import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { Store, StoreModule } from '@ngrx/store';
import { RouterTestingModule } from '@angular/router/testing';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { EventEmitter } from '@angular/core';
import { UpdateNodeTagModalComponent } from './update-node-tag-modal.component';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { UpdateNodeTagsSuccess } from 'app/entities/infra-nodes/infra-nodes.actions';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('UpdateNodeTagModalComponent', () => {
  let component: UpdateNodeTagModalComponent;
  let fixture: ComponentFixture<UpdateNodeTagModalComponent>;

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
        UpdateNodeTagModalComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        FeatureFlagsService
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(UpdateNodeTagModalComponent);
    component = fixture.componentInstance;
    component.openEvent = new EventEmitter();
    fixture.detectChanges();
  });

  describe('update node tag', () => {
    let store: Store<NgrxStateAtom>;
    const add_tags =  ['tag1, tag2, tag3'];

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('add the node tags', () => {
      store.dispatch(new UpdateNodeTagsSuccess({tags: add_tags}));
      expect(component.visible).toBe(false);
      expect(component.updatingTags).toBe(false);
    });
  });
});
