import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
// import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { InfraNodeDetailsComponent } from './infra-node-details.component';
import { MockComponent } from 'ng2-mock-component';
// import { By } from '@angular/platform-browser';
// import { InfraNode } from 'app/entities/infra-nodes/infra-nodes.model';
import {
  UpdateNodeTagsSuccess,
  GetNodeSuccess
} from 'app/entities/infra-nodes/infra-nodes.actions';
import { InfraNode } from 'app/entities/infra-nodes/infra-nodes.model';

describe('InfraNodeDetailsComponent', () => {
  // let router: Router;
  let component: InfraNodeDetailsComponent;
  let fixture: ComponentFixture<InfraNodeDetailsComponent>;
  // let element;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        MockComponent({ selector: 'chef-button',
          inputs: ['disabled', 'routerLink'] }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-option' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({ selector: 'chef-tab-selector',
          inputs: ['value', 'routerLink', 'fragment']
        }),
        InfraNodeDetailsComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(InfraNodeDetailsComponent);
    component = fixture.componentInstance;
    // element = fixture.debugElement;
    fixture.detectChanges();
    // router = TestBed.inject(Router);
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  describe('update node tags', () => {
    let store: Store<NgrxStateAtom>;
    const node: InfraNode = {
      server_id: 'test_server',
      org_id: 'test_org',
      id: 'node-692057300',
      check_in: '',
      uptime: '',
      platform: '',
      automatic_attributes: '{}',
      default_attributes: '{}',
      environment: '_default',
      name: 'node-692057300',
      normal_attributes: '{}',
      override_attributes: '{}',
      policy_group: '',
      policy_name: '',
      run_list: [],
      ip_address: '',
      fqdn: '',
      tags: ['tag2']
    };

    const tags =  ['tag1'];

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('load node details', () => {
      store.dispatch(new GetNodeSuccess(node));
      expect(component.node).not.toBeNull();
    });

    it('add the node tags', () => {
      const tagsLength = component.node.tags.length;
      store.dispatch(new UpdateNodeTagsSuccess({tags: tags}));
      expect(tagsLength).toEqual(tagsLength + 1);
    });

    it('remove the node tags', () => {
      const tagsLength = component.node.tags.length;
      store.dispatch(new UpdateNodeTagsSuccess({tags: tags}));
      expect(tagsLength).toEqual(tagsLength - 1);
    });
  });

});
