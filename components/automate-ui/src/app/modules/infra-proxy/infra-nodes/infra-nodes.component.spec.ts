import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { InfraNodesComponent } from './infra-nodes.component';
import { RouterTestingModule } from '@angular/router/testing';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { By } from '@angular/platform-browser';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { InfraNode } from 'app/entities/infra-nodes/infra-nodes.model';
import { GetNodesSuccess } from 'app/entities/infra-nodes/infra-nodes.actions';

describe('InfraNodesComponent', () => {
  let component: InfraNodesComponent;
  let fixture: ComponentFixture<InfraNodesComponent>;
  let element;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
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
        MockComponent({
          selector: 'app-delete-infra-object-modal',
          inputs: ['default', 'visible', 'objectNoun', 'objectName'],
          outputs: ['close', 'deleteNode']
        }),
        MockComponent({
          selector: 'app-update-node-tag-modal',
          inputs: ['openEvent', 'serverId', 'orgId', 'name']
        }),
        InfraNodesComponent
      ],
      providers: [
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
    fixture = TestBed.createComponent(InfraNodesComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('infra node list', () => {
    let store: Store<NgrxStateAtom>;
    const availableNodes: InfraNode[] = [{
      server_id: 'test_server_id',
      org_id: 'test_org_id',
      name: 'test',
      fqdn: 'xyz.com',
      ip_address: '1.1.1.1',
      id: 'c93e4823-c1e7-4bcd-83a9-d6fe65c87a10',
      platform: 'ubuntu',
      policy_group: '',
      policy_name: '',
      run_list: [],
      uptime: '37 minutes 00 seconds',
      check_in: '37 minutes 00 seconds',
      environment: 'test',
      default_attributes: 'test',
      override_attributes: 'test',
      normal_attributes: 'test',
      automatic_attributes: 'test',
      tags: []
    }];

    const emptyNodes: InfraNode[] = [];

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('render the nodes list', () => {
      store.dispatch(new GetNodesSuccess({nodes: availableNodes,  total: availableNodes.length}));
      expect(component.nodes.length).not.toBeNull();
      expect(element.query(By.css('.empty-section'))).toBeNull();
    });

    it('show no preview image', () => {
      store.dispatch(new GetNodesSuccess({nodes: emptyNodes,  total: emptyNodes.length}));
      expect(component.nodes.length).toBe(0);
    });
  });
});
