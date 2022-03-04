import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { InfraNodesComponent } from './infra-nodes.component';
import { RouterTestingModule } from '@angular/router/testing';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { By } from '@angular/platform-browser';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { InfraNode, InfraNodeAttribute } from 'app/entities/infra-nodes/infra-nodes.model';
import { GetNodesSuccess, GetNodeSuccess, UpdateNodeAttributesSuccess } from 'app/entities/infra-nodes/infra-nodes.actions';
import { GetNodeRunlistsSuccess } from 'app/entities/nodeRunlists/nodeRunlists.action';
import { NodeRunlist } from 'app/entities/nodeRunlists/nodeRunlists.model';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

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
          selector: 'app-reset-node-key',
          inputs: ['openEvent', 'serverId', 'orgId', 'name']
        }),
        MockComponent({
          selector: 'app-update-node-tag-modal',
          inputs: ['openEvent', 'serverId', 'orgId', 'name']
        }),
        MockComponent({
          selector: 'app-edit-infra-node-modal',
          inputs: ['label', 'openEvent', 'orgId', 'availableType',
            'node', 'serverId', 'selected', 'runlistError'],
          outputs: ['closeRunlist']
        }),
        MockComponent({
          selector: 'app-edit-infra-node-attribute-modal',
          inputs: ['openEvent', 'serverId', 'orgId', 'node', 'jsonText', 'isGetNode']
        }),
        InfraNodesComponent
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

  describe('edit run list', () => {
    let store: Store<NgrxStateAtom>;
    const node: InfraNode = {
      server_id: 'chef-server-dev-test',
      org_id: 'chef-org-dev',
      id: 'node-692057300',
      tags: ['tag2'],
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
      run_list: ['recipe[centos-cookbook-file]', 'recipe[chef-client]'],
      ip_address: '',
      fqdn: ''
    };

    const runlist: NodeRunlist = {
      id: 'environment',
      run_list: []
    };

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('open edit modal ', () => {
      component.editRunlist(node);
      expect(component.editRunlistLoading).toEqual(true);
    });

    it('get infra node ', () => {
      component.getNode(node);
      store.dispatch(new GetNodeSuccess(node));
      expect(component.nodeToEditRunlist).not.toBeNull();
    });

    it('load run list ', () => {
      component.loadNodeRunlist(node);
      store.dispatch(new GetNodeRunlistsSuccess(runlist));
      expect(component.runlist.length).not.toBeNull();
    });
  });

  describe('edit attributes', () => {
    let store: Store<NgrxStateAtom>;
    const node: InfraNode = {
      server_id: 'chef-server-dev-test',
      org_id: 'chef-org-dev',
      id: 'node-692057300',
      tags: ['tag2'],
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
      run_list: ['recipe[centos-cookbook-file]', 'recipe[chef-client]'],
      ip_address: '',
      fqdn: ''
    };

    const nodeAttribute: InfraNodeAttribute = {
      server_id: 'chef-server-dev-test',
      org_id: 'chef-org-dev',
      name: 'node-692057300',
      attributes: []
    };

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('open edit attributes modal ', () => {
      component.editAttributes(node);
      expect(component.editAttributesLoading).toEqual(true);
    });

    it('get infra node ', () => {
      component.getNode(node);
      store.dispatch(new GetNodeSuccess(node));
      expect(component.nodeToEditRunlist).not.toBeNull();
    });

    it('load attributes ', () => {
      component.loadAttributes(node);
      store.dispatch(new UpdateNodeAttributesSuccess(nodeAttribute));
      expect(component.attributes).not.toBeNull();
    });
  });
});
