import { TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA, DebugElement, Component, Input } from '@angular/core';
import { RouterTestingModule } from '@angular/router/testing';
import { ClientRunsTableComponent } from './client-runs-table.component';
import { ChefPipesModule } from '../../pipes/chef-pipes.module';
import { MockComponent } from 'ng2-mock-component';
import { By } from '@angular/platform-browser';
import { DeletableNodeControlComponent
} from '../deletable-node-control/deletable-node-control.component';
import {
  Node
} from '../../entities/client-runs/client-runs.model';
import { StoreModule } from '@ngrx/store';
import * as sidebar from '../../services/sidebar/sidebar.reducer';
import {
  SimpleChanges,
  SimpleChange
} from '@angular/core';

@Component({
  selector: 'app-authorized',
  template: '<ng-content *ngIf="visible"></ng-content>'
})
class MockAllowedAuthorizedComponent {
  public visible = true;
  @Input() anyOf: any;
  @Input() allOf: any;
  @Input() overrideVisible = true;
}

@Component({
  selector: 'app-authorized',
  template: '<ng-content *ngIf="visible"></ng-content>'
})
class MockDisallowedAuthorizedComponent {
  public visible = false;
  @Input() anyOf: any;
  @Input() allOf: any;
  @Input() overrideVisible = false;
}

function createSampleNodes(): Node[] {
  return [
    {
      id: '56343c09-e968-43b3-b896-edd7cca03fbd',
      name: 'A-non-architecto',
      fqdn: 'A-non-architecto.bergnaum.co',
      checkin: null,
      uptime_seconds: 17112079,
      environment: 'test',
      platform: 'solaris',
      policy_group: '',
      organization: '',
      source_fqdn: '',
      status: 'failure',
      latestRunId: '22efcb97-ae0d-4ece-b284-51bc73b2c6cc',
      hasRuns: true,
      lastCcrReceived: new Date(),
      deprecationsCount: 0,
      chefVersion: '12.6.0'
    },
    {
      id: '96989735-5a77-499d-a4c7-b3a1144efaaf',
      name: 'Ad-fugiat-optio',
      fqdn: 'Ad-fugiat-optio.bergnaum.co',
      source_fqdn: '',
      checkin: null,
      uptime_seconds: 11377524,
      organization: '',
      environment: '',
      platform: 'centos',
      policy_group: 'dev',
      status: 'success',
      latestRunId: '',
      hasRuns: false,
      lastCcrReceived: new Date(),
      deprecationsCount: 0,
      chefVersion: '12.6.0'
    },
    {
      id: '96989735-5a77-499d-a4c7-b3a1144efaaf',
      name: 'Ad-fugiat-optio',
      fqdn: 'Ad-fugiat-optio.bergnaum.co',
      source_fqdn: '',
      checkin: null,
      uptime_seconds: 11377524,
      organization: '',
      environment: '',
      platform: 'centos',
      policy_group: 'dev',
      status: 'missing',
      latestRunId: '',
      hasRuns: false,
      lastCcrReceived: new Date(),
      deprecationsCount: 0,
      chefVersion: '12.6.0'
    },
    {
      id: '96989735-5a77-499d-a4c7-b3a1144efaaf',
      name: 'database',
      fqdn: 'Ad-fugiat-optio.bergnaum.or',
      source_fqdn: '',
      checkin: null,
      uptime_seconds: 113774,
      organization: '',
      environment: '',
      platform: 'ubuntu',
      policy_group: 'dev',
      status: 'missing',
      latestRunId: '',
      hasRuns: false,
      lastCcrReceived: new Date(),
      deprecationsCount: 0,
      chefVersion: '12.6.0'
    }
  ];
}

describe('ClientRunsTable', () => {
  let fixture, element;
  let component: ClientRunsTableComponent;

  describe('user does not have permissions to delete nodes', () => {
    beforeEach(() => {
      TestBed.configureTestingModule({
        imports: [
          RouterTestingModule,
          ChefPipesModule,
          StoreModule.forRoot({
            sidebar: sidebar.sidebarReducer
          })
        ],
        declarations: [
          ClientRunsTableComponent,
          DeletableNodeControlComponent,
          MockDisallowedAuthorizedComponent,
          MockComponent({ selector: 'chef-tooltip' }),
          MockComponent({ selector: 'chef-dropdown' })
        ],
        providers: [
        ],
        schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
      });

      fixture = TestBed.createComponent(ClientRunsTableComponent);
      component = fixture.componentInstance;
      element = fixture.debugElement;
      component.defaultFieldDirection = {
        name: 'ASC',
        checkin: 'DESC',
        uptime_seconds: 'DESC',
        platform: 'ASC',
        environment: 'ASC',
        policy_group: 'ASC',
        chef_version: 'ASC',
        deprecations_count: 'ASC'
      };
      component.columns = {
        check_in: true,
        uptime: true,
        platform: true,
        environment: true,
        policy_group: true,
        chef_version: false,
        deprecations_count: false
      };
      component.selectedSortField = 'name';
      component.selectedFieldDirection = component.defaultFieldDirection['name'];
      component.canDeleteNodes = false;
    });

    describe('DeletableNodeControl', () => {

      it('there is no delete column header checkbox', () => {
          const sampleNodes = createSampleNodes();
          const changesObj: SimpleChanges = {
            nodes: new SimpleChange([], sampleNodes, true)
          };
          component.ngOnChanges(changesObj);

          expect(component.deletableNodes.length).toEqual(2);
          expect(component.nodes.length).toEqual(4);

          fixture.detectChanges();

          const checkbox = element.query(By.css('.delete-checkbox.header'));
          expect(checkbox).toBeNull();
      });

      it('there is no delete column row checkboxes', () => {
        const sampleNodes = createSampleNodes();
        const changesObj: SimpleChanges = {
          nodes: new SimpleChange([], sampleNodes, true)
        };

        component.ngOnChanges(changesObj);

        fixture.detectChanges();

        const checkboxes: DebugElement[] =
          element.queryAll(By.css('.delete-checkbox.row'));
        expect(checkboxes).toEqual([]);
      });

      it('there is no delete button in the deletable node control', () => {
        fixture.detectChanges();

        const sampleNodes = createSampleNodes();
        const changesObj: SimpleChanges = {
          nodes: new SimpleChange([], sampleNodes, true)
        };

        component.ngOnChanges(changesObj);

        fixture.detectChanges();

        const deletableNodeControl: DebugElement =
          element.query(By.css('app-deletable-node-control'));
        const deleteButton = deletableNodeControl.query(By.css('.delete-button'));
        expect(deleteButton).toBeNull();
      });
    });
  });

  describe('user has permissions to delete nodes', () => {
    beforeEach(() => {
      TestBed.configureTestingModule({
        imports: [
          RouterTestingModule,
          ChefPipesModule,
          StoreModule.forRoot({
            sidebar: sidebar.sidebarReducer
          })
        ],
        declarations: [
          ClientRunsTableComponent,
          DeletableNodeControlComponent,
          MockAllowedAuthorizedComponent,
          MockComponent({ selector: 'chef-tooltip' }),
          MockComponent({ selector: 'chef-checkbox' }),
          MockComponent({ selector: 'chef-dropdown' })
        ],
        providers: [
        ],
        schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
      });

      fixture = TestBed.createComponent(ClientRunsTableComponent);
      component = fixture.componentInstance;
      element = fixture.debugElement;
      component.defaultFieldDirection = {
        name: 'ASC',
        checkin: 'DESC',
        uptime_seconds: 'DESC',
        platform: 'ASC',
        environment: 'ASC',
        policy_group: 'ASC',
        chef_version: 'ASC',
        deprecations_count: 'ASC'
      };
      component.columns = {
        check_in: true,
        uptime: true,
        platform: true,
        environment: true,
        policy_group: true,
        chef_version: false,
        deprecations_count: false
      };
      component.selectedSortField = 'name';
      component.selectedFieldDirection = component.defaultFieldDirection['name'];
      component.canDeleteNodes = true;
    });

    describe('DeletableNodeControl', () => {
      it('there is a delete button in the deletable node control', () => {
        fixture.detectChanges();

        const sampleNodes = createSampleNodes();
        const changesObj: SimpleChanges = {
          nodes: new SimpleChange([], sampleNodes, true)
        };

        component.ngOnChanges(changesObj);

        fixture.detectChanges();

        const deletableNodeControl: DebugElement =
          element.query(By.css('app-deletable-node-control'));
        const deleteButton = deletableNodeControl.query(By.css('.delete-button'));
        expect(deleteButton).not.toBeNull();
      });

      it('column header checkbox is indeterminate when' +
        ' one node is selected and one is not selected', () => {
          const sampleNodes = createSampleNodes();
          const changesObj: SimpleChanges = {
            nodes: new SimpleChange([], sampleNodes, true)
          };
          component.ngOnChanges(changesObj);

          expect(component.deletableNodes.length).toEqual(2);
          expect(component.nodes.length).toEqual(4);

          fixture.detectChanges();

          const checkbox = element.query(By.css('.delete-checkbox.row'));
          expect(checkbox).not.toBeNull();

          checkbox.triggerEventHandler('change', { detail: true });

          fixture.detectChanges();

          expect(checkbox.nativeElement.checked).toEqual(true);

          const headerCheckbox = element.query(By.css('.delete-checkbox.header'));
          expect(headerCheckbox).not.toBeNull();

          expect(headerCheckbox.nativeElement.indeterminate).toEqual(true);
      });

      it('column header checkbox is checked when all nodes are selected', () => {
        const sampleNodes = createSampleNodes();
        const changesObj: SimpleChanges = {
          nodes: new SimpleChange([], sampleNodes, true)
        };

        component.ngOnChanges(changesObj);

        fixture.detectChanges();

        const checkboxes: DebugElement[] =
          element.queryAll(By.css('.delete-checkbox.row'));
        expect(checkboxes).not.toBeNull();

        checkboxes.forEach((checkbox) => checkbox.triggerEventHandler('change', { detail: true }));

        fixture.detectChanges();

        const headerCheckbox = element.query(By.css('.delete-checkbox.header'));
        expect(headerCheckbox).not.toBeNull();

        fixture.detectChanges();

        expect(headerCheckbox.nativeElement.checked).toEqual(true);
        expect(headerCheckbox.nativeElement.indeterminate).toEqual(false);
      });

      it('column header checkbox is unchecked when no nodes are checked', () => {
        fixture.detectChanges();

        const sampleNodes = createSampleNodes();
        const changesObj: SimpleChanges = {
          nodes: new SimpleChange([], sampleNodes, true)
        };

        component.ngOnChanges(changesObj);

        fixture.detectChanges();

        const headerCheckbox = element.query(By.css('.delete-checkbox.header'));
        expect(headerCheckbox).not.toBeNull();

        fixture.detectChanges();

        expect(headerCheckbox.nativeElement.checked).toEqual(false);
        expect(headerCheckbox.nativeElement.indeterminate).toEqual(false);
      });

      it('column header checkbox is disabled when there are no deletable nodes', () => {
        fixture.detectChanges();

        const changesObj: SimpleChanges = {
          nodes: new SimpleChange([], [], true)
        };

        component.ngOnChanges(changesObj);

        fixture.detectChanges();

        const headerCheckbox = element.query(By.css('.delete-checkbox.header'));
        expect(headerCheckbox).not.toBeNull();

        fixture.detectChanges();

        expect(headerCheckbox.nativeElement.disabled).toEqual(true);
        expect(headerCheckbox.nativeElement.indeterminate).toEqual(false);
      });

      it('when the column header checkbox is checked all nodes are selected', () => {
        const sampleNodes = createSampleNodes();

        const changesObj: SimpleChanges = {
          nodes: new SimpleChange([], sampleNodes, true)
        };

        component.ngOnChanges(changesObj);

        fixture.detectChanges();

        const headerCheckbox = element.query(By.css('.delete-checkbox.header'));
        expect(headerCheckbox).not.toBeNull();

        headerCheckbox.triggerEventHandler('change', { detail: true });

        fixture.detectChanges();

        expect(headerCheckbox.nativeElement.disabled).toEqual(false);
        expect(headerCheckbox.nativeElement.indeterminate).toEqual(false);
        expect(headerCheckbox.nativeElement.checked).toEqual(true);

        const checkboxes: DebugElement[] =
          element.queryAll(By.css('.delete-checkbox.row'));
        expect(checkboxes).not.toBeNull();
        expect(checkboxes.length).toEqual(2);

        checkboxes.forEach((checkbox) => {
          expect(checkbox.nativeElement.checked).toEqual(true);
        });

        // when the column header checkbox is unchecked when all nodes are selected
        // then all nodes will be unselected
        headerCheckbox.triggerEventHandler('change', { detail: false });

        fixture.detectChanges();

        checkboxes.forEach((checkbox) => {
          expect(checkbox.nativeElement.checked).toEqual(false);
        });
      });
    });

    describe('dynamic table columns', () => {
      // it('display policy column when all nodes are policy nodes', () => {
      //   spyOn(MockNodesService.prototype, 'policyNodeCount').and.returnValue(observableOf(20));
      //   spyOn(MockNodesService.prototype, 'totalNodeCount').and.returnValue(observableOf(20));
      //   fixture.detectChanges();
      //   expect(component.displayPolicyNodes).toBe(true);
      // });
      //
      // it('do not display environment column when all nodes are policy nodes', () => {
      //   spyOn(MockNodeListService.prototype, 'policyNodeCount')
      //     .and.returnValue(observableOf(20));
      //   spyOn(MockNodeListService.prototype, 'totalNodeCount').and.returnValue(observableOf(20));
      //   fixture.detectChanges();
      //   expect(component.displayEnvironmentNodes).toBe(false);
      // });
      //
      // it('display environment column when all nodes are Environment/Roles nodes', () => {
      //   spyOn(MockNodeListService.prototype, 'policyNodeCount').and.returnValue(observableOf(0));
      //   spyOn(MockNodeListService.prototype, 'totalNodeCount').and.returnValue(observableOf(20));
      //   fixture.detectChanges();
      //   expect(component.displayEnvironmentNodes).toBe(true);
      // });
      //
      // it('do not display policy column when all nodes are Environment/Roles nodes', () => {
      //   spyOn(MockNodeListService.prototype, 'policyNodeCount').and.returnValue(observableOf(0));
      //   spyOn(MockNodeListService.prototype, 'totalNodeCount').and.returnValue(observableOf(20));
      //   fixture.detectChanges();
      //   expect(component.displayPolicyNodes).toBe(false);
      // });

      // tslint:disable-next-line:max-line-length
      it('display both policy and environment columns when both Policy File and Environment/Roles nodes are present', () => {
        // spyOn(MockNodeListService.prototype, 'policyNodeCount')
        //   .and.returnValue(observableOf(10));
        // spyOn(MockNodeListService.prototype, 'totalNodeCount').and.returnValue(observableOf(20));
        fixture.detectChanges();
        expect(component.displayPolicyNodes).toBe(true);
        expect(component.displayEnvironmentNodes).toBe(true);
      });
    });

    describe('initialization', () => {
      it('should render a table', () => {
        fixture.detectChanges();
        expect(element.nativeElement.querySelector('chef-table > chef-thead')).not.toBe(null);
        expect(element.nativeElement.querySelector('chef-table > chef-tbody')).not.toBe(null);
      });
    });

    describe('when the node-list service returns', () => {
      it('should render a row for each node', () => {
        const sampleNodes = createSampleNodes();
        const changesObj: SimpleChanges = {
          nodes: new SimpleChange([], sampleNodes, true)
        };

        component.ngOnChanges(changesObj);

        fixture.detectChanges();

        expect(element.nativeElement.querySelectorAll('chef-table > chef-tbody > chef-tr').length)
          .toBe(4);
      });
    });

    describe('clicking the Node Name heading', () => {
      it('should toggle the list sort', () => {
        const th = element.nativeElement.querySelector('chef-th:nth-child(1)');
        fixture.detectChanges();

        expect(component.defaultFieldDirection['name']).toBe('ASC');
        expect(component.sortIcon('name')).toBe('sort-asc');
        expect(component.sortIcon('environment')).toBe('sort');

        spyOn(component.updateSort, 'emit');
        fixture.detectChanges();
        th.click();

        expect(component.updateSort.emit).toHaveBeenCalledWith(
          {field: 'name', fieldDirection: 'DESC'});
      });
    });

    describe('clicking the Check-in heading', () => {
      it('should emit the list sort for Check-in in descending order', () => {
        const th = element.nativeElement.querySelector('chef-th:nth-child(2)');
        fixture.detectChanges();

        expect(component.defaultFieldDirection['name']).toBe('ASC');
        expect(component.defaultFieldDirection['checkin']).toBe('DESC');
        expect(component.selectedSortField).toBe('name');

        spyOn(component.updateSort, 'emit');
        fixture.detectChanges();
        th.click();

        expect(component.updateSort.emit).toHaveBeenCalledWith(
          {field: 'checkin', fieldDirection: 'DESC'});
      });
    });

    describe('nodes with no data', () => {
      it('should not have links', () => {
        const sampleNodes = createSampleNodes();
        const changesObj: SimpleChanges = {
          nodes: new SimpleChange([], sampleNodes, true)
        };

        component.ngOnChanges(changesObj);

        fixture.detectChanges();
        // There are four nodes 8 links per node. The example as on node that
        // has data and the other that does not have data. Therefore there should only be 9 anchors
        expect(element.nativeElement.querySelectorAll('chef-td > a').length).toBe(9);
      });
    });
  });
});
