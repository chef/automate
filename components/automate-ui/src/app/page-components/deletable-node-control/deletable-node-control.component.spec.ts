import { TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { RouterTestingModule } from '@angular/router/testing';
import { DeletableNodeControlComponent } from './deletable-node-control.component';
import { ChefPipesModule } from '../../pipes/chef-pipes.module';
import { By } from '@angular/platform-browser';
import {
  Node
} from '../../entities/client-runs/client-runs.model';
import {
  SimpleChanges,
  SimpleChange
} from '@angular/core';

function createSampleNodes(): Node[] {
  return [
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

describe('DeletableNodeControlComponent', () => {
  let fixture, element;
  let component: DeletableNodeControlComponent;
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        ChefPipesModule
      ],
      declarations: [
        DeletableNodeControlComponent
      ],
      providers: [],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(DeletableNodeControlComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
  });

  it('should create control', () => {
    expect(component).toBeTruthy();
  });

  it('delete button should be disabled when no ' +
    'nodes are available to be selected for delete', () => {
    const changesObj: SimpleChanges = {
      nodes: new SimpleChange([], [], true)
    };
    component.isVisible = true;

    component.ngOnChanges(changesObj);

    fixture.detectChanges();

    const deleteButton = element.query(By.css('.delete-button'));
    expect(deleteButton).not.toBeNull();

    expect(deleteButton.nativeElement.disabled).toEqual(true);
  });

  it('delete button should be disabled when no nodes are selected for delete', () => {
    const sampleNodes = createSampleNodes();
    const changesObj: SimpleChanges = {
      nodes: new SimpleChange([], sampleNodes, true)
    };
    component.isVisible = true;

    component.ngOnChanges(changesObj);

    fixture.detectChanges();

    const deleteButton = element.query(By.css('.delete-button'));
    expect(deleteButton).not.toBeNull();

    expect(deleteButton.nativeElement.disabled).toEqual(true);
  });

  it('delete button should be enabled when one or more nodes are selected for delete', () => {
    fixture.detectChanges();

    const sampleNodes = createSampleNodes();
    const changesObj: SimpleChanges = {
      nodes: new SimpleChange([], sampleNodes, true)
    };
    component.isVisible = true;

    component.ngOnChanges(changesObj);

    component.selectNodeForDelete(true, sampleNodes[1]);

    fixture.detectChanges();

    const deleteButton = element.query(By.css('.delete-button'));
    expect(deleteButton).not.toBeNull();

    expect(component.selectedNodesIsEmpty()).toEqual(false);

    fixture.detectChanges();

    expect(deleteButton.nativeElement.disabled).toEqual(false);
  });
});
