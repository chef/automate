import { TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { MockComponent } from 'ng2-mock-component';
import { ChefComponentsModule } from '../../components/chef-components.module';
import { NodeDetailsService } from '../../services/node-details/node-details.service';
import { DeltaViewerComponent } from '../delta-viewer/delta-viewer.component';
import { ResourcesComponent } from './resources.component';
import { ResourceItemComponent } from '../resource-item/resource-item.component';
import { ChefStatusIconPipe } from '../../pipes/chef-status-icon.pipe';
import { ChefPipesModule } from '../../pipes/chef-pipes.module';

describe('ResourcesComponent', () => {
  let fixture, component, element;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        ChefComponentsModule,
        ChefPipesModule
      ],
      declarations: [
        DeltaViewerComponent,
        ResourcesComponent,
        ResourceItemComponent,
        ChefStatusIconPipe,
        MockComponent({selector: 'chef-icon'})
      ],
      providers: [
        NodeDetailsService
      ]
    });

    fixture = TestBed.createComponent(ResourcesComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
  });

  it('renders the component correctly', () => {
    expect(element.query(By.css('.resources-rollup'))).not.toBeNull();
  });

  it('calculates totals correctly', () => {

    const data = [
      ({'status': 'failed', 'id': '1'}),
      ({'status': 'updated', 'id': '2'}),
      ({'status': 'up-to-date', 'id': '3'}),
      ({'status': 'skipped', 'id': '4'}),
      ({'status': 'failed', 'id': '5'}),
      ({'status': 'unprocessed', 'id': '6'})
    ];

    component.calculateTotals(data);
    expect(component.resourcesTotal).toBe(6);
    expect(component.failedTotal).toBe(2);
    expect(component.successTotal).toBe(1);
    expect(component.unchangedTotal).toBe(2);
    expect(component.unprocessedTotal).toBe(1);
  });

  it('sets a step value for each resource', () => {

   const resources: any = [
      ({'status': 'failed', 'id': '1'}),
      ({'status': 'updated', 'id': '2'})
    ];

    component.calculateTotals(resources);
    expect(resources[0].step).toBe('1/2');
    expect(resources[1].step).toBe('2/2');
  });
});

