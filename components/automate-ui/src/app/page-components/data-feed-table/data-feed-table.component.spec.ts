import { waitForAsync, TestBed, ComponentFixture } from '@angular/core/testing';
import {
  CUSTOM_ELEMENTS_SCHEMA
} from '@angular/core';
import { MockComponent } from 'ng2-mock-component';
import { MockChefButton, MockChefError, MockChefFormField, MockChefHeading, MockChefIcon, MockChefLoadingSpinner, MockChefPageHeader, MockChefSubheading, MockChefTable, MockChefTbody, MockChefTd, MockChefTh, MockChefThead, MockChefToolbar, MockChefTr } from 'app/testing/mock-components';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { DataFeedTableComponent } from './data-feed-table.component';
import { DestinationRequests } from 'app/entities/destinations/destination.requests';
import { StoreModule } from '@ngrx/store';
import { Destination } from 'app/entities/destinations/destination.model';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('DataFeedTableComponent', () => {
  let component: DataFeedTableComponent;
  let fixture: ComponentFixture<DataFeedTableComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        DataFeedTableComponent
      ],
      providers: [
         DestinationRequests,
         { provide: TelemetryService, useClass: MockTelemetryService }
      ],
      imports: [
         StoreModule.forRoot(ngrxReducers, { runtimeChecks }),
        MockComponent({
        selector: 'app-create-data-feed-modal',
        inputs: ['visible', 'creating', 'conflictErrorEvent', 'createForm'],
        outputs: ['close', 'createClicked']
        }),
        MockComponent({ selector: 'app-delete-object-modal',
        inputs: ['default', 'visible', 'objectNoun', 'objectName'],
        outputs: ['close', 'deleteClicked'] }),
        MockChefButton,
        MockChefError,
        MockChefFormField,
        MockChefHeading,
        MockChefIcon,
        MockChefLoadingSpinner,
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockChefPageHeader,
        MockChefSubheading,
        MockChefToolbar,
        MockChefTable,
        MockChefThead,
        MockChefTbody,
        MockChefTr,
        MockChefTh,
        MockChefTd,
        MockComponent({ selector: 'a', inputs: ['routerLink'] })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  const destination = <Destination[]> [
    {
      id: '1',
      name: 'new data feed',
      secret: 'testSecret',
      url: 'http://foo.com'
    }, {
      id: '1',
      name: 'new data feed',
      secret: 'testSecret',
      url: 'http://foo.com'
    }, {
      id: '1',
      name: 'new data feed',
      secret: 'testSecret',
      url: 'http://foo.com'
    }, {
      id: '1',
      name: 'new data feed',
      secret: 'testSecret',
      url: 'http://foo.com'
    }
  ];


  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ DataFeedTableComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(DataFeedTableComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });


  it('it should toggle Column Dropdown', () => {
    component.destinations = destination;
    expect(component.columnDropdownVisible).toBeFalse();
    component.toggleColumnDropdown();
    expect(component.columnDropdownVisible).toBeTrue();
  });

  it('it should close Column Dropdown', () => {
    component.destinations = destination;
    expect(component.columnDropdownVisible).toBeFalse();
    component.closeColumnDropdown();
    expect(component.columnDropdownVisible).toBeFalse();
  });

  it('it should sort the destinations ASC', () => {
    component.destinations = destination;
    component.sortval = 'DESC';
    component.onToggleSort('Feedname');
    expect(component.sortval).toEqual('ASC');
  });

  it('it should sort the destinations DESC', () => {
    component.destinations = destination;
    component.sortval = 'ASC';
    component.onToggleSort('Feedname');
    expect(component.sortval).toEqual('DESC');
  });
});
