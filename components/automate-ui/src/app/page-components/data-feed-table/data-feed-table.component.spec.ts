import { waitForAsync, TestBed } from '@angular/core/testing';
import {
  CUSTOM_ELEMENTS_SCHEMA,
 // Component,
  //Input
} from '@angular/core';
import { MockComponent } from 'ng2-mock-component';
import { ngrxReducers,runtimeChecks} from 'app/ngrx.reducers';
import { DataFeedTableComponent } from './data-feed-table.component';
import { Destination } from 'app/entities/destinations/destination.model';
import { DestinationRequests } from 'app/entities/destinations/destination.requests';
import { StoreModule } from '@ngrx/store';

//

describe('DataFeedTableComponent', () => {
  let component: DataFeedTableComponent;
  //let fixture: ComponentFixture<DataFeedTableComponent>;
   let fixture

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        DataFeedTableComponent,
        MockComponent({
        selector: 'app-create-data-feed-modal',
        inputs: ['visible', 'creating', 'conflictErrorEvent', 'createForm'],
        outputs: ['close', 'createClicked']
        }),
        MockComponent({ selector: 'app-delete-object-modal',
        inputs: ['default', 'visible', 'objectNoun', 'objectName'],
        outputs: ['close', 'deleteClicked'] }),
        MockComponent({ selector: 'chef-button',
                inputs: ['disabled', 'routerLink'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] })
      ],
      providers: [
        // FeatureFlagsService,
         DestinationRequests,
        // HttpClient,
        // HttpHandler
      ],
      imports: [
        // FormsModule,
        // ReactiveFormsModule,
        // RouterTestingModule,
         StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));


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



  // describe('initialization', () => {
  //   it('should render a table', () => {
  //     fixture.detectChanges();
  //     expect(
  //       element.nativeElement.querySelector('chef-table > chef-thead')).not.toBe(null);
  //     expect(
  //       element.nativeElement.querySelector('chef-table > chef-tbody')).not.toBe(null);
  //   });
  // });

  it('it should toggle Column Dropdown', () => {
    expect(component.columnDropdownVisible).toBeFalsy();
    component.toggleColumnDropdown();
    expect(component.columnDropdownVisible).toBeTruthy();
  });

  it('it should close Column Dropdown', () => {
    expect(component.columnDropdownVisible).toBeFalsy();
    component.closeColumnDropdown();
    expect(component.columnDropdownVisible).toBeFalsy();
  });

  // it('it should sort the destinations', () => {
  //   component.onToggleSort('ASC');
  //   expect(component.sortVal).toEqual('DESC');
  //   //expect(destinations).toEqual()
  // });






});