import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { UserManagementComponent } from './user-management.component';

describe('UserManagementComponent', () => {
  let component: UserManagementComponent;
  let fixture: ComponentFixture<UserManagementComponent>;

  beforeEach(waitForAsync(() => {

    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'app-user-table',
            inputs: [
              'addButtonText',
              'addDescription',
              'baseUrl',
              'users',
              'removeText',
              'showEmptyMessage',
              'showTable'
            ] }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'app-create-user-modal', inputs: ['openEvent'] }),
        MockComponent({ selector: 'app-delete-object-modal',
                        inputs: ['default', 'visible', 'objectNoun', 'objectName'],
                        outputs: ['close', 'deleteClicked'] }),
        UserManagementComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      imports: [
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(UserManagementComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
