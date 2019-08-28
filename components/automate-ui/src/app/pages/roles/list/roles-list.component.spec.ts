import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { GetRolesSuccess } from 'app/entities/roles/role.actions';
import { roleEntityReducer } from 'app/entities/roles/role.reducer';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
import { RolesListComponent } from './roles-list.component';

describe('RolesListComponent', () => {
  let component: RolesListComponent;
  let fixture: ComponentFixture<RolesListComponent>;

  beforeEach(async(() => {

    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'app-authorized',
                        inputs: ['allOf'],
                        template: '<ng-content></ng-content>' }),
        MockComponent({ selector: 'app-settings-sidebar' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'a', inputs: ['routerLink']}),
        RolesListComponent
      ],
      imports: [
        ChefPipesModule,
        StoreModule.forRoot({
          policies: policyEntityReducer,
          roles: roleEntityReducer
        })
      ],
      providers: [
        FeatureFlagsService
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(RolesListComponent);
    component = fixture.componentInstance;

    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  describe('sortedRoles$', () => {
    let store: Store<NgrxStateAtom>;
    beforeEach(() => {
      store = TestBed.get(Store);
   });

    it('intermixes capitals and lowercase with lowercase first', () => {
      store.dispatch(new GetRolesSuccess({ roles: [
        { id: 'uuid-1', name: 'Viewer',
          actions: [], type: 'CHEF_MANAGED' },
        { id: 'uuid-2', name: 'developer',
          actions: [], type:  'CUSTOM' },
        { id: 'uuid-5', name: 'Developer',
          actions: [], type:  'CUSTOM' },
        { id: 'uuid-6', name: 'viewer',
          actions: [], type:  'CHEF_MANAGED' }
      ]}));
       component.sortedRoles$.subscribe(roles => {
        expect(roles.length).toBe(4);
        expect(roles[0]).toEqual(jasmine.objectContaining({ name: 'developer' }));
        expect(roles[1]).toEqual(jasmine.objectContaining({ name: 'Developer' }));
        expect(roles[2]).toEqual(jasmine.objectContaining({ name: 'viewer' }));
        expect(roles[3]).toEqual(jasmine.objectContaining({ name: 'Viewer' }));
      });
    });

    it('sorts by whole string before case', () => {
      store.dispatch(new GetRolesSuccess({ roles: [
        { id: 'uuid-2', name: 'developer',
          actions: [], type:  'CUSTOM' },
        { id: 'uuid-4', name: 'developer-Manager',
          actions: [], type:  'CUSTOM' },
        { id: 'uuid-5', name: 'Developer',
          actions: [], type:  'CUSTOM' }
      ]}));
       component.sortedRoles$.subscribe(roles => {
        expect(roles.length).toBe(3);
        expect(roles[0]).toEqual(jasmine.objectContaining({ name: 'developer' }));
        expect(roles[1]).toEqual(jasmine.objectContaining({ name: 'Developer' }));
        expect(roles[2]).toEqual(jasmine.objectContaining({ name: 'developer-Manager' }));
      });
    });

    it('uses natural ordering', () => {
      store.dispatch(new GetRolesSuccess({ roles: [
        { id: 'uuid-1', name: 'Viewer01',
          actions: [], type: 'CHEF_MANAGED' },
        { id: 'uuid-2', name: 'Viewer300',
          actions: [], type: 'CUSTOM' },
        { id: 'uuid-3', name: 'Viewer3',
          actions: [], type: 'CUSTOM' },
        { id: 'uuid-4', name: 'Viewer-2', // does not fit in same grouping
          actions: [], type: 'CUSTOM' },
        { id: 'uuid-6', name: 'viewer',
          actions: [], type: 'CHEF_MANAGED' }
      ]}));
       component.sortedRoles$.subscribe(roles => {
        expect(roles.length).toBe(5);
        expect(roles[0]).toEqual(jasmine.objectContaining({ name: 'viewer' }));
        expect(roles[1]).toEqual(jasmine.objectContaining({ name: 'Viewer-2' }));
        expect(roles[2]).toEqual(jasmine.objectContaining({ name: 'Viewer01' }));
        expect(roles[3]).toEqual(jasmine.objectContaining({ name: 'Viewer3' }));
        expect(roles[4]).toEqual(jasmine.objectContaining({ name: 'Viewer300' }));
      });
    });
  });
});
