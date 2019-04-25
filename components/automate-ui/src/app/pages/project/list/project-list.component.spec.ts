import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { of as observableOf } from 'rxjs';
import { MockComponent } from 'ng2-mock-component';

import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { customMatchers } from 'app/testing/custom-matchers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { IAMType } from 'app/entities/policies/policy.model';
import { GetProjectsSuccess } from 'app/entities/projects/project.actions';
import { projectEntityReducer } from 'app/entities/projects/project.reducer';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
import { ProjectListComponent } from './project-list.component';

describe('ProjectListComponent', () => {
  let component: ProjectListComponent;
  let fixture: ComponentFixture<ProjectListComponent>;
  let element: HTMLElement;

  beforeEach(async(() => {

    TestBed.configureTestingModule({
      declarations: [
        MockComponent({
          selector: 'chef-toolbar',
          template: '<ng-content></ng-content>'
        }),
         MockComponent({
          selector: 'app-authorized',
          inputs: ['allOf', 'not'],
          template: '<ng-content></ng-content>'
        }),
        MockComponent({ selector: 'app-admin-sidebar' }),
        MockComponent({
          selector: 'app-delete-object-modal',
          inputs: ['visible', 'objectNoun', 'objectName', 'moreDetails'],
          outputs: ['close', 'deleteClicked']
        }),
        MockComponent({
          selector: 'app-create-object-modal',
          inputs: ['creating', 'createForm', 'visible', 'objectNoun', 'conflictErrorEvent'],
          outputs: ['close', 'deleteClicked']
        }),
       MockComponent({ selector: 'chef-control-menu' }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-option' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        ProjectListComponent
      ],
      imports: [
        ReactiveFormsModule,
        RouterTestingModule,
        ChefPipesModule,
        StoreModule.forRoot({
          policies: policyEntityReducer,
          projects: projectEntityReducer
        })
      ],
      providers: [
        FeatureFlagsService
      ]
    }).compileComponents();
  }));

  let store: Store<NgrxStateAtom>;
  beforeEach(() => {
    store = TestBed.get(Store);

    store.dispatch(new GetProjectsSuccess({
      projects: [
        {
          id: 'uuid-1', name: 'Default',
          type: <IAMType>'CHEF_MANAGED'
        },
        {
          id: 'uuid-2', name: 'another-project',
          type: <IAMType>'CUSTOM'
        },
        {
          id: 'uuid-5', name: 'zzz-project',
          type: <IAMType>'CUSTOM'
        }
      ]
    }));

    jasmine.addMatchers(customMatchers);
    fixture = TestBed.createComponent(ProjectListComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement.nativeElement;
    fixture.detectChanges();
  });

  describe('when there are no projects', () => {
  });

  describe('when there are projects', () => {
    beforeEach(() => {
      store.dispatch(new GetProjectsSuccess({
        projects: [
          {
            id: 'uuid-1', name: 'Default',
            type: <IAMType>'CHEF_MANAGED'
          },
          {
            id: 'uuid-2', name: 'another-project',
            type: <IAMType>'CUSTOM'
          },
          {
            id: 'uuid-5', name: 'zzz-project',
            type: <IAMType>'CUSTOM'
          }
        ]
      }));
    });

    it('displays project data for v2', () => {
      component.iamMajorVersion$ = observableOf('v2');
      component.iamMinorVersion$ = observableOf('v1');
      fixture.detectChanges();
      expect(element).toContainPath('chef-table');
    });

    it('does not display project data for v1', () => {
      component.iamMajorVersion$ = observableOf('v1');
      component.iamMinorVersion$ = observableOf('v0');
      fixture.detectChanges();
      expect(element).not.toContainPath('chef-table');
    });

    describe('create modal', () => {
      it('create modal opens upon clicking create button', () => {
        component.iamMajorVersion$ = observableOf('v2');
        component.iamMinorVersion$ = observableOf('v1');
        fixture.detectChanges();
        expect(component.createModalVisible).toBe(false);
        (<HTMLButtonElement>(element.querySelector('#create-button'))).click();
        expect(component.createModalVisible).toBe(true);
      });

      it('opening create modal resets name to empty string', () => {
        component.iamMajorVersion$ = observableOf('v2');
        component.iamMinorVersion$ = observableOf('v1');
        fixture.detectChanges();
        component.createProjectForm.controls['name'].setValue('any');
        (<HTMLButtonElement>(element.querySelector('#create-button'))).click();
        expect(component.createProjectForm.controls['name'].value).toBe(null);
      });
    });
  });

  describe('sortedProject$', () => {
    it('intermixes capitals and lowercase with lowercase first', () => {
      store.dispatch(new GetProjectsSuccess({
        projects: [
          {
            id: 'uuid-1', name: 'Default',
            type: <IAMType>'CHEF_MANAGED'
          },
          {
            id: 'uuid-2', name: 'another-project',
            type: <IAMType>'CUSTOM'
          },
          {
            id: 'uuid-5', name: 'zzz-project',
            type: <IAMType>'CUSTOM'
          }
        ]
      }));
      component.sortedProjects$.subscribe(projects => {
        expect(projects.length).toBe(3);
        expect(projects[0]).toEqual(jasmine.objectContaining({ name: 'another-project' }));
        expect(projects[1]).toEqual(jasmine.objectContaining({ name: 'Default' }));
        expect(projects[2]).toEqual(jasmine.objectContaining({ name: 'zzz-project' }));
      });
    });

    it('sorts by whole string before case', () => {
      store.dispatch(new GetProjectsSuccess({
        projects: [
          {
            id: 'uuid-2', name: 'default',
            type: <IAMType>'CUSTOM'
          },
          {
            id: 'uuid-4', name: 'default-resources',
            type: <IAMType>'CUSTOM'
          },
          {
            id: 'uuid-5', name: 'Default',
            type: <IAMType>'CUSTOM'
          }
        ]
      }));
      component.sortedProjects$.subscribe(projects => {
        expect(projects.length).toBe(3);
        expect(projects[0]).toEqual(jasmine.objectContaining({ name: 'default' }));
        expect(projects[1]).toEqual(jasmine.objectContaining({ name: 'Default' }));
        expect(projects[2]).toEqual(jasmine.objectContaining({ name: 'default-resources' }));
      });
    });

    it('uses natural ordering', () => {
      store.dispatch(new GetProjectsSuccess({
        projects: [
          {
            id: 'uuid-1', name: 'Project01',
            type: <IAMType>'CHEF_MANAGED'
          },
          {
            id: 'uuid-2', name: 'Project300',
            type: <IAMType>'CUSTOM'
          },
          {
            id: 'uuid-3', name: 'Project3',
            type: <IAMType>'CUSTOM'
          },
          {
            id: 'uuid-4', name: 'Project-2',
            type: <IAMType>'CUSTOM'
          },
          {
            id: 'uuid-6', name: 'project',
            type: <IAMType>'CHEF_MANAGED'
          }
        ]
      }));
      component.sortedProjects$.subscribe(projects => {
        expect(projects.length).toBe(5);
        expect(projects[0]).toEqual(jasmine.objectContaining({ name: 'project' }));
        expect(projects[1]).toEqual(jasmine.objectContaining({ name: 'Project-2' }));
        expect(projects[2]).toEqual(jasmine.objectContaining({ name: 'Project01' }));
        expect(projects[3]).toEqual(jasmine.objectContaining({ name: 'Project3' }));
        expect(projects[4]).toEqual(jasmine.objectContaining({ name: 'Project300' }));
      });
    });
  });
});
