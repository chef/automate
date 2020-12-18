import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { MatOptionSelectionChange } from '@angular/material/core/option';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { using } from 'app/testing/spec-helpers';
import { NgrxStateAtom, runtimeChecks, ngrxReducers } from 'app/ngrx.reducers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { customMatchers } from 'app/testing/custom-matchers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { ProjectStatus } from 'app/entities/rules/rule.model';
import { ProjectService } from 'app/entities/projects/project.service';
import {
  GetProjectsSuccess
} from 'app/entities/projects/project.actions';
import { Project } from 'app/entities/projects/project.model';
import { ProjectListComponent } from './project-list.component';

describe('ProjectListComponent', () => {
  let component: ProjectListComponent;
  let fixture: ComponentFixture<ProjectListComponent>;
  let element: HTMLElement;
  let store: Store<NgrxStateAtom>;

  const projectList: Project[] = [
      {
        id: 'uuid-1', name: 'Default',
        type: 'CHEF_MANAGED',
        status: 'NO_RULES'
      },
      {
        id: 'uuid-2', name: 'another-project',
        type: 'CUSTOM',
        status: 'NO_RULES'
      },
      {
        id: 'uuid-5', name: 'zzz-project',
        type: 'CUSTOM',
        status: 'NO_RULES'
      }
    ];

  beforeEach(waitForAsync(() => {

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
        MockComponent({
          selector: 'app-delete-object-modal',
          inputs: ['visible', 'objectNoun', 'objectName' ],
          outputs: ['close', 'deleteClicked']
        }),
        MockComponent({
          selector: 'app-create-object-modal',
          inputs: ['creating', 'createForm', 'visible', 'objectNoun',
            'conflictErrorEvent', 'createProjectModal', 'resetPoliciesCheckboxEvent'],
          outputs: ['close', 'deleteClicked']
        }),
        MockComponent({
          selector: 'app-message-modal',
          inputs: ['visible' ],
          outputs: ['close' ]
        }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
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
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      providers: [
        FeatureFlagsService,
        ProjectService
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    jasmine.addMatchers(customMatchers);
    fixture = TestBed.createComponent(ProjectListComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement.nativeElement;
    store = TestBed.inject(Store);

    fixture.detectChanges();
  });

  describe('when there are no projects', () => {
    it('displays no projects', () => {
      component.sortedProjects$.subscribe(results => {
        expect(results.length).toBe(0);
      });
    });
  });

  describe('when there are projects', () => {

    it('displays project data', () => {
      store.dispatch(new GetProjectsSuccess({ projects: projectList }));
      fixture.detectChanges();
      expect(element).toContainPath('chef-table');
      component.sortedProjects$.subscribe(results => {
        expect(results.length).toBe(projectList.length);
        projectList.forEach(p => {
          expect(results.some(result => result.id === p.id)).toBe(true);
        });
      });
    });

   describe('create modal', () => {
      it('opens upon clicking create button', () => {
        store.dispatch(new GetProjectsSuccess({ projects: projectList }));
        fixture.detectChanges();
        expect(component.createModalVisible).toBe(false);
        (<HTMLButtonElement>(element.querySelector('[data-cy=create-project]'))).click();
        expect(component.createModalVisible).toBe(true);
      });

      it('resets name to empty string', () => {
        store.dispatch(new GetProjectsSuccess({ projects: projectList }));
        fixture.detectChanges();
        component.createProjectForm.controls['name'].setValue('any');
        (<HTMLButtonElement>(element.querySelector('[data-cy=create-project]'))).click();
        expect(component.createProjectForm.controls['name'].value).toBe(null);
      });
    });

    describe('delete modal', () => {
      const mockEvent = { isUserInput: true } as MatOptionSelectionChange;

      using([
        ['NO_RULES'],
        ['PROJECT_RULES_STATUS_UNSET']
      ], function (status: ProjectStatus) {
        it(`upon selecting delete from control menu, opens with ${status}`, () => {
          expect(component.deleteModalVisible).toBe(false);
          component.startProjectDelete(mockEvent, genProject('uuid-111', status));
          expect(component.deleteModalVisible).toBe(true);
        });
      });

      using([
        ['RULES_APPLIED'],
        ['EDITS_PENDING']
      ], function (status: ProjectStatus) {
        it(`upon selecting delete from control menu, does not open with ${status}`, () => {
          expect(component.deleteModalVisible).toBe(false);
          component.startProjectDelete(mockEvent, genProject('uuid-111', status));
          expect(component.deleteModalVisible).toBe(false);
        });
      });

     it('closes upon sending request to back-end', () => {
       component.startProjectDelete(mockEvent, genProject('uuid-111', 'NO_RULES'));
        expect(component.deleteModalVisible).toBe(true);
        component.deleteProject();
        expect(component.deleteModalVisible).toBe(false);
      });

    });

    describe('message modal', () => {
      const mockEvent = { isUserInput: true } as MatOptionSelectionChange;

      using([
        ['RULES_APPLIED'],
        ['EDITS_PENDING']
      ], function (status: ProjectStatus) {
        it(`upon selecting delete from control menu, opens with ${status}`, () => {
          expect(component.messageModalVisible).toBe(false);
          component.startProjectDelete(mockEvent, genProject('uuid-111', status));
          expect(component.messageModalVisible).toBe(true);
        });
      });

      using([
        ['NO_RULES'],
        ['PROJECT_RULES_STATUS_UNSET']
      ], function (status: ProjectStatus) {
        it(`upon selecting delete from control menu, does not open with ${status}`, () => {
          expect(component.messageModalVisible).toBe(false);
          component.startProjectDelete(mockEvent, genProject('uuid-111', status));
          expect(component.messageModalVisible).toBe(false);
        });
      });

      it('closes upon request', () => {
        component.startProjectDelete(mockEvent, genProject('uuid-111', 'EDITS_PENDING'));
        expect(component.messageModalVisible).toBe(true);
        component.closeMessageModal();
        expect(component.messageModalVisible).toBe(false);
      });

    });
  });

  function genProject(id: string, status: ProjectStatus): Project {
    return {
      id,
      status,
      name: id, // unused
      type: 'CUSTOM' // unused
    };
  }
});
