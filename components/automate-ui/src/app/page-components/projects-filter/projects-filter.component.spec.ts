import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { StoreModule } from '@ngrx/store';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';

import { runtimeChecks } from 'app/ngrx.reducers';
import {
  projectsFilterInitialState,
  projectsFilterReducer
} from 'app/services/projects-filter/projects-filter.reducer';
import { ProjectsFilterService } from 'app/services/projects-filter/projects-filter.service';
import { ProjectsFilterComponent } from './projects-filter.component';

describe('ProjectsFilterComponent', () => {
  let fixture: ComponentFixture<ProjectsFilterComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
      declarations: [
        ProjectsFilterComponent
      ],
      providers: [
        ProjectsFilterService
      ],
      imports: [
        RouterTestingModule,
        StoreModule.forRoot({
          projectsFilter: projectsFilterReducer
        }, {
          initialState: { projectsFilter: projectsFilterInitialState },
          runtimeChecks
        })
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ProjectsFilterComponent);
    fixture.detectChanges();
  });

  it('displays a projects filter dropdown', () => {
    const projectsFilter = fixture.nativeElement.querySelector('app-projects-filter-dropdown');
    expect(projectsFilter).not.toBeNull();
  });
});
