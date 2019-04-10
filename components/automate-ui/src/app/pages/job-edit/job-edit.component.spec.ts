import { CUSTOM_ELEMENTS_SCHEMA, DebugElement } from '@angular/core';
import { By } from '@angular/platform-browser';
import { RouterTestingModule } from '@angular/router/testing';
import { ActivatedRoute } from '@angular/router';
import { of as observableOf } from 'rxjs';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers } from '../../ngrx.reducers';
import { ChefSessionService } from '../../services/chef-session/chef-session.service';
import { JobEditComponent, Step } from './job-edit.component';
import { Job } from '../../entities/jobs/job.model';
import { JobGetSuccess } from '../../entities/jobs/job.actions';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';

const changeStep = fragment => {
  return {
    type: 'ROUTER_NAVIGATION',
    payload: {
      routerState: {
        url: '/jobs/1/edit',
        fragment
      },
      event: {
        id: 1
      }
    }
  };
};

describe('JobEditComponent', () => {
  let fixture: ComponentFixture<JobEditComponent>;
  let component: JobEditComponent;
  let element: DebugElement;
  let store: Store<NgrxStateAtom>;
  const job: Job = {
    id: '1',
    name: 'job-1',
    node_count: 0,
    end_time: '2018-01-01T00:00:00Z',
    recurrence: ''
  };

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers)
      ],
      declarations: [
        JobEditComponent
      ],
      providers: [
        { provide: ChefSessionService, useClass: MockChefSessionService },
        {
          provide: ActivatedRoute,
          useValue: {
            params: observableOf({ id: job.id }),
            snapshot: {
              url: [
                { path: 'jobs' },
                { path: job.id },
                { path: 'edit' }
              ]
            }
          }
        }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(JobEditComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    store = TestBed.get(Store);

    store.dispatch(new JobGetSuccess(job));
  });

  describe('nextStep', () => {
    it('returns the next step', () => {
      expect(component.nextStep(Step.First)).toBe(Step.First + 1);
    });

    it('will NOT return a step higher than the last step', () => {
      expect(component.nextStep(Step.Last)).toBe(Step.Last);
      expect(component.nextStep(Step.Last + 1)).toBe(Step.Last);
      expect(component.nextStep(Step.Last + 100)).toBe(Step.Last);
    });

    it('will NOT return a step lower than the first step', () => {
      expect(component.nextStep(Step.First - 1)).toBe(Step.First);
      expect(component.nextStep(Step.First - 100)).toBe(Step.First);
    });
  });

  describe('prevStep', () => {
    it('returns the previous step', () => {
      expect(component.prevStep(Step.Last)).toBe(Step.Last - 1);
    });

    it('will NOT return a step higher than the last step', () => {
      expect(component.prevStep(Step.Last + 1)).toBe(Step.Last);
      expect(component.prevStep(Step.Last + 100)).toBe(Step.Last);
    });

    it('will NOT return a step lower than the first step', () => {
      expect(component.prevStep(Step.First)).toBe(Step.First);
      expect(component.prevStep(Step.First - 1)).toBe(Step.First);
      expect(component.prevStep(Step.First - 100)).toBe(Step.First);
    });
  });

  describe('Navigation', () => {
    it('shows the `add nodes` step by default', () => {
      fixture.detectChanges();

      const scanNodesForm = element.query(By.css('chef-job-nodes-form'));
      expect(scanNodesForm).not.toBeNull();
    });

    it('shows the `add nodes` step when it is explicitly navigated to', () => {
      store.dispatch(changeStep(Step[2]));
      fixture.detectChanges();

      let scanNodesForm = element.query(By.css('chef-job-nodes-form'));
      expect(scanNodesForm).toBeNull();

      store.dispatch(changeStep(Step[0]));
      fixture.detectChanges();

      scanNodesForm = element.query(By.css('chef-job-nodes-form'));
      expect(scanNodesForm).not.toBeNull();
    });

    it('shows the `add profiles` step', () => {
      store.dispatch(changeStep(Step[1]));
      fixture.detectChanges();

      const profilesForm = element.query(By.css('chef-job-profiles-form'));
      expect(profilesForm).not.toBeNull();
    });

    it('shows the `add schedule` step', () => {
      store.dispatch(changeStep(Step[2]));
      fixture.detectChanges();

      const scheduleForm = element.query(By.css('chef-job-schedule-form'));
      expect(scheduleForm).not.toBeNull();
    });
  });
});
