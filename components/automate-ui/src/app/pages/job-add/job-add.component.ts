import { map, distinctUntilChanged, debounceTime, takeUntil } from 'rxjs/operators';
import { Component, OnDestroy } from '@angular/core';
import { FormArray, FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Observable, Subject } from 'rxjs';
import { Store } from '@ngrx/store';
import { clamp, isEqual } from 'lodash/fp';
import { RRule } from 'rrule';
import * as moment from 'moment';

import { NgrxStateAtom } from '../../ngrx.reducers';

import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { ChefSessionService } from '../../services/chef-session/chef-session.service';
import { Manager } from '../../entities/managers/manager.model';
import { Profile } from '../../entities/profiles/profile.model';
import { allManagers } from '../../entities/managers/manager.selectors';
import { allProfiles } from '../../entities/profiles/profile.selectors';
import {
  ManagersSearch,
  ManagerSearchFields,
  ManagerSearchNodes,
  ManagerAllNodes
} from '../../entities/managers/manager.actions';
import { ProfilesSearch } from '../../entities/profiles/profile.actions';
import { JobCreate } from '../../entities/jobs/job.actions';

import { jobAddStatus, jobAddStep } from './job-add.selectors';
import { Status } from './job-add.reducer';
import {
  nodeSelectionRequiredValidator,
  profileSelectionRequiredValidator
} from './job-add.validators';

export enum Step {
  First          = 0,
  Last           = 2,
  'add-nodes'    = 0,
  'add-profiles' = 1,
  'add-schedule' = 2
}

@Component({
  templateUrl: './job-add.component.html',
  styleUrls: ['./job-add.component.scss']
})
export class JobAddComponent implements OnDestroy {
  form: FormGroup;

  Step = Step;
  step$: Observable<Step>;

  status$: Observable<Status>;
  managers$: Observable<Manager[]>;
  profiles$: Observable<Profile[]>;

  isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private fb: FormBuilder,
    private router: Router,
    private chefSession: ChefSessionService,
    private layoutFacade: LayoutFacadeService
  ) {
    this.layoutFacade.showSidebar(Sidebar.Compliance);
    this.managers$ = this.store.select(allManagers);
    this.profiles$ = this.store.select(allProfiles);
    this.status$ = this.store.select(jobAddStatus);
    this.step$ = this.store.select(jobAddStep).pipe(
      map(fragment => Step[fragment] || Step.First));

    this.status$.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(status => {
        if (status === Status.success) {
          this.router.navigate(['/jobs']);
        }
      });

    this.setupForm();

    this.store.dispatch(new ManagersSearch({}));
    this.store.dispatch(new ProfilesSearch({ owner: this.chefSession.username }));
  }

  public ngOnDestroy() {
    this.isDestroyed.next(true);
  }

  public setupForm() {
    const nodesGroup = this.fb.group({
      managers: this.fb.array([])
    }, {
      validator: nodeSelectionRequiredValidator
    });
    const profilesGroup = this.fb.group({
      allSelected: false,
      someSelected: false,
      profiles: this.fb.array([])
    }, {
      validator: profileSelectionRequiredValidator
    });
    const defaultStart = moment.utc();
    const defaultEnd = moment.utc(defaultStart).add(1, 'days');
    const scheduleGroup = this.fb.group({
      name: ['', Validators.required],
      include: false,
      start: this.fb.group({
        datetime: this.fb.group({
          month: defaultStart.month(),
          date: defaultStart.date(),
          year: defaultStart.year(),
          hour: defaultStart.hour(),
          minute: defaultStart.minute()
        })
      }),
      end: this.fb.group({
        datetime: this.fb.group({
          month: defaultEnd.month(),
          date: defaultEnd.date(),
          year: defaultEnd.year(),
          hour: defaultEnd.hour(),
          minute: defaultEnd.minute()
        }),
        include: false
      }),
      repeat: this.fb.group({
        interval: 1,
        freq: RRule.DAILY,
        include: false
      })
    });

    this.form = this.fb.group({
      nodesGroup,
      profilesGroup,
      scheduleGroup
    });

    this.managers$.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(managers => {
        const managersArray = nodesGroup.controls['managers'] as FormArray;

        managers.forEach((manager, i) => {
          const managerId = manager.id;
          const namesGroup = this.fb.group({
            key: 'name',
            include: true,
            values: this.fb.array([])
          });
          const regionsGroup = this.fb.group({
            key: 'region',
            include: true,
            values: this.fb.array([])
          });
          const tagsArray = this.fb.array([]);
          const managerGroup = this.fb.group({
            id: manager.id,
            type: manager.type,
            name: manager.name,
            include: false,
            namesGroup,
            regionsGroup,
            tagsArray
          });

          managerGroup.valueChanges.pipe(
            debounceTime(250),
            map(value => {
              const filter_map = this.nodeFiltersFor(value);
              const query = {query: {filter_map}};
              return {managerId: value.id, query};
            }),
            distinctUntilChanged((a, b) => isEqual(a, b)),
            takeUntil(this.isDestroyed))
            .subscribe(payload => {
              this.store.dispatch(new ManagerSearchNodes(payload));
            });

          managersArray.setControl(i, managerGroup);

          this.store.dispatch(new ManagerAllNodes({managerId, query: {query: {filter_map: []}}}));

          switch (manager.type) {
            case ('automate'): {
              this.store.dispatch(new ManagerSearchFields({managerId, field: 'name'}));
              this.store.dispatch(new ManagerSearchFields({managerId, field: 'tags'}));
              break;
            }
            case ('aws-ec2'):
            case ('azure-vm'):
            case ('azure-api'): {
              this.store.dispatch(new ManagerSearchFields({managerId, field: 'regions'}));
              this.store.dispatch(new ManagerSearchFields({managerId, field: 'tags'}));
              break;
            }
          }
        });
      });

    this.profiles$.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(profiles => {
        const profilesArray = profilesGroup.controls['profiles'] as FormArray;

        profiles.forEach((profile, i) => {
          const profileGroup = this.fb.group({
            id: profile.id,
            owner: profile.owner,
            name: profile.name,
            version: profile.version,
            title: profile.title,
            include: false
          });
          profilesArray.setControl(i, profileGroup);
        });
      });
  }

  public submit(form) {
    const {nodesGroup, profilesGroup, scheduleGroup} = form.value;

    const name = scheduleGroup.name;
    const recurrence = this.recurrenceFrom(scheduleGroup);

    const profiles = profilesGroup.profiles
      .filter(profile => profile.include)
      .map(profile => profile.id);

    const node_selectors = nodesGroup.managers
      .filter(manager => manager.include)
      .map(manager => {
        const filters = this.nodeFiltersFor(manager);
        return {manager_id: manager.id, filters};
      });

    const payload = {
      type: 'exec',
      tags: [],
      name,
      profiles,
      node_selectors,
      recurrence
    };

    this.store.dispatch(new JobCreate(payload));
  }

  public recurrenceFrom(scheduleOpts) {
    if (!scheduleOpts.include) {
      return '';
    }

    const {start, end, repeat} = scheduleOpts;
    const ruleOpts = {
      dtstart: new Date(
        parseInt(start.datetime.year, 10),
        parseInt(start.datetime.month, 10),
        parseInt(start.datetime.date, 10),
        parseInt(start.datetime.hour, 10),
        parseInt(start.datetime.minute, 10)
      )
    };

    if (end.include) {
      ruleOpts['until'] = new Date(
        parseInt(end.datetime.year, 10),
        parseInt(end.datetime.month, 10),
        parseInt(end.datetime.date, 10),
        parseInt(end.datetime.hour, 10),
        parseInt(end.datetime.minute, 10)
      );
    }


    if (repeat.include) {
      ruleOpts['freq'] = repeat.freq;
      ruleOpts['interval'] = repeat.interval;
    }

    return RRule.optionsToString(ruleOpts);
  }

  public nodeFiltersFor(managerGroup) {
    const {namesGroup, regionsGroup, tagsArray} = managerGroup;
    const filtersArray = [...tagsArray];

    if (namesGroup.values.length) {
      filtersArray.push(namesGroup);
    }

    if (regionsGroup.values.length) {
      filtersArray.push(regionsGroup);
    }

    return filtersArray
      .filter(tag => {
        const {key, values} = tag;
        return key.length && values.some(v => v.length);
      })
      .map(tag => {
        const {values} = tag;
        return {...tag, values: values.filter(v => v.length)};
      })
      .map(tag => {
        const {key, values, include} = tag;
        return {key, values, exclude: !include};
      });
  }

  public nextStep(current: Step): Step {
    return clamp(Step.First, Step.Last, current + 1);
  }

  public prevStep(current: Step): Step {
    return clamp(Step.First, Step.Last, current - 1);
  }

  public nextStepFragment(current: Step): string {
    return Step[this.nextStep(current)];
  }

  public prevStepFragment(current: Step): string {
    return Step[this.prevStep(current)];
  }

  public isFirstStep(current: Step): boolean {
    return current === Step.First;
  }

  public isLastStep(current: Step): boolean {
    return current === Step.Last;
  }

  public isCurrentStep(current: Step, step: Step): boolean {
    return current === step;
  }

  public isStepValid(step: Step): boolean {
    switch (step) {
      case Step['add-nodes']:
        return this.form.get('nodesGroup').valid;
      case Step['add-profiles']:
        return this.form.get('profilesGroup').valid;
      case Step['add-schedule']:
        return this.form.get('scheduleGroup').valid;
    }
    return false;
  }
}
