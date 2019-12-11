import { combineLatest as observableCombineLatest,  Observable, Subject } from 'rxjs';

import { distinctUntilChanged, map, takeUntil, debounceTime, startWith } from 'rxjs/operators';
import { Component, OnDestroy } from '@angular/core';
import { FormArray, FormBuilder, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { clamp, find, isEqual } from 'lodash/fp';
import { RRule } from 'rrule';
import * as moment from 'moment';

import { NgrxStateAtom } from '../../ngrx.reducers';

import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { ChefSessionService } from '../../services/chef-session/chef-session.service';
import { Job } from '../../entities/jobs/job.model';
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
import { JobGet, JobUpdate } from '../../entities/jobs/job.actions';
import { jobEntities } from '../../entities/jobs/job.selectors';
import { jobEditStatus, jobEditStep } from './job-edit.selectors';
import { Status } from './job-edit.reducer';
import {
  nodeSelectionRequiredValidator,
  profileSelectionRequiredValidator
} from '../job-add/job-add.validators';

export enum Step {
  First          = 0,
  Last           = 2,
  'add-nodes'    = 0,
  'add-profiles' = 1,
  'add-schedule' = 2
}

@Component({
  templateUrl: './job-edit.component.html',
  styleUrls: ['./job-edit.component.scss']
})
export class JobEditComponent implements OnDestroy {
  form: FormGroup;

  Step = Step;
  step$: Observable<Step>;

  status$: Observable<Status>;
  managers$: Observable<Manager[]>;
  profiles$: Observable<Profile[]>;
  job$: Observable<Job>;

  isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private fb: FormBuilder,
    private route: ActivatedRoute,
    private router: Router,
    private chefSession: ChefSessionService,
    private layoutFacade: LayoutFacadeService
  ) {
    this.layoutFacade.showSidebar(Sidebar.Compliance);
    this.managers$ = this.store.select(allManagers);
    this.profiles$ = this.store.select(allProfiles);
    this.status$ = this.store.select(jobEditStatus);
    this.step$ = this.store.select(jobEditStep).pipe(
      map(fragment => Step[fragment] || Step.First));
    this.job$ = observableCombineLatest([
        this.route.params,
        this.store.select(jobEntities)
      ]).pipe(
      map(([params, jobs]) => jobs[params['id']]));

    this.status$.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(status => {
        if (status === Status.success) {
          this.router.navigate(['/jobs']);
        }
      });

    this.form = this.setupForm();

    this.route.params.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(({ id }) => this.store.dispatch(new JobGet({ id })));

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
    const form = this.fb.group({
      id: '',
      nodesGroup,
      profilesGroup,
      scheduleGroup
    });

    observableCombineLatest([
        this.managers$,
        this.profiles$,
        this.job$
      ]).pipe(
      takeUntil(this.isDestroyed),
      debounceTime(250))
      .subscribe(([managers, profiles, job]: any) => {
        form.get('id').setValue(job.id);

        // nodemanagers form
        const managersArray = nodesGroup.get('managers') as FormArray;

        managers.forEach((manager, i) => {
          const managerId = manager.id;
          const managerNs = find(ns => ns['manager_id'] === managerId, job.node_selectors);
          const managerFilters = managerNs ? managerNs['filters'] : [];

          const namesNs = find(ns => ns['key'] === 'name', managerFilters);
          const namesGroup = this.fb.group({
            key: 'name',
            include: namesNs ? !namesNs['exclude'] : true,
            values: this.fb.array(namesNs ? namesNs['values'].map(v => this.fb.control(v)) : [])
          });

          const regionsNs = find(ns => ns['key'] === 'region', managerFilters);
          const regionsGroup = this.fb.group({
            key: 'region',
            include: regionsNs ? !regionsNs['exclude'] : true,
            values: this.fb.array(regionsNs ? regionsNs['values'].map(v => this.fb.control(v)) : [])
          });

          const tagsNs = managerFilters.filter(f => f['key'] !== 'region' && f['key'] !== 'name');
          const tagsArray = this.fb.array(
            tagsNs.map(ns => {
              const tagGroup = this.fb.group({
                key: ns['key'],
                values: this.fb.array(ns['values']),
                include: !ns['exclude']
              });
              tagGroup.get('key').valueChanges.pipe(
                startWith(tagGroup.get('key').value))
                .subscribe(key => {
                  const field = `tags:${key}`;
                  this.store.dispatch(new ManagerSearchFields({managerId, field}));
                });
              return tagGroup;
            })
          );
          const managerGroup = this.fb.group({
            id: manager.id,
            type: manager.type,
            name: manager.name,
            include: !!managerNs,
            namesGroup,
            regionsGroup,
            tagsArray
          });

          managersArray.setControl(i, managerGroup);

          managerGroup.valueChanges.pipe(
            startWith(managerGroup.value),
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

          this.store.dispatch(new ManagerAllNodes({managerId, query: {}}));

          switch (manager.type) {
            case ('automate'): {
              this.store.dispatch(new ManagerSearchFields({managerId, field: 'name'}));
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

        // profiles form
        const profilesArray = profilesGroup.get('profiles') as FormArray;

        profiles.forEach((profile, i) => {
          profilesArray.setControl(i, this.fb.group({
            id: profile.id,
            owner: profile.owner,
            name: profile.name,
            version: profile.version,
            title: profile.title,
            include: job.profiles.some(id => id === profile.id)
          }));
        });

        // schedule form
        const hasSchedule = job.recurrence.length > 0;
        scheduleGroup.get('name').setValue(job.name);
        scheduleGroup.get('include').setValue(hasSchedule);

        if (hasSchedule) {
          const rule = RRule.parseString(job.recurrence);
          const hasUntil = !!rule.until;
          const hasRepeat = !!rule.interval;
          const start = moment.utc(rule.dtstart);

          scheduleGroup.get('start.datetime').setValue({
            month: start.month(),
            date: start.date(),
            year: start.year(),
            hour: start.hour(),
            minute: start.minute()
          });

          if (hasUntil) {
            const end = moment.utc(rule.until);
            scheduleGroup.get('end.include').setValue(hasUntil);
            scheduleGroup.get('end.datetime').setValue({
              month: end.month(),
              date: end.date(),
              year: end.year(),
              hour: end.hour(),
              minute: end.minute()
            });
          }

          if (hasRepeat) {
            scheduleGroup.get('repeat.include').setValue(hasRepeat);
            scheduleGroup.get('repeat.interval').setValue(rule.interval);
            scheduleGroup.get('repeat.freq').setValue(rule.freq);
          }
        }
      });

    return form;
  }

  public submit(form) {
    const {id, nodesGroup, profilesGroup, scheduleGroup} = form.value;

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
      id,
      type: 'exec',
      tags: [],
      name,
      profiles,
      node_selectors,
      recurrence
    };

    this.store.dispatch(new JobUpdate(payload));
  }

  public recurrenceFrom(schedule) {
    if (!schedule.include) {
      return '';
    }

    const {start, end, repeat} = schedule;
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
