import { map, distinctUntilChanged, debounceTime, takeUntil } from 'rxjs/operators';
import { Component, OnDestroy, OnInit } from '@angular/core';
import { FormArray, FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Observable, Subject } from 'rxjs';
import { select, Store } from '@ngrx/store';
import { clamp, isEqual } from 'lodash/fp';
import { RRule } from 'rrule';
import * as moment from 'moment/moment';

import { NgrxStateAtom } from '../../ngrx.reducers';

import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { ChefSessionService } from '../../services/chef-session/chef-session.service';
import { Manager } from '../../entities/managers/manager.model';
import { Profile } from '../../entities/profiles/profile.model';
import { allManagers, fieldsByManager, nodesByManager, totalcountNode } from '../../entities/managers/manager.selectors';
import { allProfiles } from '../../entities/profiles/profile.selectors';
import {
  ManagerSearchFields,
  ManagerSearchNodes,
  ManagerAllNodes,
  ManagersSearch,
  FirstLoad
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
export class JobAddComponent implements OnDestroy , OnInit {
  form: FormGroup;

  Step = Step;
  step$: Observable<Step>;

  status$: Observable<Status>;
  managers$: Observable<Manager[]>;
  profiles$: Observable<Profile[]>;



  private isDestroyed = new Subject<boolean>();
  public pagenumber = 1;
  public total: number;
  public scrollCalled = false;
  public loadMore = false;
  public notFirstTime = false;
  public managersList: any;
  public managersArray: any;
  public fieldCounter = 0;
  public appendDataOnscrollLater: boolean;
  public managerId = [];
  public searchName: [];
  public nodeArray: [] ;
  public nodeManagerArray: any;
  public checked: any;
  public counter: number;
  public nodeCount = 10;
  public fieldCount = 20;
  public model = { search: '', nodearray: '' };
  public earlierManagers: Manager[] = [];
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

    this.store.select(totalcountNode).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe((total) => {
      this.total = total;
    });

    this.setupForm();


    this.store.dispatch(new ProfilesSearch({ owner: this.chefSession.username }));
  }

  ngOnInit() {
    this.store.dispatch(new FirstLoad());
    this.store.dispatch(
      new ManagersSearch({
        page: this.pagenumber,
        per_page: 10
      })
    );
    this.store.pipe(
      select(fieldsByManager),
      takeUntil(this.isDestroyed)
    ).subscribe(res => {
      this.fieldCounter = 0 ;
      this.nodeManagerArray.forEach((manager) => {
        if (manager.id in res) {
          if (!res[manager.id].loadingAllTotalFields) {
            this.fieldCounter++;
          }
        }
      });
      if (this.nodeManagerArray.length === this.nodeCount &&
        this.counter + this.fieldCounter === this.fieldCount ) {
        this.loadMore = true;
      }
    });

    this.store.pipe(
      select(nodesByManager),
      takeUntil(this.isDestroyed)
    ).subscribe(res => {
      this.counter = 0;
      this.nodeManagerArray.forEach((manager) => {
        if (manager.id in res) {
          if (!res[manager.id].loadingAllTotal) {
            this.counter++;
            if (manager.type === 'aws-api' || manager.type === 'gcp-api') {
              this.fieldCounter = this.fieldCounter + 1;
            }
          }
        }
      });
      if (this.nodeManagerArray.length === this.nodeCount &&
        this.counter + this.fieldCounter === this.fieldCount ) {
        this.loadMore = true;
      }
    });
}

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public setupForm() {
    const nodesGroup = this.fb.group({
      managers: this.fb.array([])
    }, {
      validators: nodeSelectionRequiredValidator
    });
    const profilesGroup = this.fb.group({
      allSelected: false,
      someSelected: false,
      profiles: this.fb.array([])
    }, {
      validators: profileSelectionRequiredValidator
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
        this.nodeManagerArray = managers;
        this.managersArray = nodesGroup.controls['managers'] as FormArray;
        if (this.notFirstTime) {
          this.managersList = [...this.managersList, ...managers];
          this.notFirstTime = false;
        } else {
          this.managersArray.clear();
          this.pagenumber = 1;
          this.managersList = managers;

        }
        this.managersList.forEach((manager, i) => {
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

          this.managersArray.setControl(i, managerGroup);

          if (!this.earlierManagers.includes(manager)) {
            this.earlierManagers.push(manager);
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
      dtstart: new Date(Date.UTC(
        parseInt(start.datetime.year, 10),
        parseInt(start.datetime.month, 10),
        parseInt(start.datetime.date, 10),
        parseInt(start.datetime.hour, 10),
        parseInt(start.datetime.minute, 10)
      ))
    };


    if (end.include) {
      ruleOpts['until'] = new Date(Date.UTC(
        parseInt(end.datetime.year, 10),
        parseInt(end.datetime.month, 10),
        parseInt(end.datetime.date, 10),
        parseInt(end.datetime.hour, 10),
        parseInt(end.datetime.minute, 10)
      ));
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

  onLoadFunc(data) {
    this.notFirstTime = true;
    this.loadMore = false;
    let payload = {};
    this.searchName = data.search;
    this.nodeArray = data.nodearray;
    this.checked = data.checked;

    if (this.searchName === null && this.nodeArray.length === 0) {
      this.store.dispatch(new ManagersSearch({
        page: ++this.pagenumber,
        per_page: 10
      }));
    } else {
      if (this.searchName === null) {
        payload = {
          filter_map: [
            {
              key: 'manager_type',
              values: this.nodeArray
            }
          ],
          page: ++this.pagenumber,
          per_page: 10
        };
      } else if (this.searchName && this.nodeArray.length > 0) {
        payload = {
          filter_map: [
            {
              key: 'manager_type',
              values: this.nodeArray
            },
            {
              key: 'name',
              values: this.searchName
            }
          ],
          page: ++this.pagenumber,
          per_page: 10
        };
      } else if (this.searchName && this.nodeArray.length === 0) {
        payload = {
          filter_map: [
            {
              key: 'name',
              values: this.searchName
            }
          ],
          page: ++this.pagenumber,
          per_page: 10
        };
      }
      this.store.dispatch(new ManagersSearch(payload));
    }
  }

  clickCallChild(data) {
    this.searchName = data.search;
    this.nodeArray = data.nodearray;
    this.checked = data.checked;
    let payload = {};
    this.loadMore = false;
    if (this.searchName === null && this.nodeArray.length === 0) {
      this.store.dispatch(new ManagersSearch({
        page: 1,
        per_page: 10
      }));
    } else {
      if (this.searchName === null) {
        payload = {
          filter_map: [
            {
              key: 'manager_type',
              values: this.nodeArray
            }
          ],
          page: 1,
          per_page: 10
        };
      } else if (this.searchName && this.nodeArray.length > 0) {
        payload = {
          filter_map: [
            {
              key: 'manager_type',
              values: this.nodeArray
            },
            {
              key: 'name',
              values: this.searchName
            }
          ],
          page: 1,
          per_page: 10
        };
      } else if (this.searchName && this.nodeArray.length === 0) {
        payload = {
          filter_map: [
            {
              key: 'name',
              values: this.searchName
            }
          ],
          page: 1,
          per_page: 10
        };
      }
      this.store.dispatch(new ManagersSearch(payload));
    }
  }
}
