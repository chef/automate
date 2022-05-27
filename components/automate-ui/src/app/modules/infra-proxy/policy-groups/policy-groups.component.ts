import {
  Component,
  Input,
  OnInit,
  OnDestroy,
  EventEmitter,
  Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';
import { GetPolicyGroups } from 'app/entities/policy-files/policy-file.action';
import { PolicyFile } from 'app/entities/policy-files/policy-file.model';
import {
  getGroupsStatus,
  policyFile
} from 'app/entities/policy-files/policy-group.selectors';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-policy-groups',
  templateUrl: './policy-groups.component.html',
  styleUrls: ['./policy-groups.component.scss']
})
export class PolicyGroupsComponent implements OnInit, OnDestroy {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  private isDestroyed = new Subject<boolean>();
  public policyFiles: PolicyFile[] = [];
  public policyGroupsListLoading = true;
  public authFailure = false;
  public searching = false;
  public searchValue = '';
  public groupList = [];
  public pageOfItems: Array<any>;
  public searchFlag: boolean;
  public policyGroups: Array<any>;
  public currentPage = 1;
  public per_page = 100;
  public total: number;
  public searchArrayLength: number;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private telemetryService: TelemetryService
  ) { }

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.getPolicyGropus();

    combineLatest([
      this.store.select(getGroupsStatus),
      this.store.select(policyFile)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([ getPolicyFilesSt, policyFilesState]) => {
      if (getPolicyFilesSt === EntityStatus.loadingSuccess && !isNil(policyFilesState)) {
        this.policyFiles = policyFilesState;
        this.groupList = this.policyFiles;
        this.policyGroupsListLoading = false;
        this.total = this.policyFiles.length;
        this.filterDataGroupWise();
      } else if (getPolicyFilesSt === EntityStatus.loadingFailure) {
        this.policyGroupsListLoading = false;
        this.authFailure = true;
      }
    });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

  getPolicyGropus() {
    const payload = {
      server_id: this.serverId,
      org_id: this.orgId
    };
    this.store.dispatch(new GetPolicyGroups(payload));
  }

  filterDataGroupWise() {
    const key = 'policy_group';
    this.policyGroups = [];
    if (this.groupList.length > 0) {
      this.groupList.forEach((x) => {
        // Checking if there is any object in this.policyGroupsList
        // which contains the key value
        if (this.policyGroups.some((val) => val[key] === x[key])) {
          // If yes! then increase the occurrence by 1
          this.policyGroups.forEach((k) => {
            if (k[key] === x[key]) {
              k['occurrence']++;
            }
          });
        } else {
          // If not! Then create a new object initialize
          // it with the present iteration key's value and set the occurrence to 1
          const listArr = {};
          listArr[key] = x[key];
          listArr['occurrence'] = 1;
          this.policyGroups.push(listArr);
        }
      });
    } else {
      this.policyGroups = [];
    }
  }

  searchPolicyFiles(searchText: string): void {
    this.searching = true;
    this.searchValue = searchText;
    if (!this.policyGroups || !searchText) {
      this.groupList = this.policyGroups;
      this.searchFlag = false;
    } else {
      const list = this.policyGroups.filter((key) => {
        this.searchFlag = true;
        if (key) {
          return key.policy_group.includes(this.searchValue);
        }
      });
      this.groupList = list;
    }
    this.searching = false;
    if (this.groupList !== undefined) {
      this.searchArrayLength = this.groupList.length;
    }
    this.telemetryService.track('InfraServer_PolicyGroups_Search');
  }

  onChangePage($event: { page: number; pageOfItems: Array<any> }) {
    this.pageOfItems = $event.pageOfItems;
    this.currentPage = $event.page;
  }

  onUpdatePage($event: { pageIndex: number; pageSize: number; }) {
    this.currentPage = $event.pageIndex + 1;
    this.per_page = $event.pageSize;
  }
}
