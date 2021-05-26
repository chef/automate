import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { isNil } from 'lodash/fp';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus, pending } from 'app/entities/entities';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams } from 'app/route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity } from 'lodash/fp';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import {
  infraNodeFromRoute,
  updateTagsStatus,
  updateEnvStatus,
  nodeTags,
  nodeEnvironment
} from 'app/entities/infra-nodes/infra-nodes.selectors';
import {
  getAllStatus,
  environmentList
} from 'app/entities/environments/environment.selectors';
import {
  GetNode,
  UpdateNodeTags,
  UpdateNodeEnvironment
} from 'app/entities/infra-nodes/infra-nodes.actions';
import { GetEnvironments } from 'app/entities/environments/environment.action';
import { InfraNode } from 'app/entities/infra-nodes/infra-nodes.model';
import { Environment } from 'app/entities/environments/environment.model';


export type InfraNodeTabName = 'details';

@Component({
  selector: 'app-infra-node-details',
  templateUrl: './infra-node-details.component.html',
  styleUrls: ['./infra-node-details.component.scss']
})

export class InfraNodeDetailsComponent implements OnInit, OnDestroy {
  public conflictError = false;
  public InfraNode: InfraNode;
  public tabValue: InfraNodeTabName = 'details';
  public serverId: string;
  public orgId: string;
  public name: string;
  public environmentName: string;
  public nodeDetailsLoading = true;
  public updateNodeForm: FormGroup;
  private isDestroyed = new Subject<boolean>();

  // for tags
  public tags: string[];
  public nodeTags: string[];
  public removeTags: string[] = [];
  public inputTxt = '';
  public updatingTags = false;


  // for environments
  public environmentListState: { items: Environment[], total: number };
  public environments: Environment[] = [];
  public environmentsBuffer: Environment[] = [];
  public PageBufferSize = 20;
  public numberOfItemsFromEndBeforeFetchingMore = 10;
  public loading = false;
  public currentPage = 1;
  public total: number;
  public saving = false;
  public confirming = false;

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) {
    this.updateNodeForm = this.fb.group({
      environment: [null, [Validators.required]]
    });
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    // load node details
    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('org-id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('name'), filter(identity))
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([server_id, org_id, name]: string[]) => {
      this.serverId = server_id;
      this.orgId = org_id;
      this.name = name;
      this.store.dispatch(new GetNode({
        server_id: server_id, org_id: org_id, name: name
      }));
    });

    this.store.select(infraNodeFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed)
    ).subscribe(node => {
      this.InfraNode = node;
      this.nodeTags = node.tags;
      this.environmentName = node.environment;
      this.tags = this.nodeTags;
      this.nodeDetailsLoading = false;
      // set default value of node environment
      this.updateNodeForm.controls.environment.setValue(this.environmentName);
    });

    // show default list of environments
    this.loadEnvironmentsList();

    combineLatest([
      this.store.select(updateEnvStatus),
      this.store.select(nodeEnvironment)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(([state]) => this.saving && !pending(state))
    ).subscribe(([getEnvSt, envStatus]) => {
      if (getEnvSt === EntityStatus.loadingSuccess && !isNil(envStatus)) {
        this.environmentName = envStatus;
        this.updateNodeForm.markAsPristine();
        this.saving = false;
        this.closeConfirmationBox();
      }
    });

    combineLatest([
      this.store.select(updateTagsStatus),
      this.store.select(nodeTags)
    ]).pipe(
        takeUntil(this.isDestroyed)
      ).subscribe(([getTagsSt, TagsState]) => {
      if (getTagsSt === EntityStatus.loadingSuccess && !isNil(TagsState)) {
        this.nodeTags = TagsState;
        this.tags = this.nodeTags;
        this.updatingTags = false;
      } else if (getTagsSt === EntityStatus.loadingFailure) {
        this.nodeTags = this.tags;
        this.updatingTags = false;
      }
    });
  }

  // update tags
  updateTags(action: string, tags: string[]) {
    this.updatingTags = true;
    const updatedNode = {
      org_id: this.orgId,
      server_id: this.serverId,
      name: this.name,
      action: action,
      tags: tags
    };
    this.store.dispatch(new UpdateNodeTags({node: updatedNode}));
  }

  addTags() {
    if (this.inputTxt !== '') {
      this.tags = this.tags.concat(this.inputTxt.replace(/^[,\s]+|[,\s]+$/g, '')
        .replace(/,[,\s]*,/g, ',').split(',').map(item => item.trim()));
      this.inputTxt = '';
      this.updateTags('add', this.tags);
    }
  }

  removeTag(tag: string) {
    this.tags.forEach((element, index) => {
      if (element === tag) {
        this.tags.splice(index, 1);
      }
    });

    this.removeTags.push(tag);
    this.updateTags('delete', this.removeTags);
  }

  // load list of environments
  onScrollToEnd() {
    this.fetchMore();
  }

  onScroll({ end }) {
    if (this.loading || this.total <= this.environmentsBuffer.length) {
      return;
    }

    if (end + this.numberOfItemsFromEndBeforeFetchingMore >= this.environmentsBuffer.length) {
      this.fetchMore();
    }
  }

  private fetchMore() {
    this.loading = true;
    this.currentPage += 1;
    this.loadEnvironmentsList();

    // using timeout here to simulate backend API delay
    setTimeout(() => {
      this.loading = false;
    }, 200);
  }

  private loadEnvironmentsList(): void {
    this.loading = true;
    const payload = {
      environmentName: '',
      page: this.currentPage,
      per_page: this.PageBufferSize,
      server_id: this.serverId,
      org_id: this.orgId
    };

    this.store.dispatch(new GetEnvironments(payload));

    // load environments list
    combineLatest([
      this.store.select(getAllStatus),
      this.store.select(environmentList)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([getEnvironmentsSt, EnvironmentsState]) => {
      if (getEnvironmentsSt === EntityStatus.loadingSuccess && !isNil(EnvironmentsState)) {
        this.environmentListState = EnvironmentsState;
        this.environments = EnvironmentsState?.items;
        this.environmentsBuffer.push(...this.environments);
        const result = this.removeDuplicateEnv();
        this.environmentsBuffer = result;
        this.total = this.environmentListState?.total;
        this.loading = false;
      } else if (getEnvironmentsSt === EntityStatus.loadingFailure) {
        this.loading = false;
      }
    });
    console.log(this.environmentsBuffer.length);
  }

  private removeDuplicateEnv(): Environment[] {
    const setObj = new Set(); // create key value pair from array of array
    const result = this.environmentsBuffer.reduce((acc, item) => {
      if (!setObj.has(item.name)) {
        setObj.add(item.name);
        acc.push(item);
      }
      return acc;
    }, []); // converting back to array from map-object
    return result;
  }

  selectChangeHandler(env: { name: string }) {
    if (this.environmentName !== env.name) {
      this.confirming = true;
    }
  }

  closeConfirmationBox() {
    this.updateNodeForm.controls.environment.setValue(this.environmentName);
    this.confirming = false;
  }

  saveEnvironment() {
    this.saving = true;
    const updatedNode = {
      org_id: this.orgId,
      server_id: this.serverId,
      name: this.name,
      environment: this.updateNodeForm.controls.environment.value.trim()
    };
    this.store.dispatch(new UpdateNodeEnvironment({node: updatedNode}));
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
