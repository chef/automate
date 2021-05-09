import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { combineLatest, Subject } from 'rxjs';
import { isNil } from 'lodash/fp';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams } from 'app/route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity } from 'lodash/fp';
import {
  infraNodeFromRoute,
  updateEnvStatus,
  updateTagsStatus,
  nodeTags
} from 'app/entities/infra-nodes/infra-nodes.selectors';
import { GetNode, UpdateNodeEnvironment, UpdateNodeTags } from 'app/entities/infra-nodes/infra-nodes.actions';
import {
  InfraNode
} from 'app/entities/infra-nodes/infra-nodes.model';

import { GetEnvironments } from 'app/entities/environments/environment.action';
import { getAllStatus, environmentList } from 'app/entities/environments/environment.selectors';
import { Environment } from 'app/entities/environments/environment.model';
import { EntityStatus, pending } from 'app/entities/entities';

export type InfraNodeTabName = 'details';

@Component({
  selector: 'app-infra-node-details',
  templateUrl: './infra-node-details.component.html',
  styleUrls: ['./infra-node-details.component.scss']
})

export class InfraNodeDetailsComponent implements OnInit, OnDestroy {
  public conflictError = false;
  public node: InfraNode;
  public tabValue: InfraNodeTabName = 'details';
  public url: string;
  public serverId: string;
  public orgId: string;
  public name: string;
  public nodeDetailsLoading = true;
  public updateNodeForm: FormGroup;
  public environmentsAvailable: string[] = ['_default', 'production environemnt', 'default environemnt']
  private isDestroyed = new Subject<boolean>();


  public environments: Environment[] = [];
  public environmentsBuffer: Environment[] = [];
  public per_page = 20;
  public current_page = 0;
  public loading: boolean = false;
  public environmentListState: { items: Environment[], total: number };
  public total: number;

  public saving: boolean = false;
  public confirming: boolean = false;


  public tags: string[];
  public nodeTags: string[];
  public inputTxt = '';
  public updatingTags = false;

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { 
    this.updateNodeForm = this.fb.group({
      environment: [this.node?.environment, [Validators.required]],
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
      this.node = node;
      this.nodeTags = node.tags;
      this.tags = this.nodeTags;
      this.nodeDetailsLoading = false;
    });

    // load environments list
    combineLatest([
      this.store.select(getAllStatus),
      this.store.select(environmentList)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([getEnvironmentsSt, EnvironmentsState]) => {
      if (getEnvironmentsSt === EntityStatus.loadingSuccess && !isNil(EnvironmentsState)) {
        this.environmentListState = EnvironmentsState;
        this.environments = EnvironmentsState?.items;
        this.environmentsBuffer = this.environments;
        this.total = this.environmentListState?.total;
        this.nodeDetailsLoading = false;
      } else if (getEnvironmentsSt === EntityStatus.loadingFailure) {
        this.nodeDetailsLoading = false;
      }
    });

    // update node environment
    this.store.select(updateEnvStatus).pipe(
      takeUntil(this.isDestroyed),
      filter(state => this.saving && !pending(state)))
    .subscribe((state) => {
      this.saving = false;
      this.saving = (state === EntityStatus.loadingSuccess);
      if (this.saving) {
        this.updateNodeForm.markAsPristine();
      }
      this.closeConfirmationBox();
    });

    // update node tags
    combineLatest([
      this.store.select(updateTagsStatus),
      this.store.select(nodeTags)
    ]).pipe(takeUntil(this.isDestroyed)).subscribe(([getTagsSt, TagsState]) => {
      if (getTagsSt === EntityStatus.loadingSuccess && !isNil(TagsState)) {
        this.nodeTags = TagsState;
        this.tags = this.nodeTags;
        this.updatingTags = false;
      } else if (getTagsSt === EntityStatus.loadingFailure){
        this.nodeTags = this.tags;
        this.updatingTags = false;
      }
    });
  }

  getEnvironmentData() {
    const payload = {
      environmentName: '',
      page: this.current_page,
      per_page: this.per_page,
      server_id: this.serverId,
      org_id: this.orgId
    };

    this.store.dispatch(new GetEnvironments(payload));
  }


  selectChangeHandler(env: string) {
    console.log(env);
    if (this.node.environment !== env) {
      this.confirming = true;
    }
  }

  closeConfirmationBox() {
    this.confirming = false;
  }

  saveEnvironment() {
    this.saving = true;
    const updatedNode = {
      org_id: this.orgId,
      server_id: this.serverId,
      name: this.node.name,
      environment: this.updateNodeForm.controls.environment.value.trim()
    };
    this.store.dispatch(new UpdateNodeEnvironment({node: updatedNode}));
  }


  // add tags
  addTags() {    
    if(this.inputTxt != ''){
      this.updatingTags = true
      this.tags.push(this.inputTxt);
      this.inputTxt = '';
      const updatedNode = {
        org_id: this.orgId,
        server_id: this.serverId,
        name: this.node.name,
        action: 'add',
        tags: this.tags
      };
      this.store.dispatch(new UpdateNodeTags({node: updatedNode}));
    }
  }

  removeTag(tag: string){
    this.tags.forEach((element,index)=>{
      if (element == tag) this.tags.splice(index,1);
   });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
