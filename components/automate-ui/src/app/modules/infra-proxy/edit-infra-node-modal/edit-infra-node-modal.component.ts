import {
  Component,
  EventEmitter,
  Input,
  OnInit,
  OnDestroy,
  Output,
  OnChanges
} from '@angular/core';
import { Subject } from 'rxjs';
import { filter, takeUntil } from 'rxjs/operators';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import {
  updateStatus
} from 'app/entities/infra-nodes/infra-nodes.selectors';
import { EntityStatus, pending } from 'app/entities/entities';
import { InfraNode } from 'app/entities/infra-nodes/infra-nodes.model';
import { ListItem } from '../select-box/src/lib/list-item.domain';
import { UpdateNode } from 'app/entities/infra-nodes/infra-nodes.actions';
import { Utilities } from 'app/helpers/utilities/utilities';
import { AvailableType } from '../infra-roles/infra-roles.component';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-edit-infra-node-modal',
  templateUrl: './edit-infra-node-modal.component.html',
  styleUrls: ['./edit-infra-node-modal.component.scss']
})
export class EditInfraNodeModalComponent implements OnInit, OnDestroy, OnChanges {
  @Input() label: string;
  @Input() openEvent: EventEmitter<boolean>;
  @Input() orgId: string;
  @Input() availableType: AvailableType[] = [];
  @Input() node: InfraNode;
  @Input() serverId: string;
  @Input() selected: ListItem[] = [];
  @Input() runlistError: boolean;
  @Output() runlistUpdated: EventEmitter<void> =   new EventEmitter();
  @Output() closeRunlist = new EventEmitter();

  public creating = false;
  public conflictError = false;
  public isLoading = true;
  public isRunlist = false;
  public showbutton = false;
  public updateSuccessful = false;
  public updateInProgress = false;
  public visible = false;
  public server: string;
  public org: string;
  public selectedRunLists: string[] = [];
  public currentRunList: ListItem[] = [];
  public current_Page = 0;
  public availablelist: AvailableType[] = [];
  public close = new EventEmitter();
  public conflictErrorEvent = new EventEmitter<boolean>();
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private telemetryService: TelemetryService
  ) {

  }

  ngOnInit(): void {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
    .subscribe(() => {
      this.conflictError = false;
      this.showbutton = true;
      this.visible = true;
      this.server = this.serverId;
      this.org = this.orgId;
      this.currentRunList = [];
      this.current_Page = 0;
      this.selected.forEach((element) => {
        element.selected = false;
        this.currentRunList.push(element);
      });
      this.availablelist = [];
      this.availableType.forEach((element) => {
        this.availablelist.push(element);
      });
    });
    this.store.select(updateStatus).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([status]) => {
      this.isLoading = status === EntityStatus.loading;
    });
    this.store.select(updateStatus).pipe(
      takeUntil(this.isDestroyed),
      filter(state => this.updateInProgress && !pending(state)))
      .subscribe((state) => {
        this.updateInProgress = false;
        this.updateSuccessful = (state === EntityStatus.loadingSuccess);
        if (this.updateSuccessful) {
          this.runlistUpdated.emit();
          this.closeEditModal();
        }
      });
  }

  ngOnChanges() {
    this.currentRunList = [];
    this.selected.forEach((element) => {
      element.selected = false;
      this.currentRunList.push(element);
    });
    this.availablelist = [];
    this.availableType.forEach((element) => {
      this.availablelist.push(element);
    });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  closeEditModal(): void {
    this.closeRunlist.emit();
    this.resetEditModal();
    this.visible = false;
  }

  dragDropHandler(runlists: ListItem[]) {
    this.selectedRunLists = [];
    runlists.forEach(runlist => {
      this.selectedRunLists.push(`${runlist.type}[${runlist.value}]`);
    });
    this.isRunlist = true;
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!Utilities.isNavigationKey(event)) {
      this.conflictError = false;
    }
  }

  updateNode(): void {
    this.updateSuccessful = false;
    this.updateInProgress = true;

    let node: InfraNode = {
      server_id: this.serverId,
      org_id: this.orgId,
      name: this.node.name,
      environment: this.node.environment,
      policy_name: this.node.policy_name,
      policy_group: this.node.policy_group,
      automatic_attributes: JSON.parse(this.node.automatic_attributes),
      normal_attributes: JSON.parse(this.node.normal_attributes),
      // these are filled in by the switch below
      run_list: [],
      default_attributes: JSON.parse(this.node.default_attributes),
      override_attributes: JSON.parse(this.node.override_attributes)
    };

    if (this.label === 'Run List') {
      node = { ...node,
        run_list: this.isRunlist ? this.selectedRunLists : this.node.run_list
      };
    }

    this.updatingData(node);
    this.telemetryService.track('InfraServer_Nodes_EditRunList');
  }

  private resetEditModal(): void {
    this.creating = false;
    this.selectedRunLists = [];
    this.showbutton = false;
    this.isRunlist = false;
    this.conflictErrorEvent.emit(false);
  }

  private updatingData(node: InfraNode) {
    this.store.dispatch(new UpdateNode(node));
  }
}
