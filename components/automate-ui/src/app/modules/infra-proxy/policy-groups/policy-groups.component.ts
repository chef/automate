import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Subject } from 'rxjs';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';

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

  public authFailure = false;
  public searchValue = '';
  public searchFlag = false;
  public searchArr: [];

  constructor(
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
  }

    ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

}
