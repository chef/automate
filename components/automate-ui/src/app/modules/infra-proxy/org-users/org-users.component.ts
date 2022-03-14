import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Subject } from 'rxjs';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';

@Component({
  selector: 'app-org-users',
  templateUrl: './org-users.component.html',
  styleUrls: ['./org-users.component.scss']
})

export class OrgUsersComponent implements OnInit, OnDestroy {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  //Will variable add type at time of API integration
  public users;
  public usersListLoading: boolean = false;
  public authFailure: boolean = false;
  public loading: boolean = false;
  public current_page:number = 1;
  public per_page:number = 100;
  public total: number;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private layoutFacade: LayoutFacadeService
  ) {}

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

  onPageChange(event: number): void {
    this.current_page = event;
    this.loading = true;
  }

  resetPEMKey() {
    console.log('resetPEM');
  }

  onUpdatePage($event: { pageIndex: number; pageSize: number; }) {
    this.current_page = $event.pageIndex + 1;
    this.per_page = $event.pageSize;
    this.loading = true;
  }
}
