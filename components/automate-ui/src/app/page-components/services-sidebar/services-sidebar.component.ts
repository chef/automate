import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { Store  } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Observable } from 'rxjs';
import { serviceGroupState } from '../../entities/service-groups/service-groups.selector';
import { createSelector } from '@ngrx/store';
import { Service } from '../../entities/service-groups/service-groups.model';

@Component({
  selector: 'app-services-sidebar',
  templateUrl: './services-sidebar.component.html',
  styleUrls: ['./services-sidebar.component.scss']
})

export class ServicesSidebarComponent implements OnInit {
  @Input() visible: boolean;
  @Output() closeServicesSidebarEvent: EventEmitter<any> = new EventEmitter();

  public services$: Observable<Service[]>;
  public serviceGroupName$: Observable<string>;

  constructor(private store: Store<NgrxStateAtom>) { }

  ngOnInit() {
    this.services$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.servicesList));

    this.serviceGroupName$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.selectedServiceGroupName));
  }

  public closeServicesSidebar() {
    this.closeServicesSidebarEvent.emit(null);
  }

  // healthCheckStatus returns the formated health_check status from the provided service
  // TODO: @afiune here is where we can inject an error message from the health check
  public healthCheckStatus(service: Service): string {
    switch (service.health_check) {
      case 'OK':
        return 'Ok';
      case 'CRITICAL':
        return 'Critical';
      case 'WARNING':
        return 'Warning';
      case 'UNKNOWN':
        return 'Unknown';
      default:
        return service.health_check;
    }
  }
}
