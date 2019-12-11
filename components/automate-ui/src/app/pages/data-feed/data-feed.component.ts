import { of as observableOf,  Observable } from 'rxjs';

import { map, filter } from 'rxjs/operators';
import { Component, OnInit } from '@angular/core';
import { MatDialog, MatSnackBar } from '@angular/material';

import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { SortDirection } from '../../types/types';
import { DatafeedService } from '../../services/data-feed/data-feed.service';
import { TelemetryService } from '../../services/telemetry/telemetry.service';
import {
  DeleteDatafeedDialogComponent
} from 'app/page-components/delete-data-feed-dialog/delete-data-feed-dialog.component';
import { Destination } from './destination';

export interface FieldDirection {
  name: SortDirection;
  url: SortDirection;
}

@Component({
  selector: 'app-data-feed',
  templateUrl: './data-feed.component.html',
  styleUrls: ['./data-feed.component.scss']
})
export class DatafeedComponent implements OnInit {
  destinations$: Observable<Destination[]> = observableOf([]);
  errorLoading = false;
  currentPage = 1;
  pageSize = 10;
  sortField: string;
  sortDir: FieldDirection;
  permissionDenied = false;

  constructor(
    private layoutFacade: LayoutFacadeService,
    private service: DatafeedService,
    public dialog: MatDialog,
    public snackBar: MatSnackBar,
    private telemetryService: TelemetryService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.destinations$ = this.service.fetchDestinations();
    this.destinations$.subscribe(destinations => {
        this.sendCountToTelemetry(destinations);
      },
      error => {
        if (error.status === 403) {
          this.permissionDenied = true;
        } else  {
          this.errorLoading = true;
        }
      }
    );
    this.resetSortDir();
    this.toggleSort('name');
  }

  toggleSort(field: string) {
    if (field === this.sortField) {
      // when sorting is inverted for the currently sorted column
      this.sortDir[field] = this.sortDir[field] === 'asc' ? 'desc' : 'asc';
    } else {
      // when sorting a different column than the currently sorted one
      this.resetSortDir();
    }
    this.sortField = field;
    this.updateSort(field, this.sortDir[field]);
  }
  sortIcon(field: string): string {
    if (field === this.sortField) {
      return 'sort-' + this.sortDir[field];
    } else {
      return 'sort-asc';
    }
  }

  deleteDestination(destination: Destination) {
    const dialogRef2 = this.dialog.open(DeleteDatafeedDialogComponent);
    dialogRef2.afterClosed().pipe(
      filter((result: any) => result === 'delete' ))
      .subscribe(_result => {
        this.service.deleteDestination(destination).subscribe(_res => {
          this.snackBarMessage(`Destination '${destination.name}' was deleted.`);
          this.refreshDestinations();
        }, err => {
          const body = err;
          this.snackBarMessage(`Could not delete destination '${destination.name}': ${body}`);
        });
      });
  }

  snackBarMessage(message) {
    this.snackBar.open(message, '', { duration: 6000 } );
  }

  private updateSort(field: string, direction: string) {
    this.destinations$ = this.destinations$.pipe(map(destinations => {
      let sortedDestinations: Destination[] = [];
      if (field === 'name') {
        sortedDestinations = destinations.sort((r1: Destination, r2: Destination) => {
          return r1.name.localeCompare(r2.name);
        });
      }
      if (direction === 'asc') {
        return sortedDestinations;
      } else {
        return sortedDestinations.reverse();
      }
    }));
  }

  private resetSortDir(): void {
    this.sortDir = {
      name: 'asc',
      url: 'asc'
    };
  }

  private sendCountToTelemetry(destinations: Destination[]) {
    this.telemetryService.track('datafeedDestinationsCount', destinations.length);
  }

  refreshDestinations() {
    this.destinations$ = this.service.fetchDestinations();
    this.destinations$.subscribe(destinations => {
      this.sendCountToTelemetry(destinations);
      this.updateSort(this.sortField, this.sortDir[this.sortField]);
    });
  }

}
