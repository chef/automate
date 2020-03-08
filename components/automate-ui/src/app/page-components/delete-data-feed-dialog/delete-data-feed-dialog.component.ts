import { Component } from '@angular/core';
import { MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'app-delete-datafeed-dialog',
  templateUrl: './delete-data-feed-dialog.component.html',
  styleUrls: ['./delete-data-feed-dialog.component.scss']
})
export class DeleteDatafeedDialogComponent {

  constructor(public dialogRef: MatDialogRef<DeleteDatafeedDialogComponent>) {}

}
