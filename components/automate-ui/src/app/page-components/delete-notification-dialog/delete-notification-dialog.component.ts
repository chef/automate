import { Component } from '@angular/core';
import { MatDialogRef } from '@angular/material';

@Component({
  selector: 'app-delete-notification-dialog',
  templateUrl: './delete-notification-dialog.component.html',
  styleUrls: ['./delete-notification-dialog.component.scss']
})
export class DeleteNotificationDialogComponent {

  constructor(public dialogRef: MatDialogRef<DeleteNotificationDialogComponent>) {}

}
