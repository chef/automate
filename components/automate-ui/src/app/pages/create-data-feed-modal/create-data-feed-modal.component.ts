import { Component, EventEmitter, Input, Output, OnInit } from '@angular/core';
import { FormGroup } from '@angular/forms';

@Component({
  selector: 'app-create-data-feed-modal',
  templateUrl: './create-data-feed-modal.component.html',
  styleUrls: ['./create-data-feed-modal.component.scss']
})
export class CreateDataFeedModalComponent implements OnInit {
  @Input() visible = false;
  @Input() creating = false;
  @Input() sending = false;
  @Input() conflictErrorEvent: EventEmitter<boolean>;
  @Output() close = new EventEmitter();
  @Output() createClicked = new EventEmitter();
  // @Output() sendTestClicked = new EventEmitter();
  @Input() createForm: FormGroup;

  public conflictError = false;

  ngOnInit() {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
    });
  }

  public handleInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  closeEvent(): void {
    this.close.emit();
  }

  createDataFeed(): void {
    this.createClicked.emit();
  }

  // sendTest($event: Event) {
  //   $event.preventDefault();
  //   this.sendTestClicked.emit();
  // }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}

