import { Component, Input, Output, EventEmitter } from '@angular/core';

@Component({
  selector: 'app-empty-state',
  templateUrl: './empty-state.component.html',
  styleUrls: ['./empty-state.component.scss']
})

export class EmptyStateComponent {
  @Input() moduleTitle: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  resetKeyTabRedirection() {
    this.resetKeyRedirection.emit(true);
  }
}
