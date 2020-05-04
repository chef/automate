import { Component, Input, HostBinding, Output, EventEmitter, AfterViewInit } from '@angular/core';
import { trigger, transition, style, animate } from '@angular/animations';
import { Type } from 'app/entities/notifications/notification.model';

const slideDownAnim = trigger('slideDownAnim', [
  transition(':enter', [
    style({
      opacity: 0,
      transform: 'translateY(-100%) scale(0.98)'
    }),
    animate('300ms ease-out',
      style({
        opacity: 1,
        transform: 'translateY(0%) scale(1)'
      }))
  ]),
  transition(':leave', [
    style({ opacity: 1 }),
    animate('195ms ease-in', style({ opacity: 0 }))
  ])
]);

@Component({
  selector: 'app-notification',
  templateUrl: './notification.component.html',
  styleUrls: ['./notification.component.scss'],
  animations: [ slideDownAnim ]
})
export class ChefNotificationComponent implements AfterViewInit {

  @Input() type: Type = Type.info;
  @Input() timeout = 8;
  @Output() dismissed = new EventEmitter();

  @HostBinding('@slideDownAnim') slideDownAnim;
  @HostBinding('class') get theType() { return this.type; }

  private timeOutRef: ReturnType<typeof setTimeout>;

  ngAfterViewInit() {
    const isInfinite = this.timeout < 1;
    if ( isInfinite ) { return; }

    this.timeOutRef = setTimeout(this.handleClose, this.timeout * 1000);
  }

  handleClose = (): void => {
    clearTimeout(this.timeOutRef);
    this.dismissed.emit();
  }
}
