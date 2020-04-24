import { Component, Input, HostBinding, Output, EventEmitter, AfterViewInit } from '@angular/core';
import { trigger, transition, style, animate } from '@angular/animations';

import { Notification, Type } from 'app/entities/notifications/notification.model';


const inOutAnim = trigger('inOutAnim', [
  transition(':enter', [
    style({ opacity: 0 }),
    animate('1000ms ease-out', style({ opacity: 1 }))
  ]),
  transition(':leave', [
    style({ opacity: 1 }),
    animate('1000ms', style({ opacity: 0 }))
  ])
]);

@Component({
  selector: 'app-chef-notification',
  templateUrl: './notification.component.html',
  styleUrls: ['./notification.component.scss'],
  animations: [inOutAnim]
})
export class ChefNotificationComponent implements AfterViewInit {

  @Input() notification: Notification;
  @Input() type: Type;
  @Input() timeout: number;
  @Output() dismissed = new EventEmitter();

  @HostBinding('@inOutAnim') inOutAnim;
  @HostBinding('class') get theType() {return this.type; }

  timeOutRef;

  constructor() {}

  ngAfterViewInit() {
    this.timeOutRef = setTimeout(this.handleClose, this.timeout * 1000);
  }

  handleClose = () => {
    clearTimeout(this.timeOutRef);
    this.dismissed.emit();
  }
}
