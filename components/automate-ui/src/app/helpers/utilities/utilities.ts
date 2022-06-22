import { Injectable } from '@angular/core';

@Injectable()
export class Utilities {

  static isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab' || event.key === 'Enter';
  }

}

