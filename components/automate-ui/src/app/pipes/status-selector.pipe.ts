import { Pipe, PipeTransform } from '@angular/core';
import {
  SelectedStatus
} from '../types/types';

@Pipe({name: 'statusSelector'})
export class StatusSelectorPipe implements PipeTransform {
    constructor() {}

    transform(value, args: SelectedStatus): any {
      let filter = 'all';
      // parse SelectedStatus and map it to status string
      switch (args) {
        case SelectedStatus.Success:
          filter = 'success';
          break;
        case SelectedStatus.Failure:
          filter = 'failure';
          break;
        case SelectedStatus.Failed:
          filter = 'failed';
          break;
        case SelectedStatus.Passed:
          filter = 'passed';
          break;
        case SelectedStatus.Skipped:
          filter = 'skipped';
          break;
        default:
          filter = 'all';
      }
      // filter elements
      if (value) {
        return value.filter((history) => (
          (history.status === filter) || (filter === 'all')
        )
      );
    }
  }
}

