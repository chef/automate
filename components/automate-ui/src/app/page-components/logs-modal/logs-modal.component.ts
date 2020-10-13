import { Component, Input, OnChanges, SimpleChange } from '@angular/core';
import { NodeDetailsService } from '../../services/node-details/node-details.service';
import { NodeRun, Resource } from '../../types/types';
import { saveAs } from 'file-saver';
import { DateTime } from 'app/helpers/datetime/datetime';

@Component({
  selector: 'app-logs-modal',
  templateUrl: './logs-modal.component.html',
  styleUrls: ['./logs-modal.component.scss']
})

export class LogsModalComponent implements OnChanges {
  @Input() nodeRun: NodeRun;
  @Input() isVisible = false;
  @Input() resourceId?: string;

  hideBacktrace = true;
  ChefHoursMins = DateTime.CHEF_HOURS_MINS;
  public resourceObj: Resource;
  public resourceIndex: number;

  constructor(private eventService: NodeDetailsService) {
  }

  ngOnChanges(changes: { [propertyName: string]: SimpleChange }) {
    if (changes['nodeRun']) {
      this.nodeRun = changes['nodeRun'].currentValue;
    }
    if (changes['isVisible']) {
      this.isVisible = changes['isVisible'].currentValue;
    }
  }

  errorSections() {
    const sections = [];
    if (this.resourceId) {
      this.resourceObj = this.nodeRun.resources.find(obj => obj.id === this.resourceId);
      if (this.resourceObj
        && this.resourceObj.error.description
        && this.resourceObj.error.description.sections) {
          this.resourceObj.error.description.sections.forEach((section) => {
            Object.keys(section).forEach((key) => {
              sections.push({
                'heading': key,
                'text': section[key]
              });
            });
          });
      }
    } else {
      if (this.nodeRun
        && this.nodeRun.error.description
        && this.nodeRun.error.description.sections
      ) {
        this.nodeRun.error.description.sections.forEach((section) => {
          Object.keys(section).forEach((key) => {
            sections.push({
              'heading': key,
              'text': section[key]
            });
          });
        });
      }
    }

    return Object.keys(sections).length === 0 ? undefined : sections;
  }

  formatBacktrace() {
    if (this.resourceId) {
      if (this.resourceObj && this.resourceObj.error && this.resourceObj.error.backtrace) {
        return this.resourceObj.error.backtrace.join('\n');
      }
    } else {
      if (this.nodeRun && this.nodeRun.error && this.nodeRun.error.backtrace) {
        return this.nodeRun.error.backtrace.join('\n');
      }
    }
  }

  closeModal() {
    this.eventService.showModal(false);
  }

  download() {
    // build up an array of the sections and their content
    const sections = [];

    if (this.errorSections()) {
      this.errorSections().forEach((section) => {
        let sectionText = '';
        sectionText += section['heading'] + '\n';
        sectionText += '----------------------------------------------------\n';
        sectionText += section['text'].trim();

        sections.push(sectionText);
      });
    }

    // generate content of logfile
    const template = `
      Error Log & Stacktrace

      ${this.nodeRun.error.message}


      ${sections.join('\n\n\n')}


      Backtrace
      ----------------------------------------------------
      ${this.nodeRun.error.backtrace.join('\n')}
      `;

      const blob = new Blob([template], { type: 'text/plain;charset=utf-8' });
      saveAs(blob, this.nodeRun.nodeName + '_error.txt');
  }
}
