import { Component, OnInit, Output, EventEmitter } from '@angular/core';

@Component({
  standalone: false,
  selector: 'app-manual-upgrade-banner',
  templateUrl: './manual-upgrade-banner.component.html',
  styleUrls: ['./manual-upgrade-banner.component.scss']
})
export class ManualUpgradeBannerComponent implements OnInit {
    @Output() close = new EventEmitter();
    constructor() { }

    forPipelinePass = false;
    ngOnInit(): void {
      this.forPipelinePass = true;
    }
    closeEvent() {
        this.close.emit();
    }
}
