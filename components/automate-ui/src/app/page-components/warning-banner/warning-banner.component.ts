import { Component, OnInit, HostBinding, Output, EventEmitter } from '@angular/core';
import { AppConfigService } from 'app/services/app-config/app-config.service';

@Component({
  selector: 'app-warning-banner',
  templateUrl: './warning-banner.component.html',
  styleUrls: ['./warning-banner.component.scss']
})
export class WarningBannerComponent implements OnInit {
  bannerMessage: string;
  bannerTextColor: string;
  showBanner = false;
  @Output() close = new EventEmitter();
  @HostBinding('style.backgroundColor') bannerBackgroundColor: string;

  constructor(private appConfigService: AppConfigService) { }

  ngOnInit() {
      this.bannerMessage = this.appConfigService.bannerMessage;
      this.bannerBackgroundColor = this.appConfigService.bannerBackgroundColor;
      this.bannerTextColor = this.appConfigService.bannerTextColor;
      this.showBanner = this.appConfigService.showBanner;
  }
}