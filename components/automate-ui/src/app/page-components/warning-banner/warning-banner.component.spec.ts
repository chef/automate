import { ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { WarningBannerComponent } from './warning-banner.component';
import { AppConfigService } from 'app/services/app-config/app-config.service';
import { MockComponent } from 'ng2-mock-component';


enum Configs {
  Show = 'true',
  Message = 'this is a test message',
  BackgroundColor = '3864f2',
  TextColor = 'FFF'
}

class MockAppConfigService {
  loadAppConfig() {
    return {
      show: Configs.Show,
      message: Configs.Message,
      background_color: Configs.BackgroundColor,
      text_color: Configs.TextColor
    };
  }

  get bannerMessage() {
    return Configs.Message;
  }

  get bannerBackgroundColor() {
    return `#${Configs.BackgroundColor}`;
  }

  get bannerTextColor() {
    return `#${Configs.TextColor}`;
  }
}

describe('WarningBannerComponent', () => {
  let component: WarningBannerComponent;
  let fixture: ComponentFixture<WarningBannerComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        WarningBannerComponent,
        MockComponent({ selector: 'chef-icon' })
      ],
      imports: [HttpClientTestingModule],
      providers: [
        {provide: AppConfigService, useClass: MockAppConfigService}
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(WarningBannerComponent);
    component = fixture.componentInstance;
    component.showManualUpgradeContent = false;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeDefined();
  });

  it('should have variables on initialization', () => {
    expect(component.bannerMessage).toBe(Configs.Message);
    expect(component.bannerBackgroundColor).toBe(`#${Configs.BackgroundColor}`);
    expect(component.bannerTextColor).toBe(`#${Configs.TextColor}`);
  });

});
