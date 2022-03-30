import { ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { WarningBannerComponent } from './warning-banner.component';
import { AppConfigService } from 'app/services/app-config/app-config.service';


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
      declarations: [WarningBannerComponent],
      imports: [HttpClientTestingModule],
      providers: [
        {provide: AppConfigService, useClass: MockAppConfigService}
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(WarningBannerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeDefined();
  });

  it('should load variables on initialization', () => {
    expect(component.bannerMessage).toBe(Configs.Message);
    expect(component.bannerBackgroundColor).toBe(`#${Configs.BackgroundColor}`);
    expect(component.bannerTextColor).toBe(`#${Configs.TextColor}`);
  });

});
