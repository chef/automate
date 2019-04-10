import { UpgradeAdapter } from '@angular/upgrade';
import { AppModule } from './app.module';

export const upgradeAdapter = new UpgradeAdapter(AppModule);
