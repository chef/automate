import { Injectable } from '@angular/core';

import { MenuItemGroup } from './layout.model'; 

@Injectable({
    providedIn: 'root'
})
export class LayoutSidebarService {
    constructor() {}

    public getComplianceSidebar(): MenuItemGroup[] {
        return [{
            name: 'Compliance',
            items: [
                {
                    name: 'Reports',
                    icon: 'equalizer',
                    route: '/compliance/reports',
                    authorized: {
                        name: 'reports',
                        allOf: [['/compliance/reporting/stats/summary', 'post'], 
                        ['/compliance/reporting/stats/failures', 'post'], 
                        ['/compliance/reporting/stats/trend', 'post']]
                    },
                    visible: true
                },
                {
                    name: 'Scan Jobs',
                    icon: 'wifi_tethering',
                    route: '/compliance/scan-jobs',
                    authorized: {
                        name: 'reports',
                        allOf: [['/compliance/scanner/jobs', 'post'],
                        ['/compliance/scanner/jobs/search', 'post']]
                    },
                    visible: true
                },
                {
                    name: 'Profiles',
                    icon: 'library_books',
                    route: '/compliance/compliance-profiles',
                    authorized: {
                        name: 'profiles',
                        allOf: [['/compliance/profiles/search', 'post']]
                    },
                    visible: true
                }
            ],
            visible: true
        }];
    }
}
