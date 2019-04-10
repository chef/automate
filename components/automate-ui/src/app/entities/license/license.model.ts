import * as moment from 'moment';

// Example JSON returned from gateway:
// {
//   "license_id": "a953c5bb-82a5-41be-b7dd-a5de1ea53ada",
//   "configured_at": "2018-04-26T21:14:34.000Z",
//   "licensed_period": {
//     "start": "2018-02-14T00:00:00.000Z",
//     "end": "2020-02-14T23:59:59.000Z"
//   },
//   "customer_name": "Chef Dev"
// }

export interface LicenseStatus {

  // NB: Keep in sync with automate-gateway/api/license/license.proto:GetStatusResp.
  license_id: string; // uuid
  configured_at: string; // timestamp
  customer_name: string;
  licensed_period: {
    start: string; // timestamp
    end: string; // timestamp
  };
}

export interface ApplyLicensePayload {

  // NB: Keep in sync with automate-gateway/api/license/license.proto:ApplyLicenseReq.
  license: string;
}

export interface ApplyLicenseResponse {

  // NB: Keep in sync with automate-gateway/api/license/license.proto:ApplyLicenseResp.
  status: LicenseStatus;
}

export interface RequestLicensePayload {

  // NB: Keep in sync with automate-gateway/api/license/license.proto:RequestLicenseReq.
  first_name: string;
  last_name: string;
  email: string;
  gdpr_agree: boolean;
}

export interface RequestLicenseResponse {

  // NB: Keep in sync with automate-gateway/api/license/license.proto:RequestLicenseResp.
  license: string;
  status: LicenseStatus;
}

export function parsedExpirationDate(license: LicenseStatus): string {
  // Use short form, localized date (momentjs.com/docs/#localized-formats) to render date.
  // At the time of writing, Angular's date pipe is *not* locale-aware.
  return moment(license.licensed_period.end).format('l');
}
