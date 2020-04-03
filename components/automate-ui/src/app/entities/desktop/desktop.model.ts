export interface DailyCheckInCountCollection {
  buckets: DailyCheckInCount[];
  updated: Date;
}

export interface DailyCheckInCount {
  start: Date;
  end: Date;
  checkInCount: number;
  total: number;
}

export interface DayPercentage {
  daysAgo: number;
  percentage: number;
}

export interface TopErrorsCollection {
  items: TopErrorsItem[];
  updated: Date;
}

export interface TopErrorsItem {
  count: number;
  type: string;
  message: string;
}
