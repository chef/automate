export interface DailyCheckInCountCollection {
  buckets: DailyCheckInCount[];
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
}

export interface TopErrorsItem {
  count: number;
  type: string;
  message: string;
}
