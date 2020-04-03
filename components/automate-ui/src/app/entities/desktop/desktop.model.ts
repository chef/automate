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

export interface CountedDurationCollection {
  items: CountedDurationItem[];
  updated: Date;
}

export interface CountedDurationItem {
  duration: string;
  count: number;
}
