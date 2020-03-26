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
