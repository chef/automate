export interface DailyCheckInCountCollection {
  buckets: DailyCheckInCount[];
}

export interface DailyCheckInCount {
  start: Date;
  end: Date;
  checkInCount: number;
  total: number;
}
