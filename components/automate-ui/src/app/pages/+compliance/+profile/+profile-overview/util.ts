export interface ProgressData {
  file: string;
  percent: number;
  status: number;
  errors: Array<{}>;
  response: string; // this could be JSON, or <h1>401 No Authorization</h1> etc
}

// Note: to not completely re-vamp the UI Component, this function will not
// change the format too much: if the status != 200, the UI component displays
// it as failed, etc. What it does, however, is change response to a string
// that can be displayed as-is.
export function formatProgressResponse(progress: ProgressData): ProgressData {
  let report;

  // Bad profile uploads no longer return 400 w/ json body:
  if (progress.status === 400) {
    progress.errors = [{ msg: progress.response }];
    return progress;
  }

  try {
    report = JSON.parse(progress.response);
  } catch (e) {
    // huh?
  }

  if (report) {
    progress.response = formatInspecReport(report);
    progress.errors = report.errors;
    return progress;
  }

  switch (progress.status) {
    case 401:
      progress.response = 'Bad authorization';
      break;
    default:
      progress.response = 'The server was not able to process that file';
  }

  return progress;
}

// Note: much more could be done with this information
interface InspecFinding {
  line: number;
  column?: number;
  control_id: string;
  msg: string;
}

interface InspecReport {
  summary: {
    valid: boolean;
    timestamp: string;
    location: string;
    controls: number
  };
  errors?: InspecFinding[];
  warnings?: InspecFinding[];
}

function formatInspecReport(report: InspecReport): string {
  if (!report.summary.valid) {
    return 'Profile file invalid';
  }
  const warnCount = report.warnings ? report.warnings.length : 0;
  const errCount = report.errors ? report.errors.length : 0;
  let err = '';
  let warn = '';

  switch (errCount) {
    case 0: break;
    case 1:
      err = '1 error';
      break;
    default:
      err = `${errCount} errors`;
  }

  switch (warnCount) {
    case 0: break;
    case 1:
      warn = '1 warning';
      break;
    default:
      warn = `${warnCount} warnings`;
  }

  return err + (errCount > 0 && warnCount > 0 ? ', ' : '') + warn;
}
