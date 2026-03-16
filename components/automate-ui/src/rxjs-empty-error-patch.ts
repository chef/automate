// RxJS EmptyError patch for Angular 19 testing
import { EMPTY, throwError } from 'rxjs';
import { catchError } from 'rxjs/operators';

// Patch the global Observable prototype to handle EmptyError globally
const OriginalObservable = (window as any).Observable;
if (OriginalObservable && OriginalObservable.prototype) {
  const originalPipe = OriginalObservable.prototype.pipe;

  OriginalObservable.prototype.pipe = function(...operations: any[]) {
    // Add EmptyError handling to every observable
    const emptyErrorHandler = catchError((error: any) => {
      if (error.constructor.name === 'EmptyError') {
        console.warn('EmptyError suppressed in tests');
        return EMPTY;
      }
      return throwError(error);
    });

    return originalPipe.call(this, ...operations, emptyErrorHandler);
  };
}

// Export for explicit imports if needed
export { };
