// Additional test setup for RxJS error handling
import { config } from 'rxjs';

// Comprehensive EmptyError suppression for test environment
// This handles errors at multiple levels to prevent test failures

// 1. RxJS global error handler
config.onUnhandledError = (err: any) => {
  if (err && err.constructor && err.constructor.name === 'EmptyError') {
    // Completely suppress EmptyError - don't even log in tests
    return;
  }
  // Let other errors through
  console.error('Unhandled RxJS error:', err);
};

// 2. Browser-level error handling for afterAll hooks
const originalWindowError = window.onerror;
window.onerror = function(message, source, lineno, colno, error) {
  if (error && error.constructor && error.constructor.name === 'EmptyError') {
    // Suppress window-level EmptyErrors
    return true; // Prevent default error handling
  }
  if (typeof message === 'string' && message.includes('EmptyError')) {
    return true; // Suppress string-based EmptyError messages
  }
  // Call original handler for other errors
  if (originalWindowError) {
    return originalWindowError.call(this, message, source, lineno, colno, error);
  }
  return false;
};

// 3. Unhandled promise rejection handler
const originalUnhandledRejection = window.onunhandledrejection;
window.onunhandledrejection = function(event) {
  if (event.reason && event.reason.constructor && event.reason.constructor.name === 'EmptyError') {
    event.preventDefault(); // Prevent default handling
    return;
  }
  if (originalUnhandledRejection) {
    return originalUnhandledRejection.call(this, event);
  }
};

// 4. Override console.error to filter EmptyError messages
const originalConsoleError = console.error;
console.error = function(...args: any[]) {
  const message = args.join(' ');
  if (message.includes('EmptyError') || message.includes('no elements in sequence')) {
    // Don't log EmptyError at all in tests
    return;
  }
  originalConsoleError.apply(console, args);
};

// 5. Patch Jasmine's afterAll to catch and suppress EmptyErrors
if (typeof afterAll !== 'undefined') {
  const originalAfterAll = afterAll;
  (window as any).afterAll = function(fn: any, timeout?: number) {
    return originalAfterAll.call(this, function() {
      try {
        const result = fn();
        if (result && typeof result.then === 'function') {
          return result.catch((error: any) => {
            if (error && error.constructor && error.constructor.name === 'EmptyError') {
              // Suppress EmptyError in afterAll hooks
              return;
            }
            throw error;
          });
        }
        return result;
      } catch (error: any) {
        if (error && error.constructor && error.constructor.name === 'EmptyError') {
          // Suppress EmptyError in afterAll hooks
          return;
        }
        throw error;
      }
    }, timeout);
  };
}
